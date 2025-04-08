# --- Normális eloszlás CDF táblázat sztenderd eloszláshoz ---
norm_table <- list(
  "-4.0"=0.00003,"-3.9"=0.00005,"-3.8"=0.00007,"-3.7"=0.00011,"-3.6"=0.00016,
  "-3.5"=0.00023,"-3.4"=0.00034,"-3.3"=0.00048,"-3.2"=0.00069,"-3.1"=0.00097,
  "-3.0"=0.0013,"-2.9"=0.0019,"-2.8"=0.0026,"-2.7"=0.0035,"-2.6"=0.0047,
  "-2.5"=0.0062,"-2.4"=0.0082,"-2.3"=0.0107,"-2.2"=0.0139,"-2.1"=0.0179,
  "-2.0"=0.0228,"-1.9"=0.0287,"-1.8"=0.0359,"-1.7"=0.0446,"-1.6"=0.0548,
  "-1.5"=0.0668,"-1.4"=0.0808,"-1.3"=0.0968,"-1.2"=0.1151,"-1.1"=0.1357,
  "-1.0"=0.1587,"-0.9"=0.1841,"-0.8"=0.2119,"-0.7"=0.2420,"-0.6"=0.2743,
  "-0.5"=0.3085,"-0.4"=0.3446,"-0.3"=0.3821,"-0.2"=0.4207,"-0.1"=0.4602,
  "0.0"=0.5000,"0.1"=0.5398,"0.2"=0.5793,"0.3"=0.6179,"0.4"=0.6554,
  "0.5"=0.6915,"0.6"=0.7257,"0.7"=0.7580,"0.8"=0.7881,"0.9"=0.8159,
  "1.0"=0.8413,"1.1"=0.8643,"1.2"=0.8849,"1.3"=0.9032,"1.4"=0.9192,
  "1.5"=0.9332,"1.6"=0.9452,"1.7"=0.9554,"1.8"=0.9641,"1.9"=0.9713,
  "2.0"=0.9772,"2.1"=0.9821,"2.2"=0.9861,"2.3"=0.9893,"2.4"=0.9918,
  "2.5"=0.9938,"2.6"=0.9953,"2.7"=0.9965,"2.8"=0.9974,"2.9"=0.9981,
  "3.0"=0.9987,"3.1"=0.9990,"3.2"=0.9993,"3.3"=0.9995,"3.4"=0.9997,
  "3.5"=0.9998,"3.6"=0.99984,"3.7"=0.99989,"3.8"=0.99993,"3.9"=0.99995,
  "4.0"=0.99997
)

# --- Biztonságos egész szám beolvasása inputból ---
read_integer <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    value <- suppressWarnings(as.integer(strsplit(line, "#")[[1]][1]))
    if (!is.na(value)) return(value)
    cat("Hibás érték! Egész számot adj meg.\n")
  }
}

# --- Biztonságos lebegőpontos szám beolvasása inputból ---
read_numeric <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    value <- suppressWarnings(as.numeric(strsplit(line, "#")[[1]][1]))
    if (!is.na(value)) return(value)
    cat("Hibás érték! Számot adj meg.\n")
  }
}

# --- Intervallumok számának bekérése ---
repeat {
  k <- read_integer("Hány részre bontsuk az intervallumot? (pl. 10): ")
  if (k > 2) break
  cat("Hiba: legalább 3 rész kell!\n")
}

# --- Max érték bekérése ---
repeat {
  max_val <- read_numeric("Add meg a két végtelenhez eső legközelebbi értéket (pl. 4): ")
  if (max_val > 0 && max_val <= 4) break
  cat("Hiba: pozitív szám kell (max 4)!\n")
}

# --- Intervallumhatárok ---
step <- (2 * max_val) / (k - 2)
mid_points <- round(seq(-max_val, max_val, by = step), 1)
breaks <- c(-Inf, mid_points, Inf)

cat("Intervallumhatárok:\n")
print(breaks)

# --- Megfigyelt gyakoriságok bekérése ---
observed <- numeric(k)
cat("Add meg a megfigyelt gyakoriságokat az alábbi intervallumokhoz:\n")
for (i in 1:k) {
  repeat {
    prompt <- paste0("  ", i, ": ", breaks[i], " - ", breaks[i + 1], " : ")
    val <- read_integer(prompt)
    if (val >= 0) {
      observed[i] <- val
      break
    }
    cat("Nemnegatív egész számot adj meg!\n")
  }
}

# --- N összesen ---
N <- sum(observed)
cat("\nTeljes megfigyelésszám (N):", N, "\n")

# --- Elméleti valószínűségek ---
probs <- numeric(k)
for (i in 1:k) {
  lower <- formatC(breaks[i], format = "f", digits = 1)
  upper <- formatC(breaks[i + 1], format = "f", digits = 1)

  p_low <- if (breaks[i] < -1000) 0 else norm_table[[lower]]
  p_high <- if (breaks[i + 1] > 1000) 1 else norm_table[[upper]]

  if (is.null(p_low) || is.null(p_high)) {
    stop(paste("Hiba: hiányzó Φ(z) érték a táblában:", lower, "-", upper))
  }

  probs[i] <- p_high - p_low
}

cat("Elméleti valószínűségek:\n")
for (i in 1:k) {
  cat(paste0("P", i, ": ", sprintf("%.4f", probs[i]), "\n"))
}

# --- Khi-négyzet statisztika ---
chisq_stat <- 0
for (i in 1:k) {
  expected <- N * probs[i]
  if (expected > 0) {
    chisq_stat <- chisq_stat + ((observed[i] - expected)^2) / expected
  }
}

# --- Kritikus érték meghatározása ---
df <- k - 1
critical_lookup <- c(
  "1"=3.841,"2"=5.991,"3"=7.815,"4"=9.488,"5"=11.070,"6"=12.592,"7"=14.067,"8"=15.507,"9"=16.919,"10"=18.307,
  "11"=19.675,"12"=21.026,"13"=22.362,"14"=23.685,"15"=24.996,"16"=26.296,"17"=27.587,"18"=28.869,"19"=30.144,"20"=31.410
)

if (as.character(df) %in% names(critical_lookup)) {
  critical_value <- critical_lookup[as.character(df)]
} else {
  repeat {
    critical_value <- read_numeric(paste0("Add meg kézzel az 5%-os kritikus értéket df = ", df, ": "))
    if (critical_value > 0) break
    cat("Hibás érték! Pozitív szám kell.\n")
  }
}

# --- Kiértékelés ---
cat("\n--- Khi-négyzet próba eredmény ---\n")
cat("Statisztika: ", sprintf("%.4f", chisq_stat), "\n")
cat("Szabadságfok:", df, "\n")
cat("Kritikus érték (5%):", critical_value, "\n")

if (chisq_stat > critical_value) {
  cat("→ Elutasítjuk a H₀-t: az eloszlás nem sztenderd normális eloszlást követ.\n")
} else {
  cat("→ Nem utasítjuk el a H₀-t: az eloszlás sztenderd normális eloszlást követ.\n")
}

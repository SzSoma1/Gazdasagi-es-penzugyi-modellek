# --- Tolerancia az úszó pontos összegeknél ---
tolerancia <- 1e-6

# --- Biztonságos egész szám beolvasása stdin-ből ---
read_integer <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    value <- suppressWarnings(as.integer(strsplit(line, "#")[[1]][1]))
    if (!is.na(value) && value > 0) return(value)
    cat("Hibás bevitel! Pozitív egész számot adj meg.\n")
  }
}

# --- Biztonságos valós szám beolvasása stdin-ből ---
read_numeric <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    value <- suppressWarnings(as.numeric(strsplit(line, "#")[[1]][1]))
    if (!is.na(value) && value >= 0 && value <= 1) return(value)
    cat("Hibás bevitel! 0 és 1 közötti számot adj meg.\n")
  }
}

# --- 1. Felhasználótól adatok bekérése ---
eredmeny_lehetosegek <- read_integer("Hány különböző lehetséges dobási eredmény van? (pl. 6): ")
dobasok_szama <- read_integer("Hány dobás történt összesen? (pl. 100): ")

# --- 2. Valószínűségek bekérése ---
repeat {
  valoszinusegek <- numeric(eredmeny_lehetosegek)
  cat("\nAdd meg az egyes események valószínűségeit (összegüknek pontosan 1-nek kell lennie):\n")
  for (i in 1:eredmeny_lehetosegek) {
    prompt <- paste0("  p[", i, "]: ")
    valoszinusegek[i] <- read_numeric(prompt)
  }
  if (abs(sum(valoszinusegek) - 1) < tolerancia) break
  cat("\n⚠️ Hibás bevitel! A valószínűségek összegének pontosan 1-nek kell lennie. Próbáld újra!\n\n")
}

# --- 3. Megfigyelt dobási gyakoriságok bekérése ---
repeat {
  dobas_eredmenyek <- numeric(eredmeny_lehetosegek)
  cat("\nAdd meg az egyes események előfordulásait (összegük legyen ", dobasok_szama, "):\n")
  for (i in 1:eredmeny_lehetosegek) {
    prompt <- paste0("  Esemény[", i, "] előfordulása: ")
    repeat {
      val <- suppressWarnings(as.integer(strsplit(readLines("stdin", n = 1), "#")[[1]][1]))
      if (!is.na(val) && val >= 0) {
        dobas_eredmenyek[i] <- val
        break
      }
      cat("Hibás bevitel! Nemnegatív egész számot adj meg.\n")
    }
  }
  if (sum(dobas_eredmenyek) == dobasok_szama) break
  cat("\n⚠️ Az előfordulások összegének pontosan ", dobasok_szama, " kell lennie. Próbáld újra!\n\n")
}

# --- 4. Khi-négyzet statisztikák számítása ---
# a) Felhasználó által megadott valószínűségekkel
vart_ertekek_felhasznalo <- dobasok_szama * valoszinusegek
khi_negyzet_felhasznalo <- sum((dobas_eredmenyek - vart_ertekek_felhasznalo)^2 / vart_ertekek_felhasznalo)

# b) Egyenletes eloszlással
egyenletes_valoszinuseg <- rep(1 / eredmeny_lehetosegek, eredmeny_lehetosegek)
vart_ertekek_egyenletes <- dobasok_szama * egyenletes_valoszinuseg
khi_negyzet_egyenletes <- sum((dobas_eredmenyek - vart_ertekek_egyenletes)^2 / vart_ertekek_egyenletes)

# --- 5. Kritikus érték kiszámítása Wilson–Hilferty közelítéssel ---
df <- eredmeny_lehetosegek - 1

erf_inv <- function(x) {
  a <- 0.147
  x <- min(x, 1 - 1e-10)
  ln1mx2 <- log(1 - x^2)
  term1 <- 2 / (pi * a) + ln1mx2 / 2
  term2 <- ln1mx2 / a
  return(sign(x) * sqrt(sqrt(term1^2 - term2) - term1))
}

p <- 0.95  # 5%-os szignifikanciaszint
z <- sqrt(2) * erf_inv(2 * p - 1)
kritikus_ertek <- df * (1 - 2 / (9 * df) + z * sqrt(2 / (9 * df)))^3

# --- 6. Eredmények kiírása ---
cat("\n📌 Khi-négyzet kritikus érték (5%):", round(kritikus_ertek, 4), "\n")

cat("\n🔵 Khi-négyzet érték (felhasználói valószínűségek): ", round(khi_negyzet_felhasznalo, 4), "\n")
if (khi_negyzet_felhasznalo > kritikus_ertek) {
  cat("❌ Elutasítjuk a H₀-t: a megfigyelések nem illeszkednek a megadott valószínűségekre.\n")
} else {
  cat("✅ Nem utasítjuk el a H₀-t: a megfigyelések illeszkedhetnek a megadott valószínűségekre.\n")
}

cat("\n🔵 Khi-négyzet érték (egyenletes eloszlás): ", round(khi_negyzet_egyenletes, 4), "\n")
if (khi_negyzet_egyenletes > kritikus_ertek) {
  cat("❌ Elutasítjuk a H₀-t: az eloszlás nem egyenletes.\n")
} else {
  cat("✅ Nem utasítjuk el a H₀-t: az eloszlás lehet egyenletes.\n")
}

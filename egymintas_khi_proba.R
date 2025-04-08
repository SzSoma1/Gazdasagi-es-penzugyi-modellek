# --- Tolerancia az úszó pontos számítások miatt ---
tolerancia <- 1e-6

# --- Kritikus értékek 95%-os szinten (df = 0-tól 100-ig előre kiszámolva) ---
kritikus_ertekek <- qchisq(0.95, df = 0:100)

# --- Biztonságos egész szám beolvasása a stdin-ből ---
read_integer <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    szam <- suppressWarnings(as.integer(strsplit(line, "#")[[1]][1]))
    if (!is.na(szam) && szam > 0) return(szam)
    cat("❌ Hibás bevitel! Pozitív egész számot adj meg.\n")
  }
}

# --- Biztonságos lebegőpontos szám beolvasása [0,1] intervallumban ---
read_probability <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    szam <- suppressWarnings(as.numeric(strsplit(line, "#")[[1]][1]))
    if (!is.na(szam) && szam >= 0 && szam <= 1) return(szam)
    cat("❌ Hibás bevitel! Csak 0 és 1 közötti számot adj meg.\n")
  }
}

# --- Szabadságfok bekérése és kritikus érték hozzárendelése ---
repeat {
  eredmeny_lehetosegek <- read_integer("Hány különböző lehetséges dobási eredmény van? (pl. 6): ")

  if (eredmeny_lehetosegek <= 100) {
    kritikus_ertek <- kritikus_ertekek[eredmeny_lehetosegek]
    break
  } else {
    cat("A szabadságfok túl magas! Add meg manuálisan a kritikus értéket:\n")
    cat("  df =", eredmeny_lehetosegek, "\n")
    line <- readLines("stdin", n = 1)
    krit <- suppressWarnings(as.numeric(strsplit(line, "#")[[1]][1]))
    if (!is.na(krit) && krit > 0) {
      kritikus_ertek <- krit
      break
    }
    cat("❌ Hibás bevitel! Pozitív számot adj meg a kritikus értékhez.\n")
  }
}

# --- Dobások számának bekérése ---
dobasok_szama <- read_integer("Hány dobás történt összesen? (pl. 100): ")

# --- Valószínűségek bekérése vagy generálása ---
repeat {
  cat("\nAdd meg az események valószínűségeit (összegüknek pontosan 1-nek kell lennie),\n")
  cat("vagy nyomj Entert véletlenszerű generáláshoz:\n")

  cat("Használj véletlenszerű értékeket? (igen/nem): ")
  input <- tolower(trimws(readLines("stdin", n = 1)))

  if (input == "" || input == "igen") {
    # Véletlen generálás
    tmp <- runif(eredmeny_lehetosegek)
    valoszinusegek <- tmp / sum(tmp)
    cat("🔹 Generált valószínűségek: ", paste(round(valoszinusegek, 3), collapse = ", "), "\n")
    break
  } else {
    # Kézi bevitel
    valoszinusegek <- numeric(eredmeny_lehetosegek)
    for (i in 1:eredmeny_lehetosegek) {
      valoszinusegek[i] <- read_probability(paste0("p[", i, "]: "))
    }

    if (abs(sum(valoszinusegek) - 1) < tolerancia) {
      break
    } else {
      cat("\n❌ A valószínűségek összegének pontosan 1-nek kell lennie. Próbáld újra!\n\n")
    }
  }
}

# --- Dobási eredmények (gyakoriságok) bekérése vagy generálása ---
repeat {
  cat("\nAdd meg az események előfordulásait, vagy nyomj Entert véletlenszerű generáláshoz:\n")
  cat("Használj véletlenszerű értékeket? (igen/nem): ")
  input <- tolower(trimws(readLines("stdin", n = 1)))

  if (input == "" || input == "igen") {
    dobas_eredmenyek <- round(valoszinusegek * dobasok_szama)
    cat("🔹 Generált előfordulások: ", paste(dobas_eredmenyek, collapse = ", "), "\n")
    break
  } else {
    dobas_eredmenyek <- numeric(eredmeny_lehetosegek)
    for (i in 1:eredmeny_lehetosegek) {
      repeat {
        cat(paste0("Esemény[", i, "] előfordulása: "))
        line <- readLines("stdin", n = 1)
        szam <- suppressWarnings(as.integer(strsplit(line, "#")[[1]][1]))
        if (!is.na(szam) && szam >= 0) {
          dobas_eredmenyek[i] <- szam
          break
        } else {
          cat("❌ Hibás bevitel! Pozitív egész számot adj meg.\n")
        }
      }
    }

    if (sum(dobas_eredmenyek) == dobasok_szama) {
      break
    } else {
      cat("\n❌ Az előfordulások összegének pontosan ", dobasok_szama, " kell lennie. Próbáld újra!\n\n")
    }
  }
}

# --- Khi-négyzet statisztika számítása ---
vart_ertekek <- dobasok_szama * valoszinusegek
khi_negyzet <- sum((dobas_eredmenyek - vart_ertekek)^2 / vart_ertekek)

# --- Eredmények kiírása ---
cat("\n📌 Khi-négyzet kritikus érték (α = 0.05):", round(kritikus_ertek, 4), "\n")
cat("🔵 Számított khi-négyzet érték: ", round(khi_negyzet, 4), "\n")

# --- Hipotézisvizsgálat eredménye ---
if (khi_negyzet > kritikus_ertek) {
  cat("❌ Elutasítjuk a nullhipotézist: a megfigyelt eloszlás nem egyezik a várt eloszlással.\n")
} else {
  cat("✅ Nem utasítjuk el a nullhipotézist: a megfigyelt eloszlás illeszkedik a várt eloszláshoz.\n")
}

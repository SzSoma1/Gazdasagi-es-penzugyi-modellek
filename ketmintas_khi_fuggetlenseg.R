# --- Biztonságos egész szám beolvasása a stdin-ből ---
read_integer <- function(prompt, min = 1) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    value <- suppressWarnings(as.integer(strsplit(line, "#")[[1]][1]))
    if (!is.na(value) && value >= min) return(value)
    cat(paste("Hibás bevitel! Legalább", min, "egész számot adj meg.\n"))
  }
}

# --- Biztonságos nemnegatív szám beolvasása (mátrixhoz) ---
read_nonneg_numeric <- function(prompt) {
  repeat {
    cat(prompt)
    line <- readLines("stdin", n = 1)
    value <- suppressWarnings(as.numeric(strsplit(line, "#")[[1]][1]))
    if (!is.na(value) && value >= 0) return(value)
    cat("Hibás bevitel! Csak nem negatív számot adhatsz meg.\n")
  }
}

# --- 1. Sor- és oszlopszám bekérése ---
r <- read_integer("Add meg a mátrix sorainak számát (minimum 2): ", min = 2)
s <- read_integer("Add meg a mátrix oszlopainak számát (minimum 1): ", min = 1)

# --- 2. Adatok bekérése a mátrixhoz ---
szukseges_db <- r * s
cat(paste("\nAdj meg", szukseges_db, "pozitív számot a", r, "x", s, "mátrixhoz:\n"))

input_adatok <- numeric(szukseges_db)
for (i in 1:szukseges_db) {
  prompt <- paste("  Add meg a(z)", i, ". számot: ")
  input_adatok[i] <- read_nonneg_numeric(prompt)
}

# --- 3. Megfigyelési mátrix létrehozása (soronként töltve) ---
tabla <- matrix(input_adatok, nrow = r, byrow = TRUE)

# --- 4. Sorösszegek számítása ---
sor_osszeg <- rowSums(tabla)

# --- 5. Oszlopösszegek számítása ---
oszlop_osszeg <- colSums(tabla)

# --- 6. Összes megfigyelés (N) kiszámítása ---
N <- sum(tabla)

# --- 7. Várható értékek mátrixának számítása ---
E <- matrix(0, nrow = r, ncol = s)
for (i in 1:r) {
  for (j in 1:s) {
    E[i, j] <- (sor_osszeg[i] * oszlop_osszeg[j]) / N
  }
}

# --- 8. Khi-négyzet statisztika számítása ---
chi2 <- sum((tabla - E)^2 / E)

# --- 9. Szabadságfok meghatározása ---
df <- (r - 1) * (s - 1)

# --- 10. Kritikus érték meghatározása táblából vagy kérésre ---
kritikus_ertekek <- c(3.84, 5.99, 7.81, 9.49, 11.07, 12.59, 14.07, 15.51, 16.92, 18.31)
if (df >= 1 && df <= 10) {
  kritikus_ertek <- kritikus_ertekek[df]
} else {
  # Felhasználótól bekérjük a kritikus értéket
  repeat {
    cat(paste("Add meg a kritikus értéket a df =", df, "esetén: "))
    val <- suppressWarnings(as.numeric(strsplit(readLines("stdin", n = 1), "#")[[1]][1]))
    if (!is.na(val) && val > 0) {
      kritikus_ertek <- val
      break
    }
    cat("Hibás érték! Pozitív számot adj meg.\n")
  }
}

# --- 11. Eredmények kiírása ---
cat("\n📊 Khi-négyzet statisztika:", round(chi2, 4), "\n")
cat("📐 Szabadságfok:", df, "\n")
cat("📏 Kritikus érték (α = 0.05):", kritikus_ertek, "\n")

# --- 12. Döntés a hipotézisről ---
if (chi2 > kritikus_ertek) {
  cat("❌ Elutasítjuk H₀-t: A változók között van kapcsolat.\n")
} else {
  cat("✅ Nem utasítjuk el H₀-t: A változók függetlenek.\n")
}

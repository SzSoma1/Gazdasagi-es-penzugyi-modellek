# Ismert adatpontok x és y koordinátái
x <- c(1, 2, 3, 4)
y <- c(5.6, 2.3, 3.2, 7.0)

# Az adatok száma (n = 4)
n <- length(x)

# Az a pont, ahol az interpolált értéket szeretnénk meghatározni
x0 <- 3.8

# Az interpolált érték (összeg) kezdőértéke
s <- 0

# Külső ciklus: végigmegyünk minden ismert ponton
for (i in 1:n) {
  # Az i-edik Lagrange-alapfüggvény kezdőértéke
  p <- 1

  # Belső ciklus: kiszámítjuk az i-edik Lagrange-alapfüggvényt
  for (j in 1:n) {
    if (j != i) {
      # Az alapfüggvény szorzatának aktuális tagja
      p <- p * (x0 - x[j]) / (x[i] - x[j])
    }
  }

  # Az interpolált értékhez hozzáadjuk a y[i] * L_i(x0) tagot
  s <- s + y[i] * p  # <- javítva a hibás "P"-ről "p"-re
}

# Az eredmény kiírása
print(paste("x0:", x0))  # Az a pont, ahol interpoláltunk
print(paste("y0:", s))   # A becsült (interpolált) y érték

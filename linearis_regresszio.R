# Vektorok definiálása
x <- c(1, 2, 3, 4)
y <- c(1.1, 1.9, 3, 4.1)

# Vektor elemeinek összege (beépített sum() nélkül)
sum_vector <- function(x) {
  if (length(x) == 0) {
    stop("Hiba: A vektor üres!")
  }

  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }

  total
}

# Két vektor skaláris szorzatának összege (beépített szorzás nélkül)
sum_product <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) {
    stop("Hiba: Az egyik vagy mindkét vektor üres!")
  }

  if (length(x) != length(y)) {
    stop("Hiba: A vektorok hossza nem egyezik!")
  }

  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i] * y[i]
  }

  total
}

# Minta elemszáma
n <- length(x)

# Regressziós egyenes meredekségének kiszámítása
a <- (n * sum_product(x, y) - sum_vector(x) * sum_vector(y)) /
  (n * sum_product(x, x) - sum_vector(x)^2)

# Regressziós egyenes tengelymetszetének kiszámítása
b <- (sum_product(x, x) * sum_vector(y) - sum_product(x, y) * sum_vector(x)) /
  (n * sum_product(x, x) - sum_vector(x)^2)

# Eredmények kiírása
cat("x =", toString(x), "\n")
cat("y =", toString(y), "\n")
cat("a =", a, "\n")  # Meredekség
cat("b =", b, "\n")  # Metszéspont

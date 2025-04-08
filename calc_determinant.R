# Saját függvény négyzetes mátrix ellenőrzésére
is_square_matrix <- function(m) {
  row_lengths <- sapply(m, length)
  length(m) == row_lengths[1] && all(row_lengths == row_lengths[1])
}

# Saját determináns függvény beépített függvények nélkül
calc_determinant <- function(m) {
  if (!is_square_matrix(m)) {
    stop("Error: The matrix is not a square matrix!")
  }
  n <- length(m)
  if (n == 1) {
    return(m[[1]][1])
  }
  if (n == 2) {
    return(m[[1]][1] * m[[2]][2] - m[[1]][2] * m[[2]][1])
  }
  det_value <- 0
  for (j in 1:n) {
    # Részmátrix: 1. sor kivéve, j-edik oszlop kivéve
    submatrix <- list()
    for (i in 2:n) {
      row <- m[[i]][-j]
      submatrix[[length(submatrix) + 1]] <- row
    }
    cofactor <- (-1)^(1 + j) * calc_determinant(submatrix)
    det_value <- det_value + m[[1]][j] * cofactor
  }
  det_value
}

# Mátrix listákból (nem használunk matrix(), csak listákat)
m <- list(
  c(6, 1, 1),
  c(4, -2, 5),
  c(2, 8, 7)
)

# Kiíratás print nélkül
for (row in m) {
  for (val in row) {
    cat(val, " ")
  }
  cat("\n")
}

cat("\nDeterminant = ", calc_determinant(m), "\n")

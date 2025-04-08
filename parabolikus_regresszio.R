# Mintaadatok
x <- c(1, 2, 3, 4, 5)
y <- c(1.2, 1.9, 3.2, 3.8, 5.1)

# Segédváltozók
x2 <- x^2
x3 <- x^3
x4 <- x^4
xy <- x * y
x2y <- x2 * y

# Összegek
n <- length(x)
s_x <- sum(x)
s_x2 <- sum(x2)
s_x3 <- sum(x3)
s_x4 <- sum(x4)
s_y <- sum(y)
s_xy <- sum(xy)
s_x2y <- sum(x2y)

# Normálegyenlet mátrixok
a_m <- matrix(c(
  n, s_x, s_x2,
  s_x, s_x2, s_x3,
  s_x2, s_x3, s_x4
), nrow = 3, byrow = TRUE)

b_m <- c(s_y, s_xy, s_x2y)

# Egyenletrendszer megoldása
coeffs <- solve(A, B)
a <- coeffs[1]
b <- coeffs[2]
c <- coeffs[3]

# Eredmény
cat("Illesztett parabola: y =", round(a, 4), "+", round(b, 4), "* x +",
    round(c, 4), "* x^2\n")

# Előrejelzett értékek
y_pred <- a + b * x + c * x^2
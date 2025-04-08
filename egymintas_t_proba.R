# A minta adatai (pl. mérési eredmények)
x <- c(5.5, 4.8, 5.0, 5.1)

# A nullhipotézis szerinti átlag (mu0): H0: a minta átlaga = 5.0
mu0 <- 5.0

# A minta elemszáma
n_x <- length(x)

# A minta átlaga (mean)
mean_x <- sum(x) / n_x

# A minta szórásnégyzetének (varianciájának) kiszámítása
# Ez a klasszikus n-1-es nevezőjű becslés a szórásra
var_x <- sum((x - mean_x)^2) / (n_x - 1)

# A minta szórása (standard deviation)
sd_x <- sqrt(var_x)

# A t-próba statisztika kiszámítása
# t = (mintaátlag - hipotézis szerinti átlag) / (szórás / √n)
t_stat <- (mean_x - mu0) / (sd_x / sqrt(n_x))

# A szabadságfok: n - 1
df <- n_x - 1

# Kritikus t-érték (pl. 0.05 szignifikanciaszint mellett, df=3)
# Ez az érték táblázatból vagy `qt()` függvénnyel szerezhető
t_crit <- 3.182  # kétoldali teszthez, 0.05 szinten, 3 szabadságfok

# A döntési szabály: ha a teszt statisztika abszolút értéke nagyobb, mint a kritikus érték → elutasítjuk H0-t
if (abs(t_stat) > t_crit) {
  decision <- "Elutasítjuk H0-t: Szignifikáns eltérés van."
} else {
  decision <- "Elfogadjuk H0-t: Nincs szignifikáns eltérés."
}

# A döntés kiírása
print(paste("Döntés:", decision))

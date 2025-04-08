# Két minta adatainak definiálása
x <- c(5.5, 4.8, 5.0, 5.1)  # Első minta
y <- c(4.7, 5.2, 5.1)       # Második minta

# Mintaelemszámok meghatározása
n_x <- length(x)  # Első minta elemszáma
n_y <- length(y)  # Második minta elemszáma

# Mintaátlagok kiszámítása
mean_x <- sum(x) / n_x  # Első minta átlaga
mean_y <- sum(y) / n_y  # Második minta átlaga

# Minta szórásnégyzetek (empirikus varianciák) kiszámítása
var_x <- sum((x - mean_x)^2) / (n_x - 1)  # Első minta szórásnégyzete
var_y <- sum((y - mean_y)^2) / (n_y - 1)  # Második minta szórásnégyzete

# Egyesített (pooled) szórásnégyzet számítása
sp2 <- ((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2)

# t-próba statisztika kiszámítása
t_stat <- (mean_x - mean_y) / sqrt(sp2 * (1/n_x + 1/n_y))

# Szabadságfok meghatározása a kétmintás t-próbához
df <- n_x + n_y - 2

# Kritikus t-érték megadása (pl. 5%-os szignifikanciaszintnél, kétoldali teszt esetén)
t_crit <- 2.571  # Ez függ a választott szignifikanciaszinttől és df-től

# Döntéshozatal: szignifikáns-e az átlagok közti eltérés?
if (abs(t_stat) > t_crit) {
  decision <- "Elutasítjuk H0-t: Szignifikáns eltérés van."  # Ha túl nagy a t-érték, van eltérés
} else {
  decision <- "Elfogadjuk H0-t: Nincs szignifikáns eltérés."  # Ha nem nagy a t-érték, nincs eltérés
}

# Eredmény kiírása
print(paste("Döntés:", decision))

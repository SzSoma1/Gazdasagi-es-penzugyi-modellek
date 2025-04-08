# Két független minta definiálása
x <- c(5.5, 4.8, 5.0, 5.1)  # Első minta
y <- c(4.7, 5.2, 5.1)       # Második minta

# A minták elemszáma
n_x <- length(x)  # Első minta elemszáma (4)
n_y <- length(y)  # Második minta elemszáma (3)

# Az összes adat összevonása és rangsorolása
all_data <- c(x, y)        # Az x és y vektorok összeillesztése
ranks <- rank(all_data)    # Az összes érték rangsorolása (kisebb érték -> kisebb rang)

# A rangok szétválasztása az eredeti minták szerint
ranks_x <- ranks[1:n_x]                          # Az első minta rangjai
ranks_y <- ranks[(n_x+1):(n_x+n_y)]              # A második minta rangjai

# Rangösszegek számítása
R_x <- sum(ranks_x)  # Az első minta rangösszege
R_y <- sum(ranks_y)  # A második minta rangösszege

# Mann–Whitney-féle U-statisztika kiszámítása
U_x <- R_x - (n_x * (n_x + 1)) / 2  # U érték az első minta alapján
U_y <- R_y - (n_y * (n_y + 1)) / 2  # U érték a második minta alapján

# A kisebbik U érték kiválasztása a döntéshez
U <- min(U_x, U_y)

# Kritikus U érték meghatározása kézzel (táblázatból)
U_crit <- 2  # Példa: n_x = 4, n_y = 3 esetén α = 0.05-ös szinten ez lehet a kritikus érték

# Döntéshozatal: összehasonlítás a kritikus értékkel
if (U <= U_crit) {
  decision <- "Elutasítjuk H0-t: Szignifikáns eltérés van."  # Van szignifikáns különbség
} else {
  decision <- "Elfogadjuk H0-t: Nincs szignifikáns eltérés."  # Nincs szignifikáns különbség
}

# Eredmény kiírása
print(paste("Döntés:", decision))

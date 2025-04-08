# A minta adatainak definiálása: 4 mérési érték (pl. valamilyen fizikai mennyiség mért adatai)
x <- c(5.5, 4.8, 5.0, 5.1)  

# A nullhipotézis szerinti várható érték (pl. "a medián érték 5.0")
mu0 <- 5.0  

# A minta és a nullhipotézis érték közötti különbségek
d <- x - mu0

# Kizárjuk a 0 különbségeket (ezek nem játszanak szerepet a rangsorolásban)
d_nonzero <- d[d != 0]

# A különbségek abszolút értékeinek rangsora (növekvő sorrendben rangsorolva)
ranks <- rank(abs(d_nonzero))

# A pozitív előjelű különbségekhez tartozó rangok összege (W+)
W_plus <- sum(ranks[d_nonzero > 0])

# A negatív előjelű különbségekhez tartozó rangok összege (W−)
W_minus <- sum(ranks[d_nonzero < 0])

# A statisztikai próba értéke: a kisebb rangösszeg (Wilcoxon statisztika)
W <- min(W_plus, W_minus)

# Az érvényes (nem 0 különbséget tartalmazó) elemek száma
n_eff <- length(d_nonzero)

# Kritikus érték a Wilcoxon táblázat alapján (n_eff = 3 esetén W_crit = 2 5%-os szinten, kétoldali próba)
W_crit <- 2  

# Döntés meghozatala: ha a statisztika kisebb vagy egyenlő a kritikus értékkel, elutasítjuk a nullhipotézist
if (W <= W_crit) {
  decision <- "Elutasítjuk H0-t: Szignifikáns eltérés van."
} else {
  decision <- "Elfogadjuk H0-t: Nincs szignifikáns eltérés."
}

# A döntés kiírása
print(paste("Döntés:", decision))

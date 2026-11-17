# ============================================================================
# Proyecto Final de Estadística - Análisis estadístico inferencial
# Equipo: 259790 / 257537 / 257743
# Entrada : datos_limpios.csv (frecuencias por pregunta y categoría)
# Salidas : carpeta resultados/ (tablas .csv y figuras .png)
# Ejecutar con el directorio de trabajo en la carpeta de este script.
# ============================================================================
dir.create("resultados", showWarnings = FALSE)

# ---- 1. Datos y parámetros del diseño --------------------------------------
datos <- read.csv("datos_limpios.csv", fileEncoding = "UTF-8")
N   <- 17000                       # tamaño de la población (Proyecto 4)
n   <- 1231                        # tamaño de la muestra
fpc <- sqrt((N - n) / (N - 1))     # corrección por población finita
alfa <- 0.05

fr <- function(preg, cat) datos$frecuencia[datos$pregunta == preg & datos$categoria == cat]

# ---- 2. Estimación puntual e intervalos de confianza -----------------------
ind <- data.frame(
  parametro = c("Mujeres", "Con conexión fija a internet", "Con laptop",
                "Con smartphone", "Con computador de escritorio", "Con tablet",
                "Sin ningún dispositivo", "Con participación previa en educación virtual",
                "Percibe complejidad leve o moderada", "Percibe complejidad alta",
                "Acepta refuerzo de contenidos por plataformas",
                "Acepta plataformas en emergencias"),
  x = c(fr("genero", "Mujer"), fr("internet_fijo", "Sí"),
        fr("dispositivos", "Laptop"), fr("dispositivos", "Smartphone"),
        fr("dispositivos", "Computador de escritorio"), fr("dispositivos", "Tablet"),
        fr("dispositivos", "Ninguno de los anteriores"), fr("curso_virtual_previo", "Sí"),
        fr("complejidad_uso", "Leve") + fr("complejidad_uso", "Moderado"),
        fr("complejidad_uso", "Alto"), fr("acepta_refuerzo", "Sí"),
        fr("acepta_emergencia", "Sí")),
  stringsAsFactors = FALSE)

wilson <- function(x, n, conf) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n
  centro <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  mg <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(centro - mg, centro + mg)
}

ind$p_hat <- ind$x / n
ind$ee    <- sqrt(ind$p_hat * (1 - ind$p_hat) / n) * fpc
for (cf in c(0.95, 0.97)) {
  z <- qnorm(1 - (1 - cf) / 2); tag <- round(cf * 100)
  ind[[paste0("wald", tag, "_inf")]] <- ind$p_hat - z * ind$ee
  ind[[paste0("wald", tag, "_sup")]] <- ind$p_hat + z * ind$ee
}
w <- t(mapply(wilson, ind$x, MoreArgs = list(n = n, conf = 0.95)))
ind$wilson95_inf <- w[, 1]; ind$wilson95_sup <- w[, 2]
est <- ind
cols <- setdiff(names(ind), c("parametro", "x", "ee"))   # columnas en proporción -> %
est[cols] <- round(ind[cols] * 100, 2)
est$ee <- round(ind$ee * 100, 3)
write.csv(est, "resultados/estimacion_ic.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(est)

# ---- 3. Pruebas de hipótesis para una proporción ---------------------------
# Estadístico: z = (p_hat - p0) / ( sqrt(p0 (1 - p0) / n) * fpc )
# Condición de normalidad: n*p0 >= 10 y n*(1 - p0) >= 10
prueba <- function(id, texto, x, p0, alt) {
  p <- x / n
  ee0 <- sqrt(p0 * (1 - p0) / n) * fpc
  z <- (p - p0) / ee0
  pv <- switch(alt, greater = 1 - pnorm(z), less = pnorm(z), two.sided = 2 * (1 - pnorm(abs(z))))
  pv_ex <- binom.test(x, n, p0, alternative = alt)$p.value   # verificación exacta (sin fpc)
  data.frame(id = id, hipotesis = texto, p0 = p0, alternativa = alt, x = x,
             p_hat = p, z = z, valor_p = pv, valor_p_binomial_exacto = pv_ex,
             cond_np0 = n * p0, cond_nq0 = n * (1 - p0),
             decision = ifelse(pv < alfa, "Rechazar H0", "No rechazar H0"))
}
ph <- rbind(
  prueba("H1", "Conexión fija a internet: H0 p = 0.75 vs H1 p > 0.75",
         fr("internet_fijo", "Sí"), 0.75, "greater"),
  prueba("H2", "Participación previa en educación virtual: H0 p = 0.50 vs H1 p < 0.50",
         fr("curso_virtual_previo", "Sí"), 0.50, "less"),
  prueba("H3", "Acepta refuerzo de contenidos: H0 p = 0.60 vs H1 p > 0.60",
         fr("acepta_refuerzo", "Sí"), 0.60, "greater"),
  prueba("H4", "Acepta plataformas en emergencias: H0 p = 0.60 vs H1 p > 0.60",
         fr("acepta_emergencia", "Sí"), 0.60, "greater"))
ph_out <- ph; ph_out$p_hat <- round(ph$p_hat * 100, 2); ph_out$z <- round(ph$z, 3)
ph_out$valor_p <- signif(ph$valor_p, 3); ph_out$valor_p_binomial_exacto <- signif(ph$valor_p_binomial_exacto, 3)
write.csv(ph_out, "resultados/pruebas_hipotesis.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(ph_out)

# ---- 4. Bondad de ajuste (chi-cuadrada) para la complejidad percibida ------
# H0: las 4 categorías son equiprobables (25 % cada una)
cats <- c("Ninguno", "Leve", "Moderado", "Alto")
obs  <- sapply(cats, function(k) fr("complejidad_uso", k))
gof  <- chisq.test(obs, p = rep(0.25, 4))
cat("\nChi-cuadrada de bondad de ajuste:\n"); print(gof)
tab_gof <- data.frame(categoria = cats, observada = as.numeric(obs),
                      esperada = as.numeric(gof$expected),
                      residuo_estandarizado = round(as.numeric(gof$stdres), 2))
resumen_gof <- data.frame(chi2 = unname(gof$statistic), gl = unname(gof$parameter),
                          valor_p = gof$p.value, w_cohen = sqrt(unname(gof$statistic) / n),
                          esperada_minima = min(gof$expected))
write.csv(tab_gof, "resultados/gof_complejidad.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(resumen_gof, "resultados/gof_resumen.csv", row.names = FALSE, fileEncoding = "UTF-8")
print(tab_gof); print(resumen_gof)

# ---- 5. Verificación de supuestos por simulación (muestreo sin reemplazo) --
set.seed(2026)
p_pob <- ph$p_hat[1]                              # población ficticia con p = p_hat (H1)
pob <- c(rep(1, round(N * p_pob)), rep(0, N - round(N * p_pob)))
sim <- replicate(5000, mean(sample(pob, n)))
sim_res <- data.frame(media_sim = mean(sim), sd_sim = sd(sim),
                      ee_teorico = sqrt(p_pob * (1 - p_pob) / n) * fpc,
                      sesgo_asimetria = mean(((sim - mean(sim)) / sd(sim))^3))
write.csv(round(sim_res, 5), "resultados/simulacion_supuestos.csv", row.names = FALSE)
print(sim_res)

# ---- 6. Figuras ------------------------------------------------------------
png("resultados/fig06_pruebas_hipotesis.png", width = 1800, height = 1000, res = 200, type = "cairo")
par(mar = c(5, 17, 3, 2))
ci <- ind[c(2, 8, 11, 12), ]
etq <- c("H1: Internet fijo (p0 = 75 %)", "H2: Participación previa (p0 = 50 %)",
         "H3: Acepta refuerzo (p0 = 60 %)", "H4: Acepta en emergencias (p0 = 60 %)")
p0v <- c(75, 50, 60, 60)
y <- 4:1
plot(NA, xlim = c(40, 85), ylim = c(0.5, 4.5), yaxt = "n", xlab = "Proporción (%)", ylab = "",
     main = "Estimación (IC 95 %) frente al valor hipotético")
segments(ci$wald95_inf * 100, y, ci$wald95_sup * 100, y, lwd = 2)
points(ci$p_hat * 100, y, pch = 19, cex = 1.3)
points(p0v, y, pch = 4, col = "red", cex = 1.6, lwd = 3)
axis(2, at = y, labels = etq, las = 1, tick = FALSE)
legend("topleft", legend = c("Estimación e IC 95 %", "Valor hipotético p0"),
       pch = c(19, 4), col = c("black", "red"), bty = "n")
dev.off()

png("resultados/fig07_bondad_ajuste.png", width = 1600, height = 1000, res = 200, type = "cairo")
par(mar = c(5, 5, 3, 2))
m <- rbind(Observada = as.numeric(obs), Esperada = as.numeric(gof$expected))
colnames(m) <- cats
bp <- barplot(m, beside = TRUE, col = c("grey40", "grey85"), ylim = c(0, 650),
              ylab = "Número de estudiantes", xlab = "Complejidad percibida",
              main = "Complejidad percibida: frecuencias observadas y esperadas")
text(bp[1, ], m[1, ] + 25, m[1, ])
abline(h = gof$expected[1], lty = 2)
legend("topleft", legend = c("Observada", "Esperada (H0: uniforme)"),
       fill = c("grey40", "grey85"), bty = "n")
dev.off()
cat("\nListo. Resultados en la carpeta 'resultados/'.\n")

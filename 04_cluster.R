# ============================================================
# TFG — Motivaciones y criterios de elección de destino
# Script 04: Análisis clúster
# Autora: Cecilia Astarloa
# ============================================================
# ============================================================
# PASO 1: DETERMINAR EL NÚMERO ÓPTIMO DE CLÚSTERES
# ============================================================

# Las puntuaciones factoriales ya están estandarizadas (media 0, sd 1).
# Por tanto, no necesitamos volver a escalarlas.

set.seed(123)

# --- Método 1: Codo ---
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(datos_final[, c("factor_destino", "factor_digital")], 
                       centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de clústeres (k)",
     ylab = "Suma de cuadrados intragrupo",
     main = "Método del codo (factores)")

# --- Método 2: Silueta ---
sil <- numeric(10)
sil[1] <- NA

for (k in 2:10) {
  km <- kmeans(datos_final[, c("factor_destino", "factor_digital")], 
               centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(datos_final[, c("factor_destino", "factor_digital")]))
  sil[k] <- mean(ss[, 3])
}

plot(1:10, sil, type = "b", pch = 19,
     xlab = "Número de clústeres (k)",
     ylab = "Silueta media",
     main = "Método de la silueta (factores)")


# --- Método 3: NbClust ---
set.seed(123)
nb_factores <- NbClust(datos_final[, c("factor_destino", "factor_digital")], 
                       distance = "euclidean", 
                       min.nc = 2, max.nc = 8,
                       method = "kmeans", 
                       index = "all")


# ============================================================
# PASO 2: CLÚSTER CON K=3 SOBRE LOS FACTORES
# ============================================================

set.seed(123)
km_factores <- kmeans(datos_final[, c("factor_destino", "factor_digital")], 
                      centers = 3, nstart = 25)

# Asignar el segmento nuevo a cada persona
datos_final$segmento_factorial <- factor(km_factores$cluster,
                                         levels = 1:3,
                                         labels = c("Segmento F1", "Segmento F2", "Segmento F3"))

# --- Tamaños de los nuevos segmentos ---
cat("\n========== TAMAÑOS DE LOS NUEVOS SEGMENTOS ==========\n")
table(datos_final$segmento_factorial)

# --- Centros de los segmentos en los factores ---
cat("\n========== CENTROS DE LOS SEGMENTOS ==========\n")
round(km_factores$centers, 3)


# --- Guardar los datos con los nuevos segmentos ---
saveRDS(datos_final, "datos/datos_final.rds")

round(km_factores$centers, 3)
table(datos_final$segmento_factorial)

library(ggplot2)

ggplot(datos_final, aes(x = factor_destino, 
                        y = factor_digital, 
                        color = segmento_factorial)) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"),
                     name = "Segmento") +
  labs(title = "Distribución de los segmentos en el espacio factorial",
       x = "Atractivos del destino (RC1)",
       y = "Influencia social y digital (RC2)") +
  theme_minimal() +
  theme(legend.position = "bottom")


summary(aov(factor_destino ~ segmento_factorial, data = datos_final))
summary(aov(factor_digital ~ segmento_factorial, data = datos_final))






# ============================================================
# PRUEBA: K-MEDOIDS (PAM) sobre las puntuaciones factoriales
# ============================================================

library(cluster)


set.seed(123)

# Subset con las dos variables factoriales
datos_pam <- datos_final[, c("factor_destino", "factor_digital")]

# ============================================================
# PASO 1: NÚMERO ÓPTIMO DE CLUSTERS CON SILUETA
# ============================================================

sil_pam <- numeric(10)
sil_pam[1] <- NA

for (k in 2:10) {
  pam_fit <- pam(datos_pam, k = k)
  sil_pam[k] <- pam_fit$silinfo$avg.width
}

plot(1:10, sil_pam, type = "b", pch = 19,
     xlab = "Número de clústeres (k)",
     ylab = "Silueta media",
     main = "Método de la silueta - PAM (k-medoids)")

# Imprimir valores de silueta
cat("\n========== SILUETA MEDIA POR k (PAM) ==========\n")
print(round(sil_pam, 3))


# ============================================================
# PASO 2: NbClust para PAM (validación cruzada del número de k)
# ============================================================

set.seed(123)
nb_pam <- NbClust(datos_pam, 
                  distance = "euclidean", 
                  min.nc = 2, max.nc = 8,
                  method = "median",   # equivalente más cercano a PAM en NbClust
                  index = "all")


# ============================================================
# PASO 3: APLICAR PAM CON k = 3
# ============================================================

set.seed(123)
pam_3 <- pam(datos_pam, k = 3)

# Asignar segmentos
datos_final$segmento_pam <- factor(pam_3$clustering,
                                   levels = 1:3,
                                   labels = c("PAM 1", "PAM 2", "PAM 3"))

# --- Tamaños de los segmentos ---
cat("\n========== TAMAÑOS DE SEGMENTOS PAM ==========\n")
print(table(datos_final$segmento_pam))

# --- Medoides (las observaciones reales que son centro de cada cluster) ---
cat("\n========== MEDOIDES (observaciones centrales) ==========\n")
print(pam_3$medoids)

# --- Centros calculados como media de cada cluster (para comparar con k-means) ---
cat("\n========== CENTROS PAM (medias por cluster) ==========\n")
centros_pam <- aggregate(datos_pam, 
                         by = list(cluster = pam_3$clustering), 
                         FUN = mean)
print(round(centros_pam, 3))

# --- Silueta media de la solución k=3 ---
cat("\n========== SILUETA MEDIA PAM k=3 ==========\n")
print(round(pam_3$silinfo$avg.width, 3))

# --- Silueta por cluster ---
cat("\n========== SILUETA POR CLUSTER ==========\n")
print(round(pam_3$silinfo$clus.avg.widths, 3))


# ============================================================
# PASO 4: COMPARAR PAM CON K-MEANS
# ============================================================

cat("\n========== COMPARACIÓN: K-MEANS vs PAM ==========\n")
print(table(K_means = datos_final$segmento_factorial, 
            PAM = datos_final$segmento_pam))




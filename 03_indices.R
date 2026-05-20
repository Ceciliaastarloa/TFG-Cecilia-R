# ============================================================
# TFG — Motivaciones y criterios de elección de destino
# Script 06: Clúster ALTERNATIVO sobre las 10 variables pull
# Autora: Cecilia Astarloa
# ============================================================

# Carga de paquetes
library(tidyverse)
library(cluster)
library(NbClust)

# Carga de datos limpios
datos_final <- readRDS("datos/datos_final.rds")

# Definir las 10 variables pull
vars_pull <- c("pull_precio", "pull_seguridad", "pull_acceso", "pull_clima",
               "pull_gastronomia", "pull_cultura", "pull_naturaleza",
               "pull_recom", "pull_resenas", "pull_rrss")

# Verificar que todas las variables están y no tienen NAs
summary(datos_final[, vars_pull])

# Estandarizar las 10 variables
datos_pull_z <- scale(datos_final[, vars_pull])

# Verificar que la estandarización funcionó (medias = 0, sd = 1)
round(colMeans(datos_pull_z), 3)
round(apply(datos_pull_z, 2, sd), 3)


# Subset solo con esas variables
datos_pull <- datos_final[, vars_pull]


# ============================================================
# PASO 1: COMPROBAR QUE LOS DATOS SON FACTORIZABLES
# ============================================================

# Test de adecuación muestral (KMO)
# Valores deseables: > 0.6 aceptable, > 0.7 bueno, > 0.8 muy bueno
KMO(datos_pull)

# Test de esfericidad de Bartlett 
# Si p < 0.05, las variables están suficientemente correlacionadas para hacer factorial
cortest.bartlett(cor(datos_pull), n = nrow(datos_pull))


# ============================================================
# PASO 2: DETERMINAR EL NÚMERO DE FACTORES A EXTRAER
# ============================================================

# Scree plot y análisis paralelo
fa.parallel(datos_pull, fa = "pc", n.iter = 100, show.legend = TRUE,
            main = "Análisis paralelo - Componentes principales")


# ============================================================
# PASO 3: FACTORIAL CON 2 COMPONENTES
# ============================================================

# Análisis de componentes principales con rotación varimax
fa1 <- principal(datos_pull, nfactors = 2, rotate = "varimax")

# Mostrar las cargas factoriales (loadings)
print(fa1$loadings, cutoff = 0.3, sort = TRUE)

# Comunalidades (cuánta varianza de cada variable explica el modelo)
fa1$communality

# ============================================================
# PASO 4: FACTORIAL ITERATIVO - QUITAMOS pull_seguridad y pull_precio
# ============================================================

vars_pull_v2 <- c("pull_acceso", "pull_clima",
                  "pull_gastronomia", "pull_cultura", "pull_naturaleza",
                  "pull_recom", "pull_resenas", "pull_rrss")

datos_pull_v2 <- datos_final[, vars_pull_v2]

# Análisis paralelo para ver cuántos factores ahora
fa.parallel(datos_pull_v2, fa = "pc", n.iter = 100, show.legend = TRUE,
            main = "Análisis paralelo - Sin precio ni seguridad")

# Factorial con 2 componentes
fa2 <- principal(datos_pull_v2, nfactors = 2, rotate = "varimax")
print(fa2$loadings, cutoff = 0.3, sort = TRUE)

# Comunalidades
fa2$communality


# ============================================================
# PASO 5: GUARDAR LAS PUNTUACIONES FACTORIALES
# ============================================================

# Las puntuaciones factoriales son los "scores" que cada persona obtiene
# en cada uno de los dos factores. Las usaremos como input del clúster.

datos_final$factor_destino <- fa2$scores[, 1]   # RC1: Atractivos del destino
datos_final$factor_digital <- fa2$scores[, 2]   # RC2: Influencia social y digital

# Verificar que las puntuaciones se han creado bien
summary(datos_final[, c("factor_destino", "factor_digital")])



######## ALFA CRONBACH #########

library(psych)

# --- Alfa del Factor 1: Atractivos del destino ---
vars_factor1 <- c("pull_acceso", "pull_clima", "pull_gastronomia", 
                  "pull_cultura", "pull_naturaleza")

cat("\n========== FACTOR 1: ATRACTIVOS DEL DESTINO ==========\n")
alpha(datos_final[, vars_factor1])


# --- Alfa del Factor 2: Influencia social y digital ---
vars_factor2 <- c("pull_recom", "pull_resenas", "pull_rrss")

cat("\n========== FACTOR 2: INFLUENCIA SOCIAL Y DIGITAL ==========\n")
alpha(datos_final[, vars_factor2])



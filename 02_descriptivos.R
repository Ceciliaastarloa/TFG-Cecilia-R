# ============================================================
# TFG — Motivaciones y criterios de elección de destino
# Script 02: Análisis descriptivo
# Autora: Cecilia Astarloa
# ============================================================

#Carga de paquetes
library(tidyverse)
library(janitor)

#Carga de datos limpios
datos_final <- readRDS("datos/datos_final.rds")

# Verificación rápida
nrow(datos_final)
names(datos_final)

# ============================================================
# BLOQUE A — DESCRIPTIVOS DE VARIABLES CATEGÓRICAS (FRECUENCIAS ABSOLUTAS Y RELATIVAS)
# ============================================================

# Edad
table(datos_final$sd_edad)
round(prop.table(table(datos_final$sd_edad)) * 100, 1)

# Género
table(datos_final$sd_genero)
round(prop.table(table(datos_final$sd_genero)) * 100, 1)

# Estudios
table(datos_final$sd_estudios)
round(prop.table(table(datos_final$sd_estudios)) * 100, 1)

# Situación laboral
table(datos_final$sd_laboral)
round(prop.table(table(datos_final$sd_laboral)) * 100, 1)

# Ingresos
table(datos_final$sd_ingresos)
round(prop.table(table(datos_final$sd_ingresos)) * 100, 1)

# Comunidad autónoma
table(datos_final$sd_ccaa)
round(prop.table(table(datos_final$sd_ccaa)) * 100, 1)

# Frecuencia de viajes
table(datos_final$viaje_frecuencia)
round(prop.table(table(datos_final$viaje_frecuencia)) * 100, 1)

# Duración preferida
table(datos_final$viaje_duracion)
round(prop.table(table(datos_final$viaje_duracion)) * 100, 1)

# Compañía
table(datos_final$viaje_compania)
round(prop.table(table(datos_final$viaje_compania)) * 100, 1)

# Tipo de destino
table(datos_final$viaje_tipo_destino)
round(prop.table(table(datos_final$viaje_tipo_destino)) * 100, 1)

# Ámbito geográfico
table(datos_final$viaje_ambito)
round(prop.table(table(datos_final$viaje_ambito)) * 100, 1)

# Transporte 
table(datos_final$viaje_transporte)
round(prop.table(table(datos_final$viaje_transporte)) * 100, 1)

# Alojamiento
table(datos_final$viaje_alojamiento)
round(prop.table(table(datos_final$viaje_alojamiento)) * 100, 1)

# Antelación
table(datos_final$viaje_antelacion)
round(prop.table(table(datos_final$viaje_antelacion)) * 100, 1)

# Plataforma de redes
table(datos_final$rrss_plataforma)
round(prop.table(table(datos_final$rrss_plataforma)) * 100, 1)

#Contenido influyente
table(datos_final$rrss_contenido)
round(prop.table(table(datos_final$rrss_contenido)) * 100, 1)



# ============================================================
# BLOQUE B — DESCRIPTIVOS DE VARIABLES LIKERT
# ============================================================
library(psych)

# Variables push (motivaciones)
vars_push <- c("push_descanso", "push_familia", "push_nuevos", "push_cultura",
               "push_aventura", "push_rrss", "push_moda")

describe(datos_final[, vars_push])

# --- Variables pull (criterios de elección) ---
vars_pull <- c("pull_precio", "pull_seguridad", "pull_clima", "pull_acceso",
               "pull_gastronomia", "pull_cultura", "pull_naturaleza",
               "pull_recom", "pull_resenas", "pull_rrss")

describe(datos_final[, vars_pull])


# --- Variables de redes sociales (Likert) ---
vars_rrss <- c("rrss_uso", "rrss_influencia")
describe(datos_final[, vars_rrss])


library(ggplot2)

# ============================================================
# GRÁFICO 1A: Medias de motivaciones push
# ============================================================

medias_push <- data.frame(
  variable = vars_push,
  media = colMeans(datos_final[, vars_push], na.rm = TRUE)
)

# Aplicar etiquetas legibles
etiquetas_push <- c(
  push_descanso = "Descansar y desconectar",
  push_familia  = "Tiempo con familia/amigos",
  push_nuevos   = "Conocer lugares nuevos",
  push_cultura  = "Conocer otras culturas",
  push_aventura = "Aventura y experiencias",
  push_rrss     = "Compartir en redes",
  push_moda     = "Destinos de moda"
)
medias_push$variable_legible <- etiquetas_push[medias_push$variable]

ggplot(medias_push, aes(x = reorder(variable_legible, media), y = media)) +
  geom_bar(stat = "identity", fill = "#2E86AB", width = 0.7) +
  geom_text(aes(label = round(media, 2)), hjust = -0.2, size = 3.5) +
  coord_flip(ylim = c(1.5, 5)) +
  scale_y_continuous(breaks = seq(1.5, 5, 0.5)) +
  labs(
    title = "Importancia media de las motivaciones de viaje (push)",
    subtitle = "Escala 1 = Nada importante; 5 = Muy importante",
    x = NULL,
    y = "Media (eje truncado en 1,5)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


# ============================================================
# GRÁFICO 1B: Medias de criterios de elección pull
# ============================================================

medias_pull <- data.frame(
  variable = vars_pull,
  media = colMeans(datos_final[, vars_pull], na.rm = TRUE)
)

etiquetas_pull <- c(
  pull_precio      = "Precio",
  pull_seguridad   = "Seguridad",
  pull_clima       = "Clima",
  pull_acceso      = "Accesibilidad",
  pull_gastronomia = "Gastronomía",
  pull_cultura     = "Oferta cultural",
  pull_naturaleza  = "Entorno natural",
  pull_recom       = "Recomendaciones cercanas",
  pull_resenas     = "Reseñas online",
  pull_rrss        = "Contenido en redes"
)
medias_pull$variable_legible <- etiquetas_pull[medias_pull$variable]

ggplot(medias_pull, aes(x = reorder(variable_legible, media), y = media)) +
  geom_bar(stat = "identity", fill = "#E63946", width = 0.7) +
  geom_text(aes(label = round(media, 2)), hjust = -0.2, size = 3.5) +
  coord_flip(ylim = c(1.5, 5)) +
  scale_y_continuous(breaks = seq(1.5, 5, 0.5)) +
  labs(
    title = "Importancia media de los criterios de elección de destino (pull)",
    subtitle = "Escala 1 = Nada importante; 5 = Muy importante",
    x = NULL,
    y = "Media (eje truncado en 1,5)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



# ============================================================
# GRÁFICO 2: Mapa de correlaciones entre los criterios pull
# (con etiquetas legibles)
# ============================================================

library(corrplot)
# Definir el orden de las variables por bloques teóricos
orden_bloques <- c(
  "pull_precio", "pull_seguridad", "pull_acceso",        # Pragmáticos
  "pull_clima", "pull_gastronomia", "pull_cultura", "pull_naturaleza",  # Experiencial
  "pull_recom", "pull_resenas", "pull_rrss"              # Social/digital
)

# Recalcular la matriz con ese orden
matriz_cor_ordenada <- cor(datos_final[, orden_bloques], use = "complete.obs")

# Aplicar etiquetas legibles
rownames(matriz_cor_ordenada) <- etiquetas_pull[rownames(matriz_cor_ordenada)]
colnames(matriz_cor_ordenada) <- etiquetas_pull[colnames(matriz_cor_ordenada)]


# Visualizar
corrplot(matriz_cor_ordenada,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.75,
         tl.cex = 0.85,
         col = colorRampPalette(c("#E63946", "white", "#2E86AB"))(200),
         title = "Correlaciones entre criterios pull (ordenados por bloques teóricos)",
         mar = c(0, 0, 2, 0))



corrplot(matriz_cor_ordenada, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200))

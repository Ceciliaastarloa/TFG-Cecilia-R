
# ============================================================
# TFG — Motivaciones y criterios de elección de destino
# Script 05: Caracterización de los segmentos factoriales
# Autora: Cecilia Astarloa
# ============================================================

# --- Carga de paquetes ---
library(tidyverse)
library(car)

# --- Carga de datos ---
datos_final <- readRDS("datos/datos_final.rds")


# ============================================================
# CARACTERIZACIÓN POR MOTIVACIONES PUSH (ANOVA)
# ============================================================

vars_push <- c("push_descanso", "push_familia", "push_aventura",
               "push_cultura", "push_nuevos", "push_rrss", "push_moda")

cat("\n========== ANOVA - MOTIVACIONES PUSH ==========\n")
for (v in vars_push) {
  cat("\n--- Variable:", v, "---\n")
  formula <- as.formula(paste(v, "~ segmento_factorial"))
  modelo <- aov(formula, data = datos_final)
  print(summary(modelo))
  
  cat("Medias por segmento:\n")
  print(tapply(datos_final[[v]], datos_final$segmento_factorial, mean, na.rm = TRUE))
  
  cat("Test de normalidad de residuos (Shapiro-Wilk):\n")
  print(shapiro.test(residuals(modelo)))
  
  cat("Test de homocedasticidad (Levene):\n")
  print(leveneTest(formula, data = datos_final))
}




# ============================================================
# CARACTERIZACIÓN SOCIODEMOGRÁFICA (CHI-CUADRADO)
# ============================================================

vars_sd <- c("sd_edad", "sd_genero", "sd_estudios", 
             "sd_laboral", "sd_ingresos", "sd_ccaa")

cat("\n========== CHI-CUADRADO - SOCIODEMOGRÁFICOS ==========\n")
for (v in vars_sd) {
  cat("\n--- Variable:", v, "---\n")
  tabla <- table(datos_final$segmento_factorial, datos_final[[v]])
  print(tabla)
  print(chisq.test(tabla))
  
  # Porcentajes por segmento (filas)
  cat("Porcentajes por segmento (%):\n")
  print(round(prop.table(tabla, margin = 1) * 100, 1))
}


# ============================================================
# CARACTERIZACIÓN POR HÁBITOS DE VIAJE (CHI-CUADRADO)
# ============================================================


cat("\n========== CHI-CUADRADO - HÁBITOS DE VIAJE ==========\n")

vars_viaje <- c("viaje_frecuencia", "viaje_duracion", "viaje_compania",
                "viaje_tipo_destino", "viaje_ambito", "viaje_transporte",
                "viaje_alojamiento", "viaje_antelacion")

for (v in vars_viaje) {
  cat("\n--- Variable:", v, "---\n")
  
  datos_v <- datos_final[!is.na(datos_final[[v]]) & !is.na(datos_final$segmento_factorial), ]
  
  tabla <- table(datos_v$segmento_factorial, datos_v[[v]])
  print(tabla)
  
  if (nrow(tabla) > 1 && ncol(tabla) > 1) {
    print(chisq.test(tabla))
    cat("Porcentajes por segmento (%):\n")
    print(round(prop.table(tabla, margin = 1) * 100, 1))
  }
}

# ============================================================
# CARACTERIZACIÓN POR USO DE REDES SOCIALES
# ============================================================

# Variables Likert (ANOVA)
vars_rrss_likert <- c("rrss_uso", "rrss_influencia")

cat("\n========== ANOVA - USO E INFLUENCIA REDES ==========\n")
for (v in vars_rrss_likert) {
  cat("\n--- Variable:", v, "---\n")
  formula <- as.formula(paste(v, "~ segmento_factorial"))
  modelo <- aov(formula, data = datos_final)
  print(summary(modelo))
  
  cat("Medias por segmento:\n")
  print(tapply(datos_final[[v]], datos_final$segmento_factorial, mean, na.rm = TRUE))
  
  cat("Test de normalidad de residuos (Shapiro-Wilk):\n")
  print(shapiro.test(residuals(modelo)))
  
  cat("Test de homocedasticidad (Levene):\n")
  print(leveneTest(formula, data = datos_final))
}

# --- Comprobación de supuestos del ANOVA ---
cat("Test de normalidad de residuos (Shapiro-Wilk):\n")
print(shapiro.test(residuals(modelo)))

cat("Test de homocedasticidad (Levene):\n")
print(leveneTest(formula, data = datos_final))


# Variables categóricas (Chi-cuadrado)
vars_rrss_cat <- c("rrss_plataforma", "rrss_contenido")

cat("\n========== CHI-CUADRADO - PLATAFORMA Y CONTENIDO ==========\n")
for (v in vars_rrss_cat) {
  cat("\n--- Variable:", v, "---\n")
  tabla <- table(datos_final$segmento_factorial, datos_final[[v]])
  print(tabla)
  print(chisq.test(tabla))
  
  cat("Porcentajes por segmento (%):\n")
  print(round(prop.table(tabla, margin = 1) * 100, 1))
}

# ============================================================
# TFG — Motivaciones y criterios de elección de destino
# Script 01: Carga y limpieza de datos
# Autora: Cecilia Astarloa
# ============================================================

# Instalación de paquetes

#install.packages("tidyverse")
#install.packages("psych")
#install.packages("corrplot")
#install.packages("cluster")
#install.packages("NbClust")

# Carga de paquetes 
library(tidyverse)   # manipulación general de datos
library(janitor)     # limpieza de nombres y tablas
library(psych)       # descriptivos y ACP

# Carga de datos
datos <- read.csv2("datos/Resultados_encuesta_CSV_copia.csv",
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8")

# Primera visualización de los datos
dim(datos) # Para ver cuántas filas y columnas tiene

names(datos) # Nombres de las columnas

head(datos) # Ver las primeras 6 filas 

str(datos) # Estructura: tipo de cada variable

summary(datos) # Resumen estadístico básico de cada variable

View(datos) # Para ver todos los datos en una tabla en una pestaña nueva



names(datos)[names(datos) == "rss_uso"] <- "rrss_uso" # cambio el nombre que estaba mal escrito
names(datos)[names(datos) == "pull_gastronómica"] <- "pull_gastronomia"


#Primero vamos a filtrar todos los datos de las personas que no son residentes en España
datos_es <- datos[datos$Residencia == "Sí", ]
nrow(datos_es) #nos quedan 178 respuestas

# Ver cuantos NAs hay por cada columna
colSums(datos_es == "", na.rm = TRUE)
View(datos_es)

# ============================================================
# RECODIFICACIÓN DE LIKERT + LIMPIEZA FINAL
# ============================================================

# Ver que posibles valores hay como respuestas en esas columnas,
#para despues poder cambiarlo y escribir el codigo correctamente.
unique(datos$push_descanso)
unique(datos$pull_precio)
unique(datos$rrss_uso)
unique(datos$rrss_influencia)

#1. Primero, convertimos los "" (respuestas vacías) en NA
# para que R sepa operar con ellos
datos_es[datos_es == ""] <- NA


#2. Lista de variables Likert con formato "N = Etiqueta"
# Son los 7 push, los 10 pull y rrss_influencia: 18 variables.
vars_likert_num <- c(
  "push_descanso", "push_familia", "push_nuevos", "push_cultura",
  "push_aventura", "push_rrss", "push_moda",
  "pull_precio", "pull_seguridad", "pull_clima", "pull_acceso",
  "pull_gastronomia", "pull_cultura", "pull_naturaleza",
  "pull_recom", "pull_resenas", "pull_rrss",
  "rrss_influencia"
)

# Convertimos los resultados de cada celda a numeros dentro de la escala likert 
#(mediante solo dejar el primer caracter de cada celda en un bucle for 
# que va cogiendo cada columna del string que hemos hecho antes)

for (v in vars_likert_num) {
  datos_es[[v]] <- as.integer(substr(datos_es[[v]], 1, 1))
}


# Como esta columna tenia los resultados distintos a las otras columnas se hace de una manera diferente. 
# Aquí no podemos usar substr porque no hay número delante.
# Damos a cada etiqueta un numero correspondiente(y se pone L para que R lo guarde como numero entero, como antes, sin espacio para decimales)
datos_es$rrss_uso <- dplyr::recode(datos_es$rrss_uso,
                                   "Nunca"          = 1L,
                                   "Rara vez"       = 2L,
                                   "A veces"        = 3L,
                                   "Frecuentemente" = 4L,
                                   "Siempre"        = 5L
)


# Verificar que efectivamente las likert ahora son números
# Hemos cogido solo esas cuatro columnas porque son respresntativas (1 de pull, 1 de push, 1 de rrss y la otra la que era distinta)

summary(datos_es[, c("push_descanso", "pull_precio", "rrss_uso", "rrss_influencia")])


# Ahora eliminamos casos con NA en cualquiera de los 10 pull(porque son nuestras variables)
vars_pull <- c(
  "pull_precio", "pull_seguridad", "pull_clima", "pull_acceso",
  "pull_gastronomia", "pull_cultura", "pull_naturaleza",
  "pull_recom", "pull_resenas", "pull_rrss"
)

# complete.cases() devuelve TRUE para las filas sin NA en las columnas indicadas.
datos_final <- datos_es[complete.cases(datos_es[, vars_pull]), ]
View(datos_final)

# Verificar cuántos casos efectivos quedan
nrow(datos_final)


# ============================================================
# LIMPIEZA DE INCONSISTENCIAS EN CATEGÓRICAS
# ============================================================

#Revisar que todas las categorias tienen las variables correspondientes
table(datos_final$sd_edad)
table(datos_final$sd_genero)
table(datos_final$sd_estudios)
table(datos_final$sd_laboral)
table(datos_final$sd_ingresos)
table(datos_final$sd_ccaa)
table(datos_final$viaje_frecuencia)
table(datos_final$viaje_duracion)
table(datos_final$viaje_compania)
table(datos_final$viaje_tipo_destino)
table(datos_final$viaje_ambito)
table(datos_final$viaje_transporte)
table(datos_final$viaje_alojamiento)
table(datos_final$viaje_antelacion)
table(datos_final$rrss_plataforma)
table(datos_final$rrss_contenido)


#Vemos que en muchas de ellas hay inconsistencias 

# variable sd_edad: añadir "años" a dos opciones

datos_final$sd_edad[datos_final$sd_edad == "18–24"] <- "18–24 años"
datos_final$sd_edad[datos_final$sd_edad == "35–49"] <- "35–49 años"


# variable sd_estudios: unificar mayúsculas
datos_final$sd_estudios[datos_final$sd_estudios == "Bachillerato / Formación profesional"] <- 
  "Bachillerato / Formación Profesional"


# variable sd_laboral: corregir typos y agrupar rarezas en "Otro" 
datos_final$sd_laboral[datos_final$sd_laboral == "Ama casa"] <- "Ama de casa"
# Agrupar las categorías minoritarias ambiguas
datos_final$sd_laboral[datos_final$sd_laboral %in% c(
  "Ama de casa", "Estudiante y prácticas ", "Estudio y trabajo ", "Lo"
)] <- "Otro"


#variable viaje_compania: agrupar combinaciones ambiguas en "Otro"
datos_final$viaje_compania[datos_final$viaje_compania %in% c(
  "Amigos y familia ", "Con amigos, En familia", "Contigo bebe", "En familia y con amigos"
)] <- "Otro"



#variable viaje_tipo_destino: agrupar rarezas en "Otro"
datos_final$viaje_tipo_destino[datos_final$viaje_tipo_destino %in% c(
  "gastronómico", "Mix", "Mixto. Playa, montaña y cultural",
  "Playa, Rural", "Tanto playa como urbano", "Todos", "Una mezcla de todo"
)] <- "Otro"


# variable viaje_transporte: agrupar rarezas en "Otro"
datos_final$viaje_transporte[datos_final$viaje_transporte %in% c(
  "Avion coche y tren", "uber"
)] <- "Otro"


# viaje_alojamiento: mover "Hotel o apartamento" a "Otro" 
# Pero antes eliminamos los espacios raros que hay en alguna de las opciones de otros
datos_final$viaje_alojamiento <- trimws(datos_final$viaje_alojamiento)
datos_final$viaje_alojamiento[datos_final$viaje_alojamiento %in% c(
  "Hotel o apartamento", "Camping", "Hostel / albergue"
)] <- "Otro"


#variable viaje_antelacion: corregir typo
datos_final$viaje_antelacion[datos_final$viaje_antelacion == "Ente 2 semans y 1 mes"] <- 
  "Entre 2 semanas y 1 mes"


# variable rrss_plataforma: agrupar respuestas múltiples en "Múltiples plataformas" ---
datos_final$rrss_plataforma[datos_final$rrss_plataforma %in% c(
  "Google / Páginas web especializadas, Tripadvisor u otras plataformas de reseñas",
  "Instagram, TikTok, Blogs de viaje"
)] <- "Múltiples plataformas"


# Verificar que todo quedó limpio
table(datos_final$sd_estudios)
table(datos_final$sd_laboral)
table(datos_final$viaje_compania)
table(datos_final$viaje_tipo_destino)
table(datos_final$viaje_transporte)
table(datos_final$viaje_alojamiento)
table(datos_final$viaje_antelacion)
table(datos_final$rrss_plataforma)


# ============================================================
# GUARDAR EL DATASET FINAL
# ============================================================

# Guardamos datos_final en un archivo .rds (formato nativo de R, preserva tipos)
saveRDS(datos_final, "datos/datos_final.rds")

# Comprobar que se guardó
file.exists("datos/datos_final.rds")
View(datos_final)

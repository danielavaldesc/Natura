
###############################################################################
###############################################################################
## Análisis descriptivo de la encuesta segmentado POR GÉNERO (Cali)         ##
###############################################################################
###############################################################################

library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

########################
## Ruta y cargar datos 
########################

# Ruta y cargar datos
setwd("C:\\Users\\Portatil\\Desktop\\Natura\\")
dataset <- readxl::read_excel("Output\\base_Cali_2025.xlsx") %>%
  janitor::clean_names()

########################
## Variable de control
########################

# Seleccionar variable para segmentar
dataset$genero <- dataset$p40

# Recodificación 1
dataset$genero[dataset$genero %in%
                 c("Otras identidades de género",
                   "Prefiere no responder")] = "Otros"


########################
## Funciones
########################
source("Analisis\\funcion_0.R", encoding = "UTF-8")

# Carpeta de salida
dir.create("Output/Cali/Output_by_genero", 
           recursive = TRUE, showWarnings = FALSE)

########################
## Modulos
########################

# Modulo 1
var_m1 = 



##-------------------------------------##
## 4) Prueba de normalidad univariada  ##
##-------------------------------------##

continuas <- c("p1Edad", "p18", "p18_p1","p18_p2","p18_p3","p18_p4",
               "p24", "p32")

normalidad <- normality(
  data = dataset, continuous_vars = continuas,
  output_tex  = "Output/Cali/Output_by_genero/normalidad.tex",
  output_xlsx = "Output/Cali/Output_by_genero/normalidad.xlsx"
)

##-------------------------------------------------##
## 5) Descriptivo de continuas (POR GÉNERO)       ##
##-------------------------------------------------##

descriptivas.continuas <- cont_descriptive_gender(
  data           = dataset,
  control        = c("total","female","male"),
  continuous_vars = continuas,
  output_tex      = "Output/Cali/Output_by_genero/descriptiva.continuas.tex",
  output_xlsx     = "Output/Cali/Output_by_genero/descriptiva.continuas.xlsx"
)

##-------------------------------------------------##
## 6) Descriptivo de categóricas (POR GÉNERO)     ##
##-------------------------------------------------##

cat <- c("p17", "genero", "Edadr", "p3", "p5", "p7", "p9Estrato",
         "p10", "p22", "p23", "p25", "p26", "p31")

descriptivas.discretas <- cat_descriptive_gender(
  data       = dataset,
  cat_vars   = cat,
  output_tex  = "Output/Cali/Output_by_genero/descriptiva.discretas.tex",
  output_xlsx = "Output/Cali/Output_by_genero/descriptiva.discretas.xlsx"
)

##---------------------------------------##
## 7) Kruskal-Wallis (incluye género)    ##
##---------------------------------------##
# Mantengo tu exclusión de las descomposiciones de p18
kruskal <- kruskal_test(
  data       = dataset,
  continuas  = continuas[!continuas %in% c("p18","p18_p1","p18_p2","p18_p3","p18_p4")],
  # Probamos por género y, adicionalmente, por modo y otras discretas si las quieres en el reporte
  discretas  = c("genero","p17","Edadr","p3","p5","p7","p9Estrato","p10","p22","p23","p25","p26","p31"),
  output_tex  = "Output/Cali/Output_by_genero/Kruskal.tex",
  output_xlsx = "Output/Cali/Output_by_genero/Kruskal.xlsx"
)

##-------------------------------------------------##
## 8) Chi-cuadrado (asociaciones entre discretas) ##
##-------------------------------------------------##
# OJO: tu función chi2_test usa el objeto global `cat`. Por eso lo dejamos seteado.
chi2 <- chi2_test(
  data       = dataset,
  discretas  = cat,
  output_tex  = "Output/Cali/Output_by_genero/Chi_2.tex",
  output_xlsx = "Output/Cali/Output_by_genero/Chi_2.xlsx"
)

##--------------------------------------------------##
## 9) Organizar salida del resumen descriptivo      ##
##--------------------------------------------------##

library(dplyr)

summary_cont <- descriptivas.continuas %>%
  dplyr::filter(!Variable %in% c("p18","p18_p1","p18_p2","p18_p3","p18_p4"))

# Etiquetas amistosas (si esas variables existen)
summary_cont$Variable[summary_cont$Variable == "p24"]      <- "Satisfacción del modo (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_1"] <- "Costo compra (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_2"] <- "Costo de uso (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_3"] <- "Confort (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_4"] <- "Tiempo de viaje (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_5"] <- "Riesgo de delincuencia (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_6"] <- "Riesgo de acoso (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_7"] <- "Riesgo de discriminación (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_8"] <- "Contaminación (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_9"] <- "Nivel de siniestralidad (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p32"]      <- "Nivel de contaminación propio (Likert-5)"

# Columnas esperadas: Variable, total, female, male
colnames(summary_cont)[1:4] <- c("Variable","Total","Femenino","Masculino")

summary_cat <- descriptivas.discretas
summary_cat$Variable[summary_cat$Variable == "genero"]    <- "Género"
summary_cat$Variable[summary_cat$Variable == "Edadr"]     <- "Edad"
summary_cat$Variable[summary_cat$Variable == "p3"]        <- "Etnia"
summary_cat$Variable[summary_cat$Variable == "p5"]        <- "Nivel de educación"
summary_cat$Variable[summary_cat$Variable == "p7"]        <- "Actividad principal"
summary_cat$Variable[summary_cat$Variable == "p9Estrato"] <- "Estrato"
summary_cat$Variable[summary_cat$Variable == "p10"]       <- "Valor transporte"
summary_cat$Variable[summary_cat$Variable == "p22"]       <- "Distancia promedio"
summary_cat$Variable[summary_cat$Variable == "p23"]       <- "Propósito de viaje"
summary_cat$Variable[summary_cat$Variable == "p25"]       <- "Razón de elección"
summary_cat$Variable[summary_cat$Variable == "p26"]       <- "Menos gusto en el modo"
summary_cat$Variable[summary_cat$Variable == "p31"]       <- "Contaminación"

colnames(summary_cat)[1:4] <- c("Variable","Total","Femenino","Masculino")

summary_all <- dplyr::bind_rows(summary_cont, summary_cat)
writexl::write_xlsx(summary_all, "Output/Cali/Output_by_genero/Summary.xlsx")

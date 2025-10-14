###############################################################################
###############################################################################
## Análisis descriptivo de la encuesta según modos de transporte de Medellin ##
###############################################################################
###############################################################################

# Definir ruta de trabajo:
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")

# Cargar datos:
dataset = readxl::read_excel("Output\\base_Med_2025.xlsx")

# Cargar funciones auxiliares
source("Analisis\\funcion_0.R", encoding = "UTF-8")

##-------------------------------------##
## 1. Caracterización: tablas cruzadas ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------##

cross_tab(
  data = dataset,
  row_var = "p1Edad",
  col_var = "genero",
  caption = "Tabla 1: proporción (%) de participantes diferenciados según sexo y grupos etarios",
  tex_path = "Output/Medellin/Output_by_mode/edad_sexo.tex",
  xlsx_path = "Output/Medellin/Output_by_mode/edad_sexo.xlsx"
)

cross_tab(
  data = dataset,
  row_var = "p1Edad",
  col_var = "p17",
  caption = "Tabla 2: proporción (%) de participantes diferenciados según modo de transporte y grupos etarios",
  tex_path = "Output/Medellin/Output_by_mode/modo_edad.tex",
  xlsx_path = "Output/Medellin/Output_by_mode/modo_edad.xlsx"
)

cross_tab(
  data = dataset,
  row_var = "p17",
  col_var = "p9Estrato",
  caption = "Tabla 3: proporción (%) de participantes diferenciados según modo de transporte y estrato socioeconómico",
  tex_path = "Output/Medellin/Output_by_mode/modo_estrato.tex",
  xlsx_path = "Output/Medellin/Output_by_mode/modo_estrato.xlsx"
)

##-------------------------------------##
## 3. Prueba de normalidad univariada  ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------##

# Definición de variables continuas
continuas <- c("p1Edad", "p18", "p18_p1",	"p18_p2",	"p18_p3",	"p18_p4",
               "p24", "p32")

# Prueba de normalidad
normalidad <- normality(
  data = dataset, continuous_vars = continuas,
  output_tex = "Output/Medellin/Output_by_mode/normalidad.tex",
  output_xlsx = "Output/Medellin/Output_by_mode/normalidad.xlsx"
)


##-------------------------------------------------##
## 3. Resumen descriptivo: variables continuas     ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------------##

# Definición de variables continuas
descriptivas.continuas <- cont_descriptive(dataset,
                                           control = c("total", "activos", "auto", "moto", "app_auto", "app_moto",
                                                       "publico_formal", "pub_inf_auto", "pub_inf_moto"),
                                           continuous_vars = continuas,
                                           output_tex = "Output/Medellin/Output_by_mode/descriptiva.continuas.tex",
                                           output_xlsx = "Output/Medellin/Output_by_mode/descriptiva.continuas.xlsx")

##-------------------------------------------------##
## 4. Resumen descriptivo: variables categóricas   ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------------##

cat <- c("genero", "Edadr", "p3", "p5", "p7", "p9Estrato",
         "p10", "p22", "p23", "p25", "p26", "p31")

descriptivas.discretas = cat_descriptive(data = dataset,
                                         cat_vars = cat,
                                         output_tex = "Output/Medellin/Output_by_mode/descriptiva.discretas.tex",
                                         output_xlsx = "Output/Medellin/Output_by_mode/descriptiva.discretas.xlsx")


##---------------------------------------##
## 5. Prueba de Kruskal-Wallis           ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##---------------------------------------##
kruskal = kruskal_test(data = dataset,
                       continuas = continuas[!continuas %in% c("p18", "p18_p1",	"p18_p2",	"p18_p3",	"p18_p4")],
                       discretas = c("p17", cat),
                       output_tex = "Output/Medellin/Output_by_mode/Kruskal.tex",
                       output_xlsx = "Output/Medellin/Output_by_mode/Kruskal.xlsx")

##-------------------------------------------------##
## 6. Resumen descriptivo: variables categóricas   ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------------##

chi2 = chi2_test(data = dataset,
                 discretas = c("p17", cat),
                 output_tex = "Output/Medellin/Output_by_mode/Chi_2.tex",
                 output_xlsx = "Output/Medellin/Output_by_mode/Chi_2.xlsx")

##--------------------------------------------------##
## Organizar salida del resumen descriptivo         ##
##--------------------------------------------------##

# Ajustar nombres:
summary_cont = descriptivas.continuas %>% dplyr::filter(!Variable %in% c("p18", "p18_p1",	
                                                                         "p18_p2",	"p18_p3",	
                                                                         "p18_p4"))

summary_cont$Variable[summary_cont$Variable == "p24"] = "Satisfacción del modo (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_1"] = "Costo compra (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_2"] = "Costo de uso (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_3"] = "Confort (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_4"] = "Tiempo de  viaje (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_5"] = "Riesgo de delincuencia (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_6"] = "Riesgo de acoso (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_7"] = "Riesgo de discriminación (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_8"] = "Contaminación (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p28p28_9"] = "Nivel de siniestralidad (Likert-5)"
summary_cont$Variable[summary_cont$Variable == "p32"] = "Nivel de contaminación propio (Likert-5)"
colnames(summary_cont) = c("Variable", "Total","activos", 
                           "auto", "moto", "app_auto", "app_moto",
                           "publico_formal", "pub_inf_auto", "pub_inf_moto")

# Ajustar nombres:
summary_cat = descriptivas.discretas
summary_cat$Variable[summary_cat$Variable == "genero"] = "Género"
summary_cat$Variable[summary_cat$Variable == "Edadr"] = "Edad"
summary_cat$Variable[summary_cat$Variable == "p3"] = "Etnia"
summary_cat$Variable[summary_cat$Variable == "p5"] = "Nivel de educación"
summary_cat$Variable[summary_cat$Variable == "p7"] = "Actividad principal"
summary_cat$Variable[summary_cat$Variable == "p9Estrato"] = "Estarto"
summary_cat$Variable[summary_cat$Variable == "p10"] = "Valor transporte"
summary_cat$Variable[summary_cat$Variable == "p22"] = "Distancia promedio"
summary_cat$Variable[summary_cat$Variable == "p23"] = "Propósito de viaje"
summary_cat$Variable[summary_cat$Variable == "p25"] = "Razón de elección"
summary_cat$Variable[summary_cat$Variable == "p26"] = "Menos gusto en el modo"
summary_cat$Variable[summary_cat$Variable == "p31"] = "Contaminación"

colnames(summary_cat) = c("Variable", "Total","activos", 
                          "auto", "moto", "app_auto", "app_moto",
                          "publico_formal", "pub_inf_auto", "pub_inf_moto")

# Base de datos completa (resumen descriptivo)
summary_all = bind_rows(summary_cont, summary_cat)

writexl::write_xlsx(summary_all, "Output/Medellin/Output_by_mode/Summary.xlsx")
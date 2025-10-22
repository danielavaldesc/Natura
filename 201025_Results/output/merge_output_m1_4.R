
# Agregar tablas de resumen descriptivo
setwd("C:/Users/sergio.barona/Desktop/Natura/201025_Results/")

# Módulo 1
tablam1_gender = readxl::read_excel("output/m1/21102025_v2_mod1_by_gender.xlsx")
tablam1_ses = readxl::read_excel("output/m1/21102025_mod1_by_ses.xlsx")
tablam1_travel = readxl::read_excel("output/m1/21102025_mod1_by_travel_mode.xlsx")

tabla_m1 = merge(tablam1_gender, tablam1_ses, by = c("Variable", "Label",
                                                     "Categoria"))

mod1 = merge(tabla_m1, tablam1_travel, by = c("Variable", "Label",
                                                     "Categoria"))
# Módulo 2
tablam2_gender = readxl::read_excel("output/m2/21102025_mod2_by_gender.xlsx")
tablam2_ses = readxl::read_excel("output/m2/21102025_mod2_by_ses.xlsx")
tablam2_travel = readxl::read_excel("output/m2/21102025_mod2_by_travel_mode.xlsx")

tabla_m2 = merge(tablam2_gender, tablam2_ses, by = c("Variable", "Label",
                                                     "Categoria"))

mod2 = merge(tabla_m2, tablam2_travel, by = c("Variable", "Label",
                                                  "Categoria"))
# Módulo 3
tablam3_gender = readxl::read_excel("output/m3/21102025_mod3_by_gender.xlsx")
tablam3_ses = readxl::read_excel("output/m3/21102025_mod3_by_ses.xlsx")
tablam3_travel = readxl::read_excel("output/m3/21102025_mod3_by_travel_mode.xlsx")

tabla_m3 = merge(tablam3_gender, tablam3_ses, by = c("Variable", "Label",
                                                     "Categoria"))

mod3 = merge(tabla_m3, tablam3_travel, by = c("Variable", "Label",
                                                  "Categoria"))

# Módulo 4
tablam4_gender = readxl::read_excel("output/m4/21102025_mod4_by_gender.xlsx")
tablam4_ses = readxl::read_excel("output/m4/21102025_mod4_by_ses.xlsx")
tablam4_travel = readxl::read_excel("output/m4/21102025_mod4_by_travel_mode.xlsx")

tabla_m4 = merge(tablam4_gender, tablam4_ses, by = c("Variable", "Label",
                                                     "Categoria"))

mod4 = merge(tabla_m4, tablam4_travel, by = c("Variable", "Label",
                                                  "Categoria"))


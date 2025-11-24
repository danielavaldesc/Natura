##############################################
## MNL Cali – Especificación final           ##
## Modelos: con comunas / sin comunas        ##
## Fecha: 09/11/2025                         ##
##############################################

# --------- Librerías ----------
library(dplyr)
library(readxl)
library(tidyr)
library(tibble)
library(forcats)
library(nnet)
library(stargazer)
library(writexl)
library(stringr)

# --------- Datos ----------
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")
input <- read_excel("201025_Results_Cali\\output\\input_famd_cali_29102025.xlsx")

# --------- Continuas (sin p36/p37) ----------
continuas <- c(
  "p24",
  "p28_importancia_costo_compra",
  "p28_importancia_costo_uso",
  "p28_importancia_comodidad",
  "p28_importancia_tiempo",
  "p28_importancia_riesgo_robo",
  "p28_importancia_riesgo_acoso",
  "p28_importancia_discriminacion",
  "p28_importancia_emisiones",
  "p28_importancia_siniestralidad",
  "tiempo_total"
)

# --------- Ensamble base categórica ----------
categoricas <- setdiff(names(input), c(continuas, "id", "medio"))
df.cat <- input %>% dplyr::select(any_of(categoricas)) %>% mutate(across(everything(), as.factor))
df.mnl <- cbind(df.cat, input[c("id","medio",continuas)])

# --------- Unificación propósito (p23_agr5) ----------
df.mnl <- df.mnl %>%
  mutate(
    p23_agr5 = fct_collapse(
      p23_agregado,
      "Trabajo"          = c("Trabajo"),
      "Compras/Trámites" = c("Compras y trámites","Compras y tr\u00e1mites"),
      "Tiempo personal"  = c("Recreación, salud y actividades personales",
                             "Recreaci\u00f3n, salud y actividades personales",
                             "Visitas sociales"),
      "Estudio"          = "Estudio",
      "Cuidado"          = c(
        "Cuidado y familia (centro educativo, niños/as o jóvenes)",
        "Cuidado y familia (otro lugar, niños/as o jóvenes)",
        "Cuidado y familia (persona con discapacidad)",
        "Cuidado y familia (persona enferma)",
        "Cuidado y familia (recreación, niños)",
        "Cuidado y familia (salud, niños)",
        "Cuidado y familia (recreaci\u00f3n, ni\u00f1as/os)",
        "Cuidado y familia (salud, ni\u00f1as/os)"
      ),
      "Otros"            = "Otro"
    ) %>% fct_drop()
  ) %>%
  filter(p23_agr5 != "Otros") %>%
  mutate(p23_agr5 = factor(as.character(p23_agr5)))

df.mnl <- df.mnl %>%
  filter(p5_agregado != "Sin respuesta",
         p40 %in% c("Hombre","Mujer")) %>%
  mutate(p40 = factor(as.character(p40)))

df.mnl <- df.mnl %>%
  filter(p7_agregado != "Otro") %>%
  mutate(p7_agregado = factor(as.character(p7_agregado)))

# Étnico binario
df.mnl <- df.mnl %>%
  mutate(
    aut_rec_etnico = case_when(
      p3_agregado %in% c("Población afrodescendiente","Pueblos indígenas") ~ "Si",
      TRUE ~ "No"
    ),
    aut_rec_etnico = factor(aut_rec_etnico, levels = c("No","Si"))
  )

# Educación 3 cat
df.mnl <- df.mnl %>%
  mutate(
    educ_3cat = case_when(
      p5_agregado == "Primaria o menos" ~ "Primaria o menos",
      p5_agregado == "Secundaria" ~ "Secundaria",
      p5_agregado %in% c("Superior","Técnico / Tecnológico") ~ "Terciaria",
      TRUE ~ NA_character_
    ),
    educ_3cat = factor(educ_3cat, levels = c("Primaria o menos","Secundaria","Terciaria"))
  )

# Situación laboral
df.mnl <- df.mnl %>%
  mutate(
    sitlab = dplyr::case_when(
      p7_agregado == "Ocupado/a" ~ "Asalariado o independiente",
      p7_agregado == "Trabajo doméstico no remunerado" ~ "Trabajo doméstico no remunerado",
      p7_agregado %in% c("Desocupado o inactivo","Estudiante") ~ "Desocupado o inactivo",
      TRUE ~ NA_character_
    ),
    sitlab = factor(sitlab,
                    levels = c("Asalariado o independiente","Trabajo doméstico no remunerado","Desocupado o inactivo"))
  )

# Tenencia autos / motos
df.mnl <- df.mnl %>%
  mutate(
    ten_autos = case_when(
      p15_autos_agregado == "Sin autos" ~ "Sin autos",
      p15_autos_agregado %in% c("1 auto","2 o más autos") ~ "1 auto o más",
      TRUE ~ NA_character_
    ),
    ten_autos = factor(ten_autos, levels = c("Sin autos","1 auto o más")),
    ten_motos = case_when(
      p16_motos_agregado == "Sin motocicletas" ~ "Sin motocicletas",
      p16_motos_agregado %in% c("1 motocicleta","2 o más motocicletas") ~ "1 motocicleta o más",
      TRUE ~ NA_character_
    ),
    ten_motos = factor(ten_motos, levels = c("Sin motocicletas","1 motocicleta o más"))
  )

# Distancia 3 grupos
df.mnl <- df.mnl %>%
  mutate(
    dist_recod = case_when(
      p22 %in% c("Menos de 1 km","Entre 1 y 3 km") ~ "Menos de 3 km",
      p22 %in% c("Entre 4 y 7 km","Entre 8 y 12 km") ~ "Entre 4 y 12 km",
      p22 %in% c("Más de 12 km") ~ "Más de 12 km",
      TRUE ~ NA_character_
    ),
    dist_recod = factor(dist_recod, levels = c("Menos de 3 km","Entre 4 y 12 km","Más de 12 km"))
  )

# --------- Outcome ---------
df.mnl$medio <- relevel(factor(df.mnl$medio), ref = "Moto privada")

# --------- Fórmulas (base y con comunas) ----------
form_base <- as.formula(
  medio ~
    p24 + p28_importancia_costo_compra + p28_importancia_costo_uso +
    p28_importancia_comodidad + p28_importancia_tiempo + p28_importancia_riesgo_robo +
    p28_importancia_riesgo_acoso + p28_importancia_discriminacion +
    p28_importancia_emisiones + p28_importancia_siniestralidad +
    tiempo_total +
    edad_r2 + aut_rec_etnico + educ_3cat + sitlab + p40 + p38p38_dummy
)

form_con <- update(form_base, . ~ . + p19comuna)  

# --------- Estimaciones ----------
mnl.ctrl2_con <- multinom(form_con, data = df.mnl, trace = FALSE)
mnl.ctrl2_sin <- multinom(form_base, data = df.mnl, trace = FALSE)

# --------- Función OR + IC95% + z + p ----------
or_table <- function(model) {
  sm  <- summary(model)
  b   <- sm$coefficients
  se  <- sm$standard.errors
  cf  <- b %>% as.data.frame() %>% rownames_to_column("categoria_medio") %>%
    pivot_longer(-categoria_medio, names_to = "termino", values_to = "estimate")
  sf  <- se %>% as.data.frame() %>% rownames_to_column("categoria_medio") %>%
    pivot_longer(-categoria_medio, names_to = "termino", values_to = "std.error")
  out <- left_join(cf, sf, by = c("categoria_medio","termino")) %>%
    mutate(
      z = estimate / std.error,
      p = 2 * pnorm(abs(z), lower.tail = FALSE),
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96*std.error),
      CI_high = exp(estimate + 1.96*std.error)
    ) %>%
    mutate(
      OR = round(OR, 3), CI_low = round(CI_low, 3), CI_high = round(CI_high, 3),
      z  = round(z, 3),  p      = round(p, 4)
    ) %>%
    arrange(categoria_medio, termino)
  list(long = out, matrix = exp(coef(model)) %>% as.data.frame() %>% round(3))
}

or_con <- or_table(mnl.ctrl2_con)
or_sin <- or_table(mnl.ctrl2_sin)

# --------- Exportar ----------
out_dir <- "201025_Results_Cali\\MNL"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Stargazer
stargazer(mnl.ctrl2_con, type = "html",
          title = "MNL (mnl.ctrl2) – CON comunas (Cali)",
          single.row = TRUE, na.replace = "",
          out = file.path(out_dir, "mnl_ctrl2_CONcomunas_Cali_stargazer.html"))
stargazer(mnl.ctrl2_sin, type = "html",
          title = "MNL (mnl.ctrl2) – SIN comunas (Cali)",
          single.row = TRUE, na.replace = "",
          out = file.path(out_dir, "mnl_ctrl2_SINcomunas_Cali_stargazer.html"))

# Excel OR
writexl::write_xlsx(
  list(
    "CON_OR_largo"   = or_con$long,
    "CON_OR_matriz"  = or_con$matrix,
    "SIN_OR_largo"   = or_sin$long,
    "SIN_OR_matriz"  = or_sin$matrix
  ),
  path = file.path(out_dir, "mnl_ctrl2_OR_CONySIN_Cali.xlsx")
)

cat("\n✅ Resultados guardados en:\n", normalizePath(out_dir), "\n",
    "- mnl_ctrl2_CONcomunas_Cali_stargazer.html\n",
    "- mnl_ctrl2_SINcomunas_Cali_stargazer.html\n",
    "- mnl_ctrl2_OR_CONySIN_Cali.xlsx\n", sep = "")

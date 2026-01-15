# =========================
# NATURA – FPE (Cali y Medellín)
# Cruces: motivo–género–estrato–tiempo
# =========================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ---------- 1) Rutas ----------
path_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/input_famd_cali_29102025.xlsx"
path_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/input_famd_med_29102025.xlsx"

# ---------- 2) Limpieza mínima ----------
clean_gender <- function(x){
  x <- str_squish(as.character(x))
  x <- str_to_title(x)
  case_when(
    str_detect(x, "^Hombre$|^Masculino$") ~ "Hombre",
    str_detect(x, "^Mujer$|^Femenino$")   ~ "Mujer",
    TRUE ~ x
  )
}

clean_strata <- function(x){
  x <- str_squish(as.character(x))
  x <- str_to_title(x)
  x <- case_when(
    str_detect(x, "Bajo")  ~ "Bajo",
    str_detect(x, "Medio") ~ "Medio",
    str_detect(x, "Alto")  ~ "Alto",
    TRUE ~ x
  )
  factor(x, levels = c("Bajo", "Medio", "Alto"))
}

clean_motive <- function(x){
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\\s+", " ")
  x
}

# ---------- 3) Lectura (usa nombres EXACTOS) ----------
read_city <- function(path, city){
  read_excel(path) %>%
    mutate(
      ciudad = city,
      p40 = clean_gender(p40),                 # género
      p9_estrato3 = clean_strata(p9_estrato3), # estrato
      p23_agregado = clean_motive(p23_agregado), # motivo
      tiempo_total = as.numeric(tiempo_total)  # tiempo total
    ) %>%
    filter(p40 %in% c("Hombre", "Mujer")) %>%
    filter(!is.na(p23_agregado), p23_agregado != "") %>%
    filter(!is.na(tiempo_total), tiempo_total >= 0)
}

cali <- read_city(path_cali, "Cali")
med  <- read_city(path_med,  "Medellín")
base_all <- bind_rows(cali, med)

# ---------- 4) Resúmenes de tiempo (función) ----------
summ_time <- function(df, group_cols){
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n = n(),
      promedio = mean(tiempo_total, na.rm = TRUE),
      mediana  = median(tiempo_total, na.rm = TRUE),
      p25      = quantile(tiempo_total, 0.25, na.rm = TRUE, type = 7),
      p75      = quantile(tiempo_total, 0.75, na.rm = TRUE, type = 7),
      p90      = quantile(tiempo_total, 0.90, na.rm = TRUE, type = 7),
      .groups  = "drop"
    )
}

# ---------- 5) (A) Motivo × Género (por ciudad) ----------
motivo_genero <- base_all %>%
  count(ciudad, p40, p23_agregado, name = "n") %>%
  group_by(ciudad, p40) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p40, desc(pct))

# ---------- 6) (B) Motivo × Tiempo (por ciudad y género) ----------
tiempo_motivo_genero <- summ_time(base_all, c("ciudad", "p23_agregado", "p40")) %>%
  arrange(ciudad, p23_agregado, p40)

# ---------- 7) (C) Motivo × Estrato (por ciudad) ----------
motivo_estrato <- base_all %>%
  count(ciudad, p9_estrato3, p23_agregado, name = "n") %>%
  group_by(ciudad, p9_estrato3) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p9_estrato3, desc(pct))

# ---------- 8) (D) Motivo × Género × Estrato (interseccional) ----------
motivo_genero_estrato <- base_all %>%
  count(ciudad, p40, p9_estrato3, p23_agregado, name = "n") %>%
  group_by(ciudad, p40, p9_estrato3) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p40, p9_estrato3, desc(pct))

# ---------- 9) (E) Tiempo por ciudad × género (para el párrafo) ----------
tiempo_ciudad_genero <- summ_time(base_all, c("ciudad", "p40"))

# ---------- 10) Exportar ----------
out_path <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Outputs/cruces_ideam_genero_estrato_motivo_tiempo.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  list(
    "tiempo_ciudad_genero" = tiempo_ciudad_genero,
    "motivo_x_genero" = motivo_genero,
    "tiempo_x_motivo_x_genero" = tiempo_motivo_genero,
    "motivo_x_estrato" = motivo_estrato,
    "motivo_x_genero_x_estrato" = motivo_genero_estrato
  ),
  path = out_path
)

message("✅ Listo. Exporté: ", out_path)

# ---------- 11) Chequeos rápidos ----------
base_all %>% count(ciudad)
base_all %>% count(ciudad, p40)
base_all %>% count(ciudad, p9_estrato3)
base_all %>% count(ciudad, p23_agregado) %>% arrange(ciudad, desc(n))

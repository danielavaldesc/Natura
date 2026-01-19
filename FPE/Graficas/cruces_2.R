# =========================
# NATURA – FPE (Cali y Medellín)
# Cruces (tablas cruzadas):
# 1) P22 (distancia) × Género
# 2) P22 × Género × Ocupación (p7)
# 3) P22 × Género × Ocupación agregada (p7_agregado)
# 4) P21 (número de destinos) × Género
# =========================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ---------- 1) Rutas ----------
path_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Cali 2025_V01_Cliente.xlsx"
path_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Medellin 2025_Cliente.xlsx"

stopifnot(file.exists(path_cali))
stopifnot(file.exists(path_med))

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

clean_cat <- function(x){
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\\s+", " ")
  x
}

# ---------- 3) Lectura por ciudad (SOLO columnas necesarias) ----------
read_city <- function(path, city){
  read_excel(path, sheet = excel_sheets(path)[1]) %>%
    select(any_of(c("p40", "p7", "p21", "p22"))) %>%
    mutate(
      ciudad = city,
      p40 = clean_gender(p40),
      p7  = clean_cat(p7),
      p21 = clean_cat(p21),
      p22 = clean_cat(p22)
    ) %>%
    filter(p40 %in% c("Hombre", "Mujer"))
}

cali <- read_city(path_cali, "Cali")
med  <- read_city(path_med,  "Medellín")

base_all <- bind_rows(cali, med)

# ---------- 4) p7 agregada (tu clasificación exacta) ----------
base_all <- base_all %>%
  mutate(
    p7_agregado = case_when(
      p7 %in% c("Trabajar", "Trabajar y estudiar") ~ "Ocupado/a",
      p7 %in% c("Está desempleado",
                "Es pensionado(a)",
                "Incapacitado permanente para trabajar") ~ "Desocupado o inactivo",
      p7 == "Estudiar" ~ "Estudiante",
      p7 == "Hacer trabajo doméstico en su propio hogar" ~ "Trabajo doméstico no remunerado",
      p7 == "Otra actividad" ~ "Otro",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# TABLA 1) P22 × Género (por ciudad)  -> n y %
# ============================================================
p22_genero <- base_all %>%
  filter(!is.na(p22), p22 != "") %>%
  count(ciudad, p40, p22, name = "n") %>%
  group_by(ciudad, p40) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p40, p22)

# ============================================================
# TABLA 2) P22 × Género × Ocupación (p7) -> n y %
# ============================================================
p22_genero_p7 <- base_all %>%
  filter(!is.na(p22), p22 != "") %>%
  filter(!is.na(p7), p7 != "") %>%
  count(ciudad, p40, p7, p22, name = "n") %>%
  group_by(ciudad, p40, p7) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p7, p40, p22)

# ============================================================
# TABLA 3) P22 × Género × Ocupación agregada (p7_agregado) -> n y %
# ============================================================
p22_genero_p7_agregado <- base_all %>%
  filter(!is.na(p22), p22 != "") %>%
  filter(!is.na(p7_agregado)) %>%
  count(ciudad, p40, p7_agregado, p22, name = "n") %>%
  group_by(ciudad, p40, p7_agregado) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p7_agregado, p40, p22)

# ============================================================
# TABLA 4) P21 (destinos) × Género (por ciudad) -> n y %
# ============================================================
p21_genero <- base_all %>%
  filter(!is.na(p21), p21 != "") %>%
  count(ciudad, p40, p21, name = "n") %>%
  group_by(ciudad, p40) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  arrange(ciudad, p40, p21)

# ---------- 5) Exportar ----------
out_path <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Outputs/cruces_p21_p22_genero_ocupacion.xlsx"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  list(
    "p22_x_genero" = p22_genero,
    "p22_x_genero_x_p7" = p22_genero_p7,
    "p22_x_genero_x_p7_agregado" = p22_genero_p7_agregado,
    "p21_x_genero" = p21_genero
  ),
  path = out_path
)

message("✅ Listo. Exporté: ", out_path)

# ---------- 6) Chequeos rápidos ----------
base_all %>% count(ciudad)
base_all %>% count(ciudad, p40)

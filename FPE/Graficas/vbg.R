# ============================================================
# Violencias (dummy) x MOTIVO DE VIAJE (p23_agregado) x GÉNERO
# ============================================================

library(readxl)
library(tidyverse)
library(stringr)
library(scales)

# ---- 1) Rutas ----
path_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Cali 2025_V01_Cliente.xlsx"
path_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Medellin 2025_Cliente.xlsx"

out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/violencias"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 2) Paleta ----
col_hombre <- "#3B66FF"
col_mujer  <- "#B9B9D3"

# ---- 3) Lista oficial de opciones P39 ----
opciones_p39 <- c(
  "Mientras caminaba",
  "En los paraderos o estaciones",
  "En el metro",
  "En el metrocable",
  "En el tranvía",
  "En el metroplus",
  "En uno de los alimentadores del metro",
  "En una buseta o colectivo",
  "En un bicitaxi",
  "En un mototaxi",
  "En un jeep (guala)",
  "En un bus de transporte intermunicipal",
  "En un taxi",
  "En un vehículo de aplicación (Uber/Cabify)",
  "En el bus del trabajo",
  "En una motocicleta",
  "En un motoratón",
  "Iba en su bicicleta",
  "En un vehículo particular",
  "Otro, cuál?"
)

# ---- 4) Helpers ----
std_genero <- function(x) {
  x <- str_squish(as.character(x))
  case_when(
    x %in% c("Hombre","hombre","HOMBRE","Masculino","masculino","M","m",1,"1") ~ "Hombre",
    x %in% c("Mujer","mujer","MUJER","Femenino","femenino","F","f",2,"2")     ~ "Mujer",
    TRUE ~ NA_character_
  )
}

norm_txt <- function(x) {
  x %>%
    str_squish() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("’|´|`", "'")
}

# ---- 5) Leer ciudad (SOLO p40 y p39) ----
read_city <- function(path, ciudad_nombre) {
  read_excel(path) %>%
    transmute(
      ciudad = ciudad_nombre,
      genero = std_genero(p40),
      p39 = as.character(p39)
    )
}

df <- bind_rows(
  read_city(path_cali, "Cali"),
  read_city(path_med,  "Medellín")
) %>%
  filter(!is.na(genero))

# ---- 6) Denominador: total H/M por ciudad (igual que antes) ----
# OJO: esto incluye a TODA la muestra. Si prefieres denom = solo víctimas, te lo cambio.
den <- df %>%
  group_by(ciudad, genero) %>%
  summarise(denom_total = n(), .groups = "drop")

# ---- 7) Expandir selección múltiple y QUITAR NAs (los "no les pasó") ----
df_p39_long <- df %>%
  mutate(
    p39 = if_else(is.na(p39), "", p39),
    p39 = str_replace_all(p39, "\\r\\n|\\n|\\r", ";"),
    p39 = str_replace_all(p39, "\\|", ";")
    # si tu base usa coma como separador, activa:
    # p39 = str_replace_all(p39, ",", ";")
  ) %>%
  # ✅ aquí eliminamos los que no respondieron P39 (típicamente "No les pasó")
  filter(str_squish(p39) != "") %>%
  separate_rows(p39, sep = ";") %>%
  mutate(p39 = norm_txt(p39)) %>%
  filter(p39 != "")

# ---- 8) Mapear variantes a categorías oficiales ----
df_p39_long <- df_p39_long %>%
  mutate(
    lugar = case_when(
      str_detect(str_to_lower(p39), "mientras camin") ~ "Mientras caminaba",
      str_detect(str_to_lower(p39), "paraderos|estacion") ~ "En los paraderos o estaciones",
      str_detect(str_to_lower(p39), "\\ben el metro\\b") ~ "En el metro",
      str_detect(str_to_lower(p39), "metrocable") ~ "En el metrocable",
      str_detect(str_to_lower(p39), "tranv") ~ "En el tranvía",
      str_detect(str_to_lower(p39), "metroplus") ~ "En el metroplus",
      str_detect(str_to_lower(p39), "alimentador") ~ "En uno de los alimentadores del metro",
      str_detect(str_to_lower(p39), "buseta|colectivo") ~ "En una buseta o colectivo",
      str_detect(str_to_lower(p39), "bicitaxi") ~ "En un bicitaxi",
      str_detect(str_to_lower(p39), "mototaxi") ~ "En un mototaxi",
      str_detect(str_to_lower(p39), "jeep|guala") ~ "En un jeep (guala)",
      str_detect(str_to_lower(p39), "intermunicipal") ~ "En un bus de transporte intermunicipal",
      str_detect(str_to_lower(p39), "\\ben un taxi\\b") ~ "En un taxi",
      str_detect(str_to_lower(p39), "uber|cabify|aplicaci") ~ "En un vehículo de aplicación (Uber/Cabify)",
      str_detect(str_to_lower(p39), "bus del trabajo") ~ "En el bus del trabajo",
      str_detect(str_to_lower(p39), "motocic") ~ "En una motocicleta",
      str_detect(str_to_lower(p39), "motorat") ~ "En un motoratón",
      str_detect(str_to_lower(p39), "bicicleta") ~ "Iba en su bicicleta",
      str_detect(str_to_lower(p39), "veh[ií]culo particular") ~ "En un vehículo particular",
      str_detect(str_to_lower(p39), "^otro|otro,") ~ "Otro, cuál?",
      TRUE ~ p39
    )
  ) %>%
  # ✅ por si algo quedara NA por alguna razón
  filter(!is.na(lugar), str_squish(lugar) != "")

# ---- 9) Conteos y prevalencia ----
p39_resumen <- df_p39_long %>%
  group_by(ciudad, genero, lugar) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(den, by = c("ciudad","genero")) %>%
  mutate(pct = 100 * n / denom_total)

# ---- 10) AGRUPAR "chorrero": TOP N + Otros lugares ----
top_n <- 8  # <-- ajusta: 6, 8, 10 según qué tan largo lo quieras

top_lugares <- p39_resumen %>%
  group_by(ciudad, lugar) %>%
  summarise(p = mean(pct), .groups = "drop") %>%
  group_by(ciudad) %>%
  arrange(desc(p), .by_group = TRUE) %>%
  slice_head(n = top_n) %>%
  ungroup() %>%
  pull(lugar) %>%
  unique()

p39_resumen2 <- p39_resumen %>%
  mutate(
    lugar2 = if_else(lugar %in% top_lugares, lugar, "Otros lugares")
  ) %>%
  group_by(ciudad, genero, lugar2) %>%
  summarise(
    n = sum(n),
    denom_total = first(denom_total),
    pct = sum(pct),      # suma de % porque comparten el mismo denominador
    .groups = "drop"
  )

# Orden de lugares (por promedio)
orden_lugares <- p39_resumen2 %>%
  group_by(lugar2) %>%
  summarise(p = mean(pct), .groups = "drop") %>%
  arrange(desc(p)) %>%
  pull(lugar2)

p39_resumen2 <- p39_resumen2 %>%
  mutate(
    lugar2 = factor(lugar2, levels = orden_lugares),
    genero = factor(genero, levels = c("Hombre","Mujer")),
    ciudad = factor(ciudad, levels = c("Cali","Medellín"))
  )

write_csv(p39_resumen2, file.path(out_dir, "tabla_p39_donde_ocurrio_TOPyOTROS_por_genero_ciudad.csv"))

# ---- 11) Gráfica por ciudad ----
plot_city_p39 <- function(ciudad_sel) {
  
  d <- p39_resumen2 %>% filter(ciudad == ciudad_sel)
  
  p <- ggplot(d, aes(x = lugar2, y = pct, fill = genero)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = paste0(round(pct, 1), "%")),
      position = position_dodge(width = 0.8),
      vjust = -0.35,
      size = 4.2,
      fontface = "bold"
    ) +
    scale_fill_manual(values = c("Hombre" = col_hombre, "Mujer" = col_mujer)) +
    scale_y_continuous(limits = c(0, 100), labels = percent_format(scale = 1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, 26)) +
    labs(
      title = paste0("¿Dónde ocurrió? (P39) — ", ciudad_sel),
      x = NULL,
      y = "Porcentaje (%)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.margin = margin(b = 8),
      legend.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 13, margin = margin(b = 8)),
      axis.text.x = element_text(size = 11, angle = 25, hjust = 1, vjust = 1),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(t = 12, r = 12, b = 40, l = 14)
    )
  
  out_file <- file.path(out_dir, paste0("p39_donde_ocurrio_", tolower(ciudad_sel), ".png"))
  ggsave(out_file, p, width = 14, height = 8, dpi = 300)
  message("Guardado: ", out_file)
  p
}

plot_city_p39("Cali")
plot_city_p39("Medellín")


##################################
## p3838 

library(readxl)
library(tidyverse)
library(stringr)
library(scales)

std_genero <- function(x) {
  x <- str_squish(as.character(x))
  case_when(
    x %in% c("Hombre","hombre","HOMBRE","Masculino","masculino","M","m",1,"1") ~ "Hombre",
    x %in% c("Mujer","mujer","MUJER","Femenino","femenino","F","f",2,"2")     ~ "Mujer",
    TRUE ~ NA_character_
  )
}

# 1) Leer ciudad (incluye P38 en columnas)
read_city_p38 <- function(path, ciudad_nombre) {
  read_excel(path) %>%
    mutate(
      ciudad = ciudad_nombre,
      genero = std_genero(p40),
      id = row_number()   # si no tienes un ID único en la base
    ) %>%
    select(ciudad, genero, id, starts_with("p38p38_"))
}

df_p38 <- bind_rows(
  read_city_p38(path_cali, "Cali"),
  read_city_p38(path_med,  "Medellín")
) %>%
  filter(!is.na(genero))

# 2) Denominador: total H/M por ciudad (toda la muestra)
den <- df_p38 %>%
  group_by(ciudad, genero) %>%
  summarise(denom_total = n_distinct(id), .groups = "drop")

# 3) Pasar P38 a formato largo
p38_long <- df_p38 %>%
  pivot_longer(
    cols = starts_with("p38p38_"),
    names_to = "tipo_var",
    values_to = "resp"
  ) %>%
  mutate(
    # adapta esto a tu codificación real:
    # si resp viene como "Sí/No", o 1/0, o TRUE/FALSE
    yes = case_when(
      resp %in% c(1, "1", "Sí", "Si", "SI", "sí", "si", TRUE) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# 4) Etiquetas (ajusta según el cuestionario)
map_tipo <- c(
  "p38p38_1"  = "Comentarios incómodos o discriminación",
  "p38p38_2"  = "Miradas morbosas al cuerpo",
  "p38p38_3"  = "Piropos obscenos/ofensivos (carácter sexual)",
  "p38p38_4"  = "Recargaron el cuerpo sin consentimiento (incomodidad)",
  "p38p38_5"  = "Le hicieron sentir miedo",
  "p38p38_6"  = "Tocamientos/manoseo sin consentimiento",
  "p38p38_7"  = "Robo o atraco",
  "p38p38_99" = "Otro (99)"
)

# 5) Prevalencia: % de personas (no de respuestas)
p38_prev <- p38_long %>%
  filter(yes) %>%
  group_by(ciudad, genero, tipo_var) %>%
  summarise(n_personas = n_distinct(id), .groups = "drop") %>%
  left_join(den, by = c("ciudad","genero")) %>%
  mutate(
    tipo = recode(tipo_var, !!!map_tipo),
    pct = 100 * n_personas / denom_total
  )

p38_prev

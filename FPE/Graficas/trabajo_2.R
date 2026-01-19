# ============================================================
# SCRIPT R — Trabajo (2)
# ============================================================

library(tidyverse)
library(readxl)
library(forcats)
library(stringr)
library(scales)

# Para exportar a Excel:
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

# -----------------------------
# 1) Rutas de entrada
# -----------------------------
input_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Cali 2025_V01_Cliente.xlsx"
input_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/BD Base Movilidad Medellin 2025_Cliente.xlsx"

stopifnot(file.exists(input_cali))
stopifnot(file.exists(input_med))

# -----------------------------
# 2) Carpeta de salida
# -----------------------------
out_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Graficas/trabaj_remu"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 3) Paletas
# -----------------------------
colores_genero <- c(
  "Hombre" = "#1F3A5F",
  "Mujer"  = "#4A90C2"
)

colores_cuidado <- c(
  "Cuidado remunerado"    = "#4B1D70",
  "Cuidado no remunerado" = "#A678D3"
)

# -----------------------------
# 3.1) Tema grande (CLAVE)
# -----------------------------
tema_grande <- theme_minimal(base_size = 15) +
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text    = element_text(size = 15, face = "bold"),
    axis.text     = element_text(size = 13),
    axis.title    = element_text(size = 14),
    legend.text   = element_text(size = 13),
    legend.title  = element_text(size = 14)
  )

# -----------------------------
# 4) Cargar mínimo necesario
# -----------------------------
prep_min <- function(path_xlsx, ciudad_label) {
  df <- read_excel(path_xlsx)
  
  required_cols <- c("p40", "p7", "p7_1")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste0("Faltan columnas en ", ciudad_label, ": ", paste(missing, collapse = ", ")))
  }
  
  df %>%
    transmute(
      ciudad = ciudad_label,
      p40  = as.character(p40),
      p7   = str_squish(as.character(p7)),
      p7_1 = str_squish(as.character(p7_1))
    ) %>%
    mutate(
      genero = case_when(
        p40 == "Hombre" ~ "Hombre",
        p40 == "Mujer"  ~ "Mujer",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(genero), !is.na(p7))
}

datos <- bind_rows(
  prep_min(input_cali, "Cali"),
  prep_min(input_med,  "Medellín")
) %>%
  mutate(
    genero = fct_relevel(genero, "Hombre", "Mujer"),
    ciudad = factor(ciudad, levels = c("Cali", "Medellín"))
  )

# -----------------------------
# 5) Clasificar cuidado
# -----------------------------
datos <- datos %>%
  mutate(
    cuidado_tipo = case_when(
      (p7 == "2" | str_to_lower(p7) == "trabajar") &
        !is.na(p7_1) &
        (p7_1 == "1" | str_to_lower(p7_1) %in% c("empleado(a) doméstico", "empleado(a) domestico")) ~
        "Cuidado remunerado",
      
      (p7 == "4" |
         str_detect(str_to_lower(p7), "hacer trabajo doméstico en su propio hogar") |
         str_detect(str_to_lower(p7), "hacer trabajo domestico en su propio hogar")) ~
        "Cuidado no remunerado",
      
      TRUE ~ NA_character_
    )
  )

# ============================================================
# TABLAS
# ============================================================
denom <- datos %>%
  count(ciudad, genero, name = "total_genero_ciudad")

num <- datos %>%
  filter(!is.na(cuidado_tipo)) %>%
  count(ciudad, genero, cuidado_tipo, name = "n_cuidado")

tabla_pct_del_total <- num %>%
  left_join(denom, by = c("ciudad", "genero")) %>%
  mutate(pct_del_total_genero = 100 * n_cuidado / total_genero_ciudad)

tabla_zoom <- datos %>%
  filter(!is.na(cuidado_tipo)) %>%
  count(ciudad, cuidado_tipo, genero, name = "n") %>%
  group_by(ciudad, cuidado_tipo) %>%
  mutate(pct_dentro_cuidado = 100 * n / sum(n)) %>%
  ungroup()

# -----------------------------
# 6) Exportar Excel
# -----------------------------
out_xlsx <- file.path(out_dir, "tablas_cuidado_por_ciudad.xlsx")
wb <- createWorkbook()

addWorksheet(wb, "Pct_del_total_genero")
writeData(wb, "Pct_del_total_genero", tabla_pct_del_total)

addWorksheet(wb, "Zoom_dentro_cuidado")
writeData(wb, "Zoom_dentro_cuidado", tabla_zoom)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

# ============================================================
# GRÁFICO A — % del total por género
# ============================================================
p_A <- ggplot(tabla_pct_del_total,
              aes(x = genero, y = pct_del_total_genero, fill = cuidado_tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(
    aes(label = paste0(round(pct_del_total_genero, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 4.5
  ) +
  facet_wrap(~ ciudad) +
  scale_fill_manual(values = colores_cuidado) +
  labs(
    title = "Trabajo de cuidado como % del total de hombres y mujeres",
    subtitle = "Base: total de personas en cada género (por ciudad)",
    x = NULL, y = "Porcentaje (%)",
    fill = NULL
  ) +
  tema_grande +
  theme(legend.position = "top")

ggsave(
  filename = file.path(out_dir, "fig_A_pct_del_total_genero_por_ciudad.png"),
  plot = p_A,
  width = 13.5, height = 7, dpi = 300
)

# ============================================================
# GRÁFICO B — Zoom dentro del cuidado (100%)
# ============================================================

p_B <- ggplot(
  tabla_zoom,
  aes(
    x = cuidado_tipo,
    y = pct_dentro_cuidado / 100,
    fill = genero
  )
) +
  geom_col(width = 0.7, color = "white") +
  geom_text(
    aes(
      label = paste0(round(pct_dentro_cuidado, 1), "%"),
      color = genero            # 👈 clave: color por género
    ),
    position = position_stack(vjust = 0.5),
    size = 4.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_wrap(~ ciudad) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colores_genero) +
  scale_color_manual(          # 👈 colores del TEXTO
    values = c(
      "Hombre" = "white",       # azul oscuro → texto blanco
      "Mujer"  = "black"        # azul claro → texto negro
    )
  ) +
  labs(
    title = "Zoom: composición por género dentro del trabajo de cuidado",
    subtitle = "Base: personas clasificadas en cada tipo de cuidado (por ciudad)",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  tema_grande +
  theme(
    legend.position = "top",
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = file.path(out_dir, "fig_B_zoom_genero_dentro_cuidado_por_ciudad.png"),
  plot = p_B,
  width = 13.5,
  height = 7,
  dpi = 300
)

# -----------------------------
# 7) Mensaje final
# -----------------------------
message(
  "Listo ✅ Salidas guardadas en:\n", out_dir,
  "\n\nExcel:",
  "\n- ", out_xlsx,
  "\n\nFiguras:",
  "\n- fig_A_pct_del_total_genero_por_ciudad.png",
  "\n- fig_B_zoom_genero_dentro_cuidado_por_ciudad.png"
)


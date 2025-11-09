#############################################
# Correlaciones + Heatmap (Cali)            #
#############################################

# ---- Librerías ----
library(dplyr)
library(readxl)
library(tidyverse)
library(GGally)
library(writexl)
library(ggplot2)
library(stringr)
library(tibble)
library(tidyr)

# ---- Paths ----
output_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/201025_Results_Cali/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---- Cargar base ----
dataset <- read_excel("C:/Users/danie/OneDrive/Escritorio/Natura/201025_Results_Cali/output/input_famd_cali_29102025.xlsx")

# Limpieza mínima (coherente con Medellín)
dataset <- subset(dataset, !(p40 %in% c("Otro", "Prefiere no responder", "Otras identidades de género")))

# ---- Variables continuas del modelo (mismas que Medellín) ----
vars_continuas_nombres <- c(
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
  "p32_contaminacion_likert",
  "p36_influencia_amigos",
  "p37_influencia_familia",
  "tiempo_total",
  "p1edad"
)

vars_continuas <- dataset %>%
  dplyr::select(any_of(vars_continuas_nombres)) %>%
  mutate(across(everything(), as.numeric))

# --------- Etiquetas legibles para el gráfico ----------
pretty_labels <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_replace("^p1edad$", "Edad") %>%
    str_replace("^tiempo total$", "Tiempo total") %>%
    str_replace("^p32 contaminacion likert$", "Contaminación (Likert)") %>%
    str_replace("^p36 influencia amigos$", "Influencia amigos") %>%
    str_replace("^p37 influencia familia$", "Influencia familia") %>%
    str_replace("^p28 importancia ", "Imp. ")
}
vars_continuas_plot <- vars_continuas
names(vars_continuas_plot) <- pretty_labels(names(vars_continuas_plot))

# ---- Matriz de correlación (continuas) ----
matriz_corr_cont <- cor(vars_continuas, use = "pairwise.complete.obs", method = "pearson")

# ---- Heatmap (continuas) con labels ajustados ----
p_heat_cont <- GGally::ggcorr(
  vars_continuas_plot,
  method = c("everything", "pearson"),
  label = TRUE,
  label_round = 2,
  label_size = 2.8,
  layout.exp = 1
) +
  ggtitle("Correlaciones (Pearson) - Variables Continuas - Cali") +
  theme(
    plot.title  = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 9)
  ) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

print(p_heat_cont)
ggsave(
  filename = file.path(output_dir, "heatmap_correlaciones_continuas_cali.png"),
  plot = p_heat_cont,
  width = 14, height = 10, dpi = 300
)

# ---- Función tidy del triángulo superior ----
upper_tri_tidy <- function(cor_mat) {
  as.data.frame(cor_mat) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
    filter(var1 < var2) %>%          # triángulo superior
    mutate(abs_r = abs(r)) %>%
    arrange(desc(abs_r))
}

# ---- Pares |r| >= 0.5 (continuas) ----
pares_cont <- upper_tri_tidy(matriz_corr_cont) %>%
  filter(abs_r >= 0.5)

# ---- Correlaciones: TODAS las numéricas (incluye dummies) ----
vars_numericas <- dataset %>% dplyr::select(where(is.numeric))

if (ncol(vars_numericas) >= 2) {
  matriz_corr_full <- cor(vars_numericas, use = "pairwise.complete.obs", method = "pearson")
  pares_full <- upper_tri_tidy(matriz_corr_full) %>%
    filter(abs_r >= 0.5)
} else {
  matriz_corr_full <- tibble()
  pares_full <- tibble(var1 = character(), var2 = character(), r = numeric(), abs_r = numeric())
}

# ---- Exportar a Excel ----
mat_cont_df <- as.data.frame(matriz_corr_cont) %>% rownames_to_column("variable")
mat_full_df <- if (ncol(vars_numericas) >= 2) {
  as.data.frame(matriz_corr_full) %>% rownames_to_column("variable")
} else {
  tibble()
}

writexl::write_xlsx(
  list(
    "continuas_matriz"          = mat_cont_df,
    "continuas_pares_|r|>=0.5"  = pares_cont,
    "todas_matriz"              = mat_full_df,
    "todas_pares_|r|>=0.5"      = pares_full
  ),
  path = file.path(output_dir, "correlaciones_cali.xlsx")
)

# ---- Mensajes ----
message("✅ Heatmap: ", file.path(output_dir, "heatmap_correlaciones_continuas_cali.png"))
message("✅ Excel:   ", file.path(output_dir, "correlaciones_cali.xlsx"))
if (nrow(pares_cont) == 0) message("ℹ️ Sin pares continuos con |r| ≥ 0.5.")
if (nrow(pares_full) == 0) message("ℹ️ Sin pares (todas numéricas) con |r| ≥ 0.5.")

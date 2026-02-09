# ============================================================
# Violencias (dummy) x modo (medio) x género (p40)
# ============================================================

library(readxl)
library(tidyverse)

# ---- 1) Rutas ----
path_cali <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/input_famd_cali_29102025.xlsx"
path_med  <- "C:/Users/danie/OneDrive/Escritorio/Natura/FPE/Input/input_famd_med_29102025.xlsx"

# ---- 2) Leer bases (solo variables usadas) ----
cali <- read_excel(path_cali) %>%
  select(p38p38_dummy, p40, medio) %>%
  mutate(ciudad = "Cali")

med <- read_excel(path_med) %>%
  select(p38p38_dummy, p40, medio) %>%
  mutate(ciudad = "Medellín")

dataset <- bind_rows(cali, med)

# ---- 3) Estandarizar variables ----
dataset <- dataset %>%
  mutate(
    medio = str_squish(as.character(medio)),
    
    genero = case_when(
      p40 %in% c("Hombre","hombre","HOMBRE","Masculino","masculino","M","m",1,"1") ~ "Hombre",
      p40 %in% c("Mujer","mujer","MUJER","Femenino","femenino","F","f",2,"2")     ~ "Mujer",
      TRUE ~ NA_character_
    ),
    
    violencia = case_when(
      p38p38_dummy %in% c(1,"1",TRUE,"Si","Sí","si","sí") ~ 1,
      p38p38_dummy %in% c(0,"0",FALSE,"No","no","No sabe","NO SABE","no sabe") ~ 0,
      TRUE ~ NA_real_
    )
  )

# ---- 4) Tabla de prevalencia ----
umbral_n <- 30

violencia_modo <- dataset %>%
  filter(!is.na(violencia), !is.na(genero), !is.na(medio), medio != "") %>%
  group_by(ciudad, genero, medio) %>%
  summarise(
    prevalencia_pct = mean(violencia) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= umbral_n)

print(violencia_modo)

# ---- 5) Ordenar modos por prevalencia promedio (para lectura) ----
orden_medios <- violencia_modo %>%
  group_by(medio) %>%
  summarise(preval_prom = mean(prevalencia_pct), .groups = "drop") %>%
  arrange(desc(preval_prom)) %>%
  pull(medio)

violencia_modo <- violencia_modo %>%
  mutate(
    medio = factor(medio, levels = orden_medios),
    genero = factor(genero, levels = c("Hombre", "Mujer")),
    ciudad = factor(ciudad, levels = c("Cali", "Medellín"))
  )

# ---- 6) Gráfica con mejor separación de ciudades ----
ggplot(violencia_modo, aes(x = medio, y = prevalencia_pct, fill = genero)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(round(prevalencia_pct, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 5,            # 👈 MÁS GRANDES los números (antes 3)
    fontface = "bold"    # 👈 más legibles
  ) +
  facet_grid(ciudad ~ ., scales = "fixed", switch = "y") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("Mujer" = "#8E44AD", "Hombre" = "#3498DB")) +
  labs(
    title = "Prevalencia de violencias basadas en género por modo de transporte",
    subtitle = paste0("Personas que reportan al menos una experiencia (n ≥ ", umbral_n, " por grupo)"),
    x = "Modo de transporte (medio)",
    y = "Porcentaje (%)",
    fill = NULL,
    caption = "Fuente: Encuesta de movilidad – Cali y Medellín"
  ) +
  theme_minimal(base_size = 15) +  # 👈 sube TODO el tamaño general (antes 12)
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13),   # 👈 leyenda más grande
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13),
    
    strip.text.y.left = element_text(face = "bold", angle = 0, size = 14), # 👈 "Cali/Medellín"
    strip.placement = "outside",
    panel.spacing.y = unit(1.8, "lines"),
    
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1),          # 👈 nombres de modos
    axis.text.y = element_text(size = 12),
    
    panel.grid.major.x = element_blank()
  )

ggsave("violencias_por_modo_genero_ciudad_filas.png", width = 12, height = 8, dpi = 300)

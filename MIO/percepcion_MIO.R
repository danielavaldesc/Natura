# =============================================================================
# GRÁFICAS MIO - NATURA CALI
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)

# -----------------------------------------------------------------------------
# 1. Rutas
# -----------------------------------------------------------------------------

input_path <- "C:/Users/danie/OneDrive/Escritorio/Natura/201025_Results_Cali/output/clean_cali_dataset_21102025.xlsx"

output_dir <- "C:/Users/danie/OneDrive/Escritorio/Natura/MIO/graficos"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# 2. Paleta de colores
# -----------------------------------------------------------------------------

paleta <- c(
  azul_1 = "#0d3a72",
  azul_2 = "#08284e",
  azul_3 = "#104a91",
  celeste = "#82ceec",
  gris = "#eceef0"
)

# -----------------------------------------------------------------------------
# 3. Cargar base
# -----------------------------------------------------------------------------

dataset <- read_excel(input_path)

# -----------------------------------------------------------------------------
# 4. Limpieza y recodificación
# -----------------------------------------------------------------------------

dataset <- dataset %>%
  dplyr::select(-dplyr::starts_with("p28_importancia_"))

dataset_limpio <- dataset %>%
  dplyr::mutate(
    p17_modo_agregado = dplyr::case_when(
      p17 %in% c("Caminata", "Bicicleta") ~ "Modo activo",
      p17 %in% c("Transporte público (MIO)", 
                 "Transporte público colectivo busetas") ~ "Transporte público",
      p17 %in% c("Taxi",
                 "Automóvil en plataforma (Ejemplo:Uber, Yango, Didi, InDriver)",
                 "Moto en plataforma (Ejemplo: Didi, Picap, Uber, otras)") ~ "Taxi / Plataforma",
      p17 %in% c("Automóvil", "Campero/ Camioneta (SUV)", 
                 "Van/Camioneta con platón") ~ "Auto privado",
      p17 %in% c("Moto 2T", "Moto 4T") ~ "Moto privada",
      p17 %in% c("Moto taxi (moto ratón)", "Guala o pirata",
                 "Bicitaxi con motor", "Bicitaxi sin motor") ~ "Transporte informal",
      p17 == "Camión / vehículo de carga" ~ "Vehículo pesado",
      TRUE ~ NA_character_
    ),
    
    p17_grupo_mio = dplyr::case_when(
      p17 == "Transporte público (MIO)" ~ "MIO",
      !is.na(p17) ~ "Otros modos",
      TRUE ~ NA_character_
    ),
    
    p24 = dplyr::case_when(
      as.character(p24) %in% c("1", "Nada Satisfecho") ~ 1,
      as.character(p24) %in% c("2", "Poco Satisfecho") ~ 2,
      as.character(p24) %in% c("3", "Satisfecho") ~ 3,
      as.character(p24) %in% c("4", "Muy Satisfecho") ~ 4,
      as.character(p24) %in% c("5", "Totalmente satisfecho") ~ 5,
      TRUE ~ NA_real_
    ),
    
    p24_label = factor(
      p24,
      levels = 1:5,
      labels = c(
        "Nada satisfecho",
        "Poco satisfecho",
        "Satisfecho",
        "Muy satisfecho",
        "Totalmente satisfecho"
      )
    ),
    
    p26_agregado = dplyr::case_when(
      p26 %in% c("El costo de compra", "El costo de uso u operación") ~ "Costo económico",
      p26 %in% c("El tiempo de espera", "El tiempo de viaje") ~ "Tiempo de viaje / espera",
      p26 %in% c("Las condiciones de incomodidad",
                 "La exposición a condiciones climáticas desfavorables (lluvia o calor)") ~ "Incomodidad / clima",
      p26 == "La falta de autonomía o control sobre el viaje" ~ "Falta de autonomía / control",
      p26 == "La percepción de inseguridad personal (robo o atraco, acoso o violencia de algún tipo)" ~ "Inseguridad personal",
      p26 == "La vulnerabilidad frente accidentes de tránsito" ~ "Riesgo de accidente",
      p26 == "El nivel de emisiones (contaminación)" ~ "Impacto ambiental",
      p26 == "Nada me disgusta" ~ "Nada le disgusta",
      p26 == "No sabe/ No responde" ~ "Sin respuesta",
      p26 == "Otra razón" ~ "Otro motivo",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate(
    razones_transporte = purrr::pmap_chr(
      dplyr::select(., p25, p25_1, p25_2, p25_3, p25_4),
      function(...) {
        x <- c(...)
        x <- x[!is.na(x)]
        x <- unique(x)
        paste(x, collapse = ", ")
      }
    ),
    p25_razones_agregadas = dplyr::na_if(trimws(razones_transporte), ""),
    
    situaciones_evitar = purrr::pmap_chr(
      dplyr::select(
        ., p27, p27_1, p27_2, p27_3, p27_4, p27_5, p27_6,
        p27_7, p27_8, p27_9, p27_10, p27_otro
      ),
      function(...) {
        x <- c(...)
        x <- x[!is.na(x)]
        x <- unique(x)
        paste(x, collapse = ", ")
      }
    ),
    p27_situaciones_multiples = dplyr::na_if(trimws(situaciones_evitar), "")
  ) %>%
  dplyr::mutate(
    p28_importancia_costo_compra = dplyr::case_when(
      as.character(p28p28_1) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_1) %in% c("2") ~ 2,
      as.character(p28p28_1) %in% c("3") ~ 3,
      as.character(p28p28_1) %in% c("4") ~ 4,
      as.character(p28p28_1) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_costo_uso = dplyr::case_when(
      as.character(p28p28_2) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_2) %in% c("2") ~ 2,
      as.character(p28p28_2) %in% c("3") ~ 3,
      as.character(p28p28_2) %in% c("4") ~ 4,
      as.character(p28p28_2) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_comodidad = dplyr::case_when(
      as.character(p28p28_3) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_3) %in% c("2") ~ 2,
      as.character(p28p28_3) %in% c("3") ~ 3,
      as.character(p28p28_3) %in% c("4") ~ 4,
      as.character(p28p28_3) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_tiempo = dplyr::case_when(
      as.character(p28p28_4) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_4) %in% c("2") ~ 2,
      as.character(p28p28_4) %in% c("3") ~ 3,
      as.character(p28p28_4) %in% c("4") ~ 4,
      as.character(p28p28_4) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_riesgo_robo = dplyr::case_when(
      as.character(p28p28_5) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_5) %in% c("2") ~ 2,
      as.character(p28p28_5) %in% c("3") ~ 3,
      as.character(p28p28_5) %in% c("4") ~ 4,
      as.character(p28p28_5) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_riesgo_acoso = dplyr::case_when(
      as.character(p28p28_6) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_6) %in% c("2") ~ 2,
      as.character(p28p28_6) %in% c("3") ~ 3,
      as.character(p28p28_6) %in% c("4") ~ 4,
      as.character(p28p28_6) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_discriminacion = dplyr::case_when(
      as.character(p28p28_7) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_7) %in% c("2") ~ 2,
      as.character(p28p28_7) %in% c("3") ~ 3,
      as.character(p28p28_7) %in% c("4") ~ 4,
      as.character(p28p28_7) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_emisiones = dplyr::case_when(
      as.character(p28p28_8) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_8) %in% c("2") ~ 2,
      as.character(p28p28_8) %in% c("3") ~ 3,
      as.character(p28p28_8) %in% c("4") ~ 4,
      as.character(p28p28_8) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    ),
    p28_importancia_siniestralidad = dplyr::case_when(
      as.character(p28p28_9) %in% c("1", "Nada Importante") ~ 1,
      as.character(p28p28_9) %in% c("2") ~ 2,
      as.character(p28p28_9) %in% c("3") ~ 3,
      as.character(p28p28_9) %in% c("4") ~ 4,
      as.character(p28p28_9) %in% c("5", "Muy importante") ~ 5,
      TRUE ~ NA_real_
    )
  )

# -----------------------------------------------------------------------------
# 5. TEMA VISUAL 
# -----------------------------------------------------------------------------

font_family <- "Aptos"

paleta <- c(
  azul_1 = "#0d3a72",
  azul_2 = "#08284e",
  azul_3 = "#104a91",
  celeste = "#82ceec",
  celeste_suave = "#cdeefa",
  gris = "#eceef0",
  fondo = "#ffffff"
)

colores_satisfaccion <- c(
  "Alta satisfacción" = "#08284e",
  "Satisfacción media" = "#104a91",
  "Baja satisfacción" = "#82ceec"
)

colores_importancia <- c(
  "#08284e", "#0d3a72", "#104a91", "#1b5a9e",
  "#2f6faf", "#328cc1", "#5fb7d9", "#82ceec", "#cdeefa"
)

tema_natura <- theme_minimal(base_family = font_family) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      color = paleta["azul_2"],
      margin = margin(b = 14)
    ),
    axis.text = element_text(
      color = paleta["azul_2"],
      size = 12
    ),
    axis.text.y = element_text(
      face = "bold"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      color = paleta["azul_2"],
      margin = margin(t = 8)
    ),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "#dfe5ea",
      linewidth = 0.35
    ),
    plot.background = element_rect(fill = paleta["fondo"], color = NA),
    panel.background = element_rect(fill = paleta["fondo"], color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = paleta["azul_2"]),
    plot.margin = margin(15, 25, 15, 15)
  )

guardar_grafico <- function(plot, nombre, w = 11, h = 6.5){
  ggsave(
    file.path(output_dir, nombre),
    plot,
    width = w,
    height = h,
    dpi = 300,
    bg = paleta["fondo"]
  )
}

wrap_txt <- function(x, width = 38) stringr::str_wrap(x, width)

# -----------------------------------------------------------------------------
# 6. P24
# -----------------------------------------------------------------------------

g_p24 <- dataset_limpio %>%
  filter(!is.na(p17_modo_agregado), !is.na(p24)) %>%
  mutate(
    satisfaccion_grupo = case_when(
      p24 %in% c(4, 5) ~ "Alta satisfacción",
      p24 == 3 ~ "Satisfacción media",
      p24 %in% c(1, 2) ~ "Baja satisfacción",
      TRUE ~ NA_character_
    ),
    satisfaccion_grupo = factor(
      satisfaccion_grupo,
      levels = c("Alta satisfacción", "Satisfacción media", "Baja satisfacción")
    )
  ) %>%
  count(p17_modo_agregado, satisfaccion_grupo) %>%
  group_by(p17_modo_agregado) %>%
  mutate(
    pct = n / sum(n),
    total_alta = sum(pct[satisfaccion_grupo == "Alta satisfacción"])
  ) %>%
  ungroup() %>%
  mutate(
    p17_modo_agregado = fct_reorder(p17_modo_agregado, total_alta)
  ) %>%
  ggplot(aes(x = p17_modo_agregado, y = pct, fill = satisfaccion_grupo)) +
  geom_col(width = 0.68, color = "white", linewidth = 0.45) +
  geom_text(
    aes(label = ifelse(pct >= 0.07, scales::percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = colores_satisfaccion,
    name = NULL
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Satisfacción por modo de transporte",
    x = NULL,
    y = "Porcentaje"
  ) +
  tema_natura +
  theme(
    legend.position = "bottom"
  )

guardar_grafico(g_p24, "p24.png", 11, 6.5)

# -----------------------------------------------------------------------------
# 7. P25
# -----------------------------------------------------------------------------

g_p25 <- dataset_limpio %>%
  filter(p17 == "Transporte público (MIO)") %>%
  select(p25, p25_1, p25_2, p25_3, p25_4) %>%
  pivot_longer(everything(), values_to = "razon") %>%
  filter(!is.na(razon), razon != "") %>%
  count(razon, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    razon = wrap_txt(razon)
  ) %>%
  ggplot(aes(x = fct_reorder(razon, pct), y = pct)) +
  geom_col(fill = paleta["azul_1"], width = 0.68) +
  geom_text(
    aes(label = scales::percent(pct, 0.1)),
    hjust = -0.15,
    fontface = "bold",
    size = 4,
    color = paleta["azul_2"]
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, .15))
  ) +
  labs(
    title = "Razones para usar el MIO",
    y = "Porcentaje"
  ) +
  tema_natura

guardar_grafico(g_p25, "p25.png", 11, 6.5)

# -----------------------------------------------------------------------------
# 8. P26
# -----------------------------------------------------------------------------

g_p26 <- dataset_limpio %>%
  filter(p17 == "Transporte público (MIO)", !is.na(p26_agregado)) %>%
  count(p26_agregado, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    p26_agregado = wrap_txt(p26_agregado)
  ) %>%
  ggplot(aes(x = fct_reorder(p26_agregado, pct), y = pct)) +
  geom_col(fill = paleta["azul_2"], width = 0.68) +
  geom_text(
    aes(label = scales::percent(pct, 0.1)),
    hjust = -0.15,
    fontface = "bold",
    size = 4,
    color = paleta["azul_2"]
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, .15))
  ) +
  labs(
    title = "Aspectos que menos gustan del MIO",
    y = "Porcentaje"
  ) +
  tema_natura

guardar_grafico(g_p26, "p26.png", 11, 6.5)

# -----------------------------------------------------------------------------
# 9. P27
# -----------------------------------------------------------------------------

g_p27 <- dataset_limpio %>%
  mutate(grupo = ifelse(p17 == "Transporte público (MIO)", "MIO", "Otros modos")) %>%
  pivot_longer(starts_with("p27"), values_to = "sit") %>%
  filter(!is.na(sit), sit != "") %>%
  count(grupo, sit) %>%
  group_by(grupo) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  group_by(sit) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(total)) %>%
  slice_head(n = 20) %>%
  mutate(sit = wrap_txt(sit, 35)) %>%
  ggplot(aes(x = fct_reorder(sit, pct), y = pct, fill = grupo)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.6
  ) +
  geom_text(
    aes(label = scales::percent(pct, 0.1)),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3.5,
    fontface = "bold",
    color = paleta["azul_2"]
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(
      "MIO" = "#08284e",          
      "Otros modos" = "#82ceec"   
    )
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, .25))
  ) +
  labs(
    title = "Situaciones evitadas",
    y = "Porcentaje"
  ) +
  tema_natura

guardar_grafico(g_p27, "p27.png", 12, 7)

# -----------------------------------------------------------------------------
# 10. P28
# -----------------------------------------------------------------------------

g_p28 <- dataset_limpio %>%
  filter(p17 == "Transporte público (MIO)") %>%
  select(starts_with("p28_importancia")) %>%
  pivot_longer(
    everything(),
    names_to = "factor",
    values_to = "val"
  ) %>%
  filter(!is.na(val)) %>%
  mutate(
    factor = recode(
      factor,
      p28_importancia_costo_compra = "Costo de compra",
      p28_importancia_costo_uso = "Costo de uso",
      p28_importancia_comodidad = "Comodidad",
      p28_importancia_tiempo = "Tiempo de viaje",
      p28_importancia_riesgo_robo = "Riesgo de robo",
      p28_importancia_riesgo_acoso = "Riesgo de acoso",
      p28_importancia_discriminacion = "Discriminación",
      p28_importancia_emisiones = "Emisiones",
      p28_importancia_siniestralidad = "Siniestralidad"
    )
  ) %>%
  group_by(factor) %>%
  summarise(prom = mean(val), .groups = "drop") %>%
  arrange(prom) %>%
  mutate(factor = factor(factor, levels = factor)) %>%
  ggplot(aes(x = factor, y = prom, fill = factor)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(
    aes(label = round(prom, 2)),
    hjust = -0.15,
    fontface = "bold",
    size = 4,
    color = paleta["azul_2"]
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = colores_importancia) +
  scale_y_continuous(
    limits = c(0, 5.5),
    expand = expansion(mult = c(0, .1))
  ) +
  labs(
    title = "Importancia de factores",
    y = "Promedio"
  ) +
  tema_natura

guardar_grafico(g_p28, "p28.png", 11, 6.5)
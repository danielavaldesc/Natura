
setwd("C:\\Users\\Portatil\\Desktop\\Natura\\201025_Results_Cali\\")
dataset = readxl::read_excel("output\\input_famd_cali_29102025.xlsx")
dataset$id = seq(from = 1, to = nrow(dataset),by = 1)

vars <- c(
  "id",
  "medio",
  "edad_r2",
  "p3_agregado",
  "p5_agregado",
  "p7_agregado",
  "p9_estrato3",
  "p12_dificultad_binaria",
  "p40",
  "p13",
  "p14",
  "p15_autos_agregado",
  "p16_motos_agregado",
  "p19comuna",
  "p22",
  "p23_agregado",
  "p32_contaminacion_likert",
  "p38p38_dummy",
  # "p39_lugar_agregado_mod",
  #"p1edad",
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
  "p36_influencia_amigos",
  "p37_influencia_familia",
  "tiempo_total"
)


library(dplyr)
df_items <- dataset %>%
  dplyr::select(all_of(vars)) %>% filter(p7_agregado != "Otro") %>%
  mutate(p7_agregado = as.factor(as.character(p7_agregado)))

# Variables continuas a categoricas
df_items <- df_items %>%
  mutate(
    tiempo_total_cat = cut(
      tiempo_total,
      breaks = c(-Inf, 15, 30, 60, 120, Inf),
      labels = c("≤15", "16-30", "31-60", "61-120", ">120"),
      right = TRUE,
      ordered_result = TRUE
    )
  ) %>% dplyr::select(-c("tiempo_total", "p19comuna"))


mca_data <- df_items %>%
  mutate(across(everything(), ~ as.factor(.x)))

library(forcats)

# Eliminar observaciones
mca_data = mca_data %>% filter(p5_agregado  != "Sin respuesta",
                               p40 %in% c("Hombre", "Mujer")) %>%
  mutate(p40 = as.factor(as.character(p40)),
         p5_agregado = as.factor(as.character(p5_agregado)))


# Agrupar
mca_data <- mca_data %>%
  mutate(
    p23_agr5 = fct_collapse(
      p23_agregado,
      "Trabajo"           = c("Trabajo"),
      "Compras/Trámites"  = c("Compras y trámites"),
      "Tiempo personal"   = c("Recreación, salud y actividades personales",
                              "Visitas sociales"),
      "Estudio"           = c("Estudio"),
      "Cuidado"     = c("Cuidado y familia (centro educativo, niños/as o jóvenes)",
                        "Cuidado y familia (otro lugar, niños/as o jóvenes)"),
      "Otros"     = c("Otro")
    ) %>% fct_drop()
  )

mca_data <- mca_data %>%
  mutate(across(everything(), ~ as.factor(.x)))

# Matriz de correlación 
library(FactoMineR)
library(factoextra)

res_mca <- MCA(mca_data %>% dplyr::select(-c("p23_agregado", "id", "medio")), graph = FALSE)

library(ggplot2)
library(scales)
library(patchwork)

# Tabla de eigenvalues del MCA
eig_df <- as.data.frame(res_mca$eig)
names(eig_df) <- c("eigenvalue","variance","cumulative")  # % en 2 y 3
eig_df$dim <- seq_len(nrow(eig_df))

# (opcional) mostrar solo las primeras k dimensiones
k <- min(38, nrow(eig_df))
plot_df <- eig_df[1:k, ]

g1 <- ggplot(plot_df, aes(x = dim, y = variance/100)) +
  geom_col(col = "black", fill = "dodgerblue3") +
  geom_text(aes(label = percent(variance/100, accuracy = 0.1)),
            vjust = -0.2, size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .1))) +
  labs(title = "MCA: Varianza explicada por dimensión",
       x = "Dimensión", y = "%") +
  theme_bw(base_size = 12)

g2 <- ggplot(plot_df, aes(x = dim, y = cumulative/100)) +
  geom_line(linewidth = 0.8, col = "dodgerblue3") +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = expansion(mult = c(0, .02))) +
  labs(title = "MCA: Varianza acumulada",
       x = "Dimensión", y = "%") +
  theme_bw(base_size = 12)

# Uno al lado del otro
g_comb <- g1 / g2

setwd("C:\\Users\\Portatil\\Desktop\\Natura\\081125_WorkingPaper\\")
ggsave(filename = "figures_wp/cali/varianza_mca.png", plot = g_comb, height = 12,
       width = 12)


library(paran)
library(recipes)

dataset = mca_data %>% dplyr::select(-c("p23_agregado", "id"))

rec <- recipe(medio ~ ., dataset) %>%
  step_dummy(all_nominal_predictors(), one_hot = T)

paran.dataset <- rec %>% prep() %>% juice() %>% as.data.frame() %>% dplyr::select(-medio)

cat.index <- 1:ncol(paran.dataset)

for (i in cat.index) {
  paran.dataset[,i] = scale(paran.dataset[,i]/sqrt(nrow(paran.dataset)/sum(paran.dataset[,i])),
                            center = T, scale = F)
}

# Método paralelo de Horn
set.seed(123)
paran <- paran::paran(paran.dataset, iterations = 5000, quietly = TRUE)

retained <- as.numeric(paran$Retained)

paran.df <- data.frame(
  Componente = 1:retained,
  `Autovalor ajustado` = round(paran$AdjEv[1:retained], 3),
  `Autovalor no ajustado` = round(paran$Ev[1:retained], 3),
  `Sesgo estimado` = round(paran$Bias[1:retained], 3)
)

# Tabla en formato LaTeX
writexl::write_xlsx(paran.df, "figures_wp/cali/paran.xlsx")



# Mapa sobre las variables
mca_var <- fviz_mca_var(
  res_mca,
  axes = c(1, 2),
  select.var = list(contrib = 20),    
  col.var = "contrib",                
  alpha.var = 0.9,                    
  pointsize = 5, labelsize = 3,   
  shape.var = 19,                    
  gradient.cols = c("#3B8CCB","#E6C229","#E4572E") # paleta perceptual
) +
  theme_bw(base_size = 12) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_x_continuous(expand = expansion(mult = c(.08,.08))) +
  scale_y_continuous(expand = expansion(mult = c(.08,.08)))


ggsave(filename = "figures_wp/cali/mca_var.png", plot = mca_var, height = 12/1.5,
       width = 12/1.5)



# Opción A: dejar que fviz filtre por cos2 (umbral)
mca_cor_fancy = fviz_mca_var(res_mca, choice = "mca.cor", 
                             repel = TRUE, # Avoid text overlapping (slow)
                             ggtheme = theme_bw())


ggsave("figures_wp/cali/mca_cor.png", mca_cor_fancy, width = 8.5, height = 7, dpi = 300)


library(patchwork)
library(purrr)

res_mca <- MCA(mca_data %>% select(-c("p23_agregado", "id", "medio")), ncp = 9, graph = FALSE)

max_axes <- ncol(res_mca$var$coord) 
dims <- seq_len(min(9, max_axes))

make_contrib_plot <- function(dim, top = 15, gap = -4) {
  fviz_contrib(res_mca, choice = "var", axes = dim, top = top) +
    theme_bw(base_size = 11) +
    coord_cartesian(clip = "off") + 
    theme(
      axis.line.x        = element_line(color = "grey25", linewidth = 0.5),
      axis.ticks.x       = element_line(color = "grey40"),
      axis.ticks.length  = unit(0, "pt"),                
      axis.text.x        = element_text(
        angle = 90, hjust = 1, vjust = 0.5,
        margin = margin(t = gap)      
      ),
      panel.grid.minor   = element_blank()
    ) +
    labs(title = paste0("Dim ", dim), x = NULL, y = "Contributions (%)")
}


plots <- map(dims, make_contrib_plot)
panel_contrib <- wrap_plots(plots, ncol = 3, guides = "collect") +
  plot_annotation(
    title = "MCA — Contribuciones de categorías por dimensión (Top 15)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("figures_wp/cali/mca_contrib_panel.png", plot = panel_contrib,
       width = 12, height = 14, units = "in", dpi = 300)




# Asegura factor y etiqueta NAs (opcional)
mca_data <- mca_data %>% mutate(medio = forcats::fct_explicit_na(medio, "Sin dato"))

# Mapa de individuos por 'medio' (Dim 1–2)
mca_ind_medio <- fviz_mca_ind(
  res_mca,
  axes       = c(1, 2),
  habillage  = mca_data$medio,   # <-- vector de grupos para colorear
  addEllipses= TRUE,              # elipses por grupo
  ellipse.type = "t",             # "t" o "norm"
  ellipse.level= 0.95,
  label      = "none",            # sin etiquetas por individuo
  alpha.ind  = 0.6,
  pointsize  = 1.8,
  legend.title = "Medio"
) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(title = "MCA — Individuos diferenciados por 'medio'",
       x = "Dim 1", y = "Dim 2")

# Guardar
ggsave("figures_wp/cali/mca_ind_medio.png", mca_ind_medio, width = 8, height = 6, dpi = 300)



library(plotly)
library(dplyr)
library(RColorBrewer)
library(htmlwidgets)

# 1) Coordenadas de individuos (Dim 1–3) y meta
coords <- as.data.frame(res_mca$ind$coord[, 1:3])
names(coords) <- c("Dim1","Dim2","Dim3")
coords$medio  <- mca_data$medio

# (opcional) quedarte con los mejor representados en 1–3:
# coords$cos2_123 <- rowSums(res_mca$ind$cos2[,1:3])
# coords <- coords %>% filter(cos2_123 >= 0.15)

# 2) % de varianza para títulos de ejes
eig <- res_mca$eig
ax1 <- paste0("Dim 1 (", round(eig[1,2], 1), "%)")
ax2 <- paste0("Dim 2 (", round(eig[2,2], 1), "%)")
ax3 <- paste0("Dim 3 (", round(eig[3,2], 1), "%)")

# 3) Paleta
ncol_m <- nlevels(coords$medio)
pal    <- brewer.pal(max(3, min(8, ncol_m)), "Dark2")

# 4) Dispersión 3D
fig <- plot_ly(
  data = coords,
  x = ~Dim1, y = ~Dim2, z = ~Dim3,
  color = ~medio, colors = pal,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3, opacity = 0.75)
) %>%
  layout(
    title = "MCA — Individuos por 'medio' (3D)",
    scene = list(
      xaxis = list(title = ax1, zeroline = TRUE),
      yaxis = list(title = ax2, zeroline = TRUE),
      zaxis = list(title = ax3, zeroline = TRUE),
      aspectmode = "data"
    ),
    legend = list(orientation = "h", y = -0.1)
  )


# 5) Guardar interactivo (HTML)
saveWidget(as_widget(fig), "figures_wp/cali/mca_ind_medio_3d.html", selfcontained = TRUE)

webshot2::webshot(
  "figures_wp/cali/mca_ind_medio_3d.html",
  "figures_wp/cali/mca_ind_medio_3d.png",
  vwidth = 1400, vheight = 1000
)



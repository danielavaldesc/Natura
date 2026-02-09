# Two pie charts (Cali vs Medellín) of "tiene licencia" vs "no tiene licencia",
# split by género, using the exact percentages you gave.
# Output is a single ggplot object similar to your slide style.

library(tidyverse)

# 1) Data (percentages)
df <- tribble(
  ~ciudad,      ~sexo,    ~licencia_pct,
  "Cali",       "Hombre", 67.0,
  "Cali",       "Mujer",  29.4,
  "Medellín",   "Hombre", 47.9,
  "Medellín",   "Mujer",  21.0
) %>%
  mutate(no_licencia_pct = 100 - licencia_pct) %>%
  pivot_longer(cols = c(licencia_pct, no_licencia_pct),
               names_to = "estado",
               values_to = "pct") %>%
  mutate(
    estado = recode(estado,
                    licencia_pct = "Tiene licencia",
                    no_licencia_pct = "No tiene licencia")
  )

# 2) Plot
p <- ggplot(df, aes(x = "", y = pct, fill = estado)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_grid(sexo ~ ciudad) +
  geom_text(
    aes(label = if_else(pct >= 8, paste0(format(pct, nsmall = 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3.5
  ) +
  scale_fill_manual(values = c("Tiene licencia" = "#2E6FEE", "No tiene licencia" = "#D9D9D9")) +
  labs(
    title = "Acceso a licencia de conducción (solo población ocupada)",
    subtitle = "Cali vs Medellín · Por género",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

p

ggsave("tortas_licencia_por_ciudad.png", p, width = 10, height = 6, dpi = 300)

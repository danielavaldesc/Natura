# -----------------------------------------------
# 2.plots.R
# Presentación general (barras/pies) + mapas de LAT/LON (Cali y Medellín)
# -----------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(janitor)
  library(ggplot2); library(sf); library(writexl); library(tidyr); library(purrr)
  library(forcats); library(scales)
})

# ==== CONFIG ====
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")
out_root <- "1.analisis_estadistico/plots"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# (Opcional) Fondo: shapefile departamentos (EPSG:4326). Si no está, solo puntos.
shp_deptos <- "spatialdata/dptos_col/departamentos.shp"
has_shp <- file.exists(shp_deptos)

# Cajas geográficas (EPSG:4326)
bbox_ciudad <- list(
  Cali     = list(xmin = -76.70, xmax = -76.42, ymin = 3.28, ymax = 3.55),
  Medellin = list(xmin = -75.70, xmax = -75.47, ymin = 6.16, ymax = 6.38)
)

# ==== THEME LIMPIO ====
theme_clean <- function(){
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(color = "grey30"),
      axis.title.x       = element_text(color = "grey30"),
      axis.title.y       = element_text(color = "grey30"),
      legend.position    = "right"
    )
}

# ==== HELPERS ====

pick_col <- function(df, patterns){
  pat <- paste0("(", paste(patterns, collapse="|"), ")")
  nm <- names(df)[grepl(pat, names(df), ignore.case = TRUE)][1]
  if (is.na(nm)) NULL else nm
}

detect_depto   <- function(df) pick_col(df, c("^p2_1$", "depto", "depart", "dpto"))
detect_mpio    <- function(df) pick_col(df, c("^p2_1_1$", "municip", "mpio"))
detect_genero  <- function(df) pick_col(df, c("^genero$","^sexo$","sexo"))
detect_estrato <- function(df) pick_col(df, c("^estrato$","estrat","^p9(_estrato)?$"))
# Transporte: forzamos P17 como primera opción
detect_modo    <- function(df) pick_col(df, c("^p17$","^p17_","modo","medio.*trans","transporte","^p6$","^p7(_1)?$"))

# lat/lon: p47Latitude / p47Longitude / variaciones
detect_lat <- function(df) pick_col(df, c("^p47.*lat","lat","latitude"))
detect_lon <- function(df) pick_col(df, c("^p47.*lon","lon","long","longitude"))

parse_num_locale <- function(x){
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "(?<=\\d)\\.(?=\\d{3}(\\D|$))", "") # miles
  x <- stringr::str_replace(x, ",", ".")                                # coma -> punto
  suppressWarnings(as.numeric(x))
}

# --- Tabla % ordenada (opcional: top N + "Otros") ---
tbl_pct <- function(x, top_n = Inf){
  tb <- tibble(cat = as.character(x)) |>
    filter(!is.na(cat), cat != "") |>
    count(cat, name = "n") |>
    mutate(pct = 100 * n / sum(n)) |>
    arrange(desc(pct))
  
  if (is.finite(top_n) && nrow(tb) > top_n) {
    head_tb <- slice_head(tb, n = top_n)
    tail_tb <- slice_tail(tb, n = nrow(tb) - top_n) |>
      summarise(cat = "Otros", n = sum(n), pct = sum(pct))
    tb <- bind_rows(head_tb, tail_tb) |> arrange(desc(pct))
  }
  tb
}

# --- Barras horizontales con etiquetas (sin n() en aes) ---
plot_bar_pct <- function(df, var, title, out_path, top_n = Inf){
  tb <- tbl_pct(df[[var]], top_n = top_n)
  if (!nrow(tb)) return(invisible(NULL))
  
  tb <- tb |>
    mutate(cat_f = forcats::fct_reorder(cat, pct),
           label = sprintf("%.1f%% (n=%s)", pct, format(n, big.mark=",")))
  
  g <- ggplot(tb, aes(y = cat_f, x = pct)) +
    geom_col(fill = "#4E79A7", width = 0.75) +
    geom_text(aes(label = label), hjust = -0.05, size = 3.4) +
    scale_x_continuous(labels = label_percent(accuracy = 1, scale = 1),
                       expand = expansion(mult = c(0, .15))) +
    labs(title = title, x = "% del total", y = NULL) +
    theme_clean()
  
  ggsave(out_path, g, width = 9, height = max(4, nrow(tb)*0.35), dpi = 220)
}

# --- Barras apiladas modo × género (precalcula todo) ---
plot_stacked <- function(df, var_x, var_fill, title, out_path){
  d <- df |>
    select(x = all_of(var_x), fill = all_of(var_fill)) |>
    filter(!is.na(x), x != "", !is.na(fill), fill != "")
  if (!nrow(d)) return(invisible(NULL))
  
  tb <- d |>
    count(x, fill, name = "n") |>
    group_by(x) |>
    mutate(pct = 100 * n / sum(n)) |>
    ungroup() |>
    mutate(x_f = forcats::fct_reorder(x, -pct, .fun = max))
  
  g <- ggplot(tb, aes(x = x_f, y = pct, fill = fill)) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
    labs(title = title, x = NULL, y = "% dentro de cada categoría", fill = NULL) +
    theme_clean()
  
  ggsave(out_path, g, width = 10, height = 6, dpi = 200)
}

# --- Pie (Top-10 + Otros) ---
plot_pie <- function(df, var, title, out_path, top_n = 10){
  tb <- tbl_pct(df[[var]], top_n = top_n)
  if (!nrow(tb)) return(invisible(NULL))
  tb <- tb |> mutate(lbl = sprintf("%s (%.1f%%)", cat, pct))
  
  g <- ggplot(tb, aes(x = "", y = pct, fill = cat)) +
    geom_col(width = 0.95, color = "white") +
    coord_polar("y") +
    geom_text(aes(label = lbl), position = position_stack(vjust = 0.5), size = 3.1) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_void() + theme(legend.position = "none", plot.title = element_text(face="bold"))
  
  ggsave(out_path, g, width = 7.5, height = 7.5, dpi = 220)
}

# --- sf de puntos ---
build_points_sf <- function(df, lat_col, lon_col){
  if (is.null(lat_col) || is.null(lon_col)) return(NULL)
  d <- df |>
    mutate(lat = parse_num_locale(.data[[lat_col]]),
           lon = parse_num_locale(.data[[lon_col]])) |>
    filter(is.finite(lat), is.finite(lon))
  if (!nrow(d)) return(NULL)
  sf::st_as_sf(d, coords = c("lon","lat"), crs = 4326)
}

# --- Mapa de puntos por ciudad ---
plot_mapa_puntos <- function(points_sf, city, out_dir, title_suffix = ""){
  bb <- bbox_ciudad[[city]]
  if (is.null(bb) || is.null(points_sf)) return(invisible(NULL))
  
  out <- file.path(out_dir, paste0("mapa_puntos_", tolower(city), ".png"))
  
  # recorte por bbox
  poly_bb  <- sf::st_as_sfc(sf::st_bbox(c(xmin = bb$xmin, ymin = bb$ymin,
                                          xmax = bb$xmax, ymax = bb$ymax), crs = 4326))
  pts_city <- suppressWarnings(points_sf[poly_bb, , op = sf::st_within])
  if (!nrow(pts_city)) { message("Sin puntos dentro de bbox para ", city); return(invisible(NULL)) }
  
  g <- ggplot()
  if (has_shp) {
    deptos <- suppressMessages(sf::st_read(shp_deptos, quiet = TRUE))
    g <- g + geom_sf(data = deptos, fill = "grey95", color = "grey80", linewidth = 0.2)
  }
  g <- g +
    geom_sf(data = pts_city, alpha = 0.75, size = 1.8, color = "#2C7FB8") +
    coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE) +
    labs(title = paste0("Ubicación de encuestas (", city, ")"),
         subtitle = title_suffix,
         caption = "Fuente: Encuesta Movilidad NATURA") +
    theme_clean() + theme(panel.grid.major = element_blank())
  
  ggsave(out, g, width = 8, height = 7, dpi = 240)
}

# ==== PIPE POR CIUDAD ====
graficos_ciudad <- function(csv_path, tag_ciudad){
  stopifnot(file.exists(csv_path))
  out_dir <- file.path(out_root, tag_ciudad)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  df <- readr::read_csv(csv_path, show_col_types = FALSE) |> janitor::clean_names()
  N  <- nrow(df)
  
  # detectar columnas
  var_depto   <- detect_depto(df)
  var_mpio    <- detect_mpio(df)
  var_genero  <- detect_genero(df)
  var_estrato <- detect_estrato(df)
  var_modo    <- detect_modo(df)          # => P17 priorizado
  var_lat     <- detect_lat(df)
  var_lon     <- detect_lon(df)
  
  # N total
  gN <- ggplot() + annotate("text", x = 0, y = 0,
                            label = paste0("Tamaño de muestra ", tag_ciudad, ": N = ", format(N, big.mark=",")),
                            size = 6, fontface = "bold") + theme_void()
  ggsave(file.path(out_dir, "00_tamano_muestra.png"), gN, width = 8, height = 2.5, dpi = 200)
  
  # Barras (ordenadas) y tortas
  if (!is.null(var_genero))  plot_bar_pct(df, var_genero,  paste0("Género (N=", N, ")"),   file.path(out_dir, "01_genero_barras.png"), top_n = Inf)
  if (!is.null(var_estrato)) plot_bar_pct(df, var_estrato, paste0("Estrato (N=", N, ")"),  file.path(out_dir, "02_estrato_barras.png"), top_n = Inf)
  if (!is.null(var_modo)) {
    plot_bar_pct(df, var_modo, paste0("Medio de transporte más usado (P17) (N=", N, ")"),
                 file.path(out_dir, "03_modo_barras.png"), top_n = 15)
    plot_pie(df, var_modo, "Medio de transporte más usado (P17) — Top 10 + Otros",
             file.path(out_dir, "03_modo_torta.png"), top_n = 10)
  }
  
  # Comparativa modo × género
  if (!is.null(var_modo) && !is.null(var_genero)) {
    plot_stacked(df, var_modo, var_genero,
                 paste0("Modo de transporte por género (P17) (N=", N, ")"),
                 file.path(out_dir, "04_modo_x_genero_apilado.png"))
  }
  
  # Mapas de puntos LAT/LON
  pts <- build_points_sf(df, var_lat, var_lon)
  if (!is.null(pts)) {
    subt <- NULL
    if (!is.null(var_mpio) || !is.null(var_depto)) {
      subt <- paste(
        c(if (!is.null(var_mpio))  paste0("Mpio: ",  paste0(unique(na.omit(df[[var_mpio]])),  collapse=", ")),
          if (!is.null(var_depto)) paste0("Depto: ", paste0(unique(na.omit(df[[var_depto]])), collapse=", "))),
        collapse = " | ")
    }
    if (tag_ciudad %in% names(bbox_ciudad)) {
      plot_mapa_puntos(pts, tag_ciudad, out_dir, title_suffix = subt)
    }
  }
  
  # Tablas % a Excel
  sheets <- list(meta = tibble(indicador = "n_muestra", valor = N))
  if (!is.null(var_genero))  sheets[["genero_%"]]  <- tbl_pct(df[[var_genero]])
  if (!is.null(var_estrato)) sheets[["estrato_%"]] <- tbl_pct(df[[var_estrato]])
  if (!is.null(var_modo))    sheets[["p17_modo_%"]] <- tbl_pct(df[[var_modo]], top_n = 10)
  writexl::write_xlsx(sheets, file.path(out_dir, "tablas_porcentajes.xlsx"))
  
  message("OK -> ", tag_ciudad, " (gráficos en ", out_dir, ")")
}

# ==== Ejecutar para ambas ciudades ====
csv_cali <- "Output/cali_limpio.csv"
csv_med  <- "Output/medellin_limpio.csv"
if (file.exists(csv_cali)) graficos_ciudad(csv_cali, "Cali")
if (file.exists(csv_med))  graficos_ciudad(csv_med,  "Medellin")

message("Terminado: 2.plots (P17 priorizado, barras/pies pro y mapas de puntos).")

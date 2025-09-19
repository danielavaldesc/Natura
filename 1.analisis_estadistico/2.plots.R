# -----------------------------------------------
# 2.plots.R  
# -----------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(janitor)
  library(ggplot2); library(sf); library(writexl); library(tidyr); library(purrr)
  library(forcats); library(scales); library(readxl)
})

# ==== CONFIG ====
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\")  
in_cali <- "Output\\cali_limpio.xlsx"        
in_med  <- "Output\\medellin_limpio.xlsx"    
out_root <- "1.analisis_estadistico/plots"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# Shapefile opcional (EPSG:4326)
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

# 0) Arreglo universal de acentos/UTF-8 sobre TODAS las columnas de texto
fix_utf8_df <- function(df){
  df[] <- lapply(df, function(x){
    if (is.character(x)) {
      y <- iconv(x, from = "", to = "UTF-8")                 
      y <- str_replace_all(y, "[\\u00A0]", " ")               
      y
    } else x
  })
  df
}

pick_col <- function(df, patterns){
  for (pat in patterns) {
    nm <- names(df)[grepl(pat, names(df), ignore.case = TRUE)]
    if (length(nm)) return(nm[1])
  }
  return(NULL)
}

detect_depto   <- function(df) pick_col(df, c("^p2_1$", "depto", "depart", "dpto"))
detect_mpio    <- function(df) pick_col(df, c("^p2_1_1$", "municip", "mpio"))
detect_genero  <- function(df) pick_col(df, c("^genero$","^género$","^sexo$","\\bsexo\\b"))
detect_estrato <- function(df) pick_col(df, c("^p9estrato$","^estrato$","estrat","^p9(_estrato)?$"))
detect_modo    <- function(df) pick_col(df, c("^p17$","^p17_v[0-9]+$","^p17_", "modo","medio.*trans","transporte","^p6$","^p7(_1)?$"))
detect_lat     <- function(df) pick_col(df, c("^p47latitude$","^p47.*lat","\\blat$","latitude"))
detect_lon     <- function(df) pick_col(df, c("^p47longitude$","^p47.*lon","\\blon$","long","longitude"))

parse_num_locale <- function(x){
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "(?<=\\d)\\.(?=\\d{3}(\\D|$))", "") # miles
  x <- stringr::str_replace(x, ",", ".")                                # coma -> punto
  suppressWarnings(as.numeric(x))
}

# --- Etiquetas más cortas para categorías de modo (útil en apilados) ---
shorten_labels <- function(v){
  s <- as.character(v)
  s <- str_replace_all(s, "Transporte p[uú]blico", "TP")
  s <- str_replace_all(s, "Transporte p[uú]blico \\(MIO\\)", "TP (MIO)")
  s <- str_replace_all(s, "Autom[oó]vil en plataforma.*", "Plataforma (Auto)")
  s <- str_replace_all(s, "Moto en plataforma.*", "Plataforma (Moto)")
  s <- str_replace_all(s, "Autom[oó]vil", "Auto")
  s
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

# --- Barras horizontales con etiquetas ---
plot_bar_pct <- function(df, var, title, out_path, top_n = Inf){
  tb <- tbl_pct(df[[var]], top_n = top_n)
  if (!nrow(tb)) return(invisible(NULL))
  tb <- tb |>
    mutate(cat = shorten_labels(cat),
           cat_f = forcats::fct_reorder(cat, pct),
           label = sprintf("%.1f%% (n=%s)", pct, format(n, big.mark=",")))
  g <- ggplot(tb, aes(y = cat_f, x = pct)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = label), hjust = -0.05, size = 3.4) +
    scale_x_continuous(labels = label_percent(accuracy = 1, scale = 1),
                       expand = expansion(mult = c(0, .15))) +
    labs(title = title, x = "% del total", y = NULL) +
    theme_clean()
  ggsave(out_path, g, width = 9, height = max(4, nrow(tb)*0.35), dpi = 220)
}

# --- Barras apiladas (proporciones) — prolijo: ordena x por tamaño y envuelve etiquetas ---
plot_stacked <- function(df, var_x, var_fill, title, out_path){
  d <- df |>
    select(x = all_of(var_x), fill = all_of(var_fill)) |>
    mutate(x  = shorten_labels(x),
           fill = shorten_labels(fill)) |>
    filter(!is.na(x), x != "", !is.na(fill), fill != "")
  if (!nrow(d)) return(invisible(NULL))
  tb <- d |>
    count(x, fill, name = "n") |>
    group_by(x) |>
    mutate(pct = 100 * n / sum(n)) |>
    ungroup()
  # ordena x por tamaño total
  ord_x <- tb |>
    group_by(x) |>
    summarise(N = sum(n), .groups="drop") |>
    arrange(desc(N)) |>
    pull(x)
  tb <- tb |>
    mutate(x_f = factor(x, levels = ord_x),
           x_f = fct_relabel(x_f, ~ stringr::str_wrap(.x, width = 18)))
  g <- ggplot(tb, aes(x = x_f, y = pct, fill = fill)) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
    labs(title = title, x = NULL, y = "% dentro de cada categoría", fill = NULL) +
    theme_clean() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(out_path, g, width = max(10, length(unique(tb$x))*0.6), height = 6.5, dpi = 220)
}

# --- Pie (Top-10 + Otros) — envuelve etiquetas para evitar sobre-posiciones ---
plot_pie <- function(df, var, title, out_path, top_n = 10){
  tb <- tbl_pct(df[[var]], top_n = top_n)
  if (!nrow(tb)) return(invisible(NULL))
  tb <- tb |>
    mutate(cat = shorten_labels(cat),
           lbl = stringr::str_wrap(sprintf("%s (%.1f%%)", cat, pct), width = 28))
  g <- ggplot(tb, aes(x = "", y = pct, fill = cat)) +
    geom_col(width = 0.95, color = "white") +
    coord_polar("y") +
    geom_text(aes(label = lbl), position = position_stack(vjust = 0.5), size = 3.1) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_void() + theme(legend.position = "none", plot.title = element_text(face="bold"))
  ggsave(out_path, g, width = 8.5, height = 8.5, dpi = 230)
}

# --- Agrupar modos (P17) en 4 categorías ---
group_modo <- function(x) {
  y <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(y, "autom[oó]vil|carro|camioneta|suv|plat[oó]n|van|taxi|uber|yango|didi|in[ -]?driver|particular|car pooling") ~ "Autos",
    stringr::str_detect(y, "moto|motocicleta|moto taxi|mototaxi|picap") ~ "Motos",
    stringr::str_detect(y, "mio|bus|buseta|microbus|metro|tranv[ií]a|brt|sitp|alimentador|metrocable|tp.*formal|transporte p[uú]blico") ~ "Transporte público formal",
    TRUE ~ "Otros"
  )
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

# --- Mapa robusto: si bbox de ciudad deja 0 puntos, usa bbox "auto" de los datos ---
plot_mapa_puntos <- function(points_sf, city, out_dir, title_suffix = ""){
  if (is.null(points_sf)) return(invisible(NULL))
  out <- file.path(out_dir, paste0("mapa_puntos_", tolower(city), ".png"))
  
  # BBox de la ciudad si existe
  bb_city <- bbox_ciudad[[city]]
  poly_bb_city <- if (!is.null(bb_city)) {
    sf::st_as_sfc(sf::st_bbox(c(xmin = bb_city$xmin, ymin = bb_city$ymin,
                                xmax = bb_city$xmax, ymax = bb_city$ymax), crs = 4326))
  } else NULL
  
  pts_city <- tryCatch({
    if (!is.null(poly_bb_city)) sf::st_crop(points_sf, sf::st_bbox(poly_bb_city)) else points_sf
  }, error = function(e) points_sf)
  
  # Si quedó vacío, definimos bbox "auto" con un buffer pequeño
  if (!nrow(pts_city)) {
    bb <- sf::st_bbox(points_sf)
    buf_x <- (bb$xmax - bb$xmin) * 0.08
    buf_y <- (bb$ymax - bb$ymin) * 0.08
    poly_bb_auto <- sf::st_as_sfc(sf::st_bbox(c(
      xmin = bb$xmin - buf_x, xmax = bb$xmax + buf_x,
      ymin = bb$ymin - buf_y, ymax = bb$ymax + buf_y
    ), crs = 4326))
    poly_bb_city <- poly_bb_auto
    pts_city <- points_sf
  }
  
  # Gráfico
  g <- ggplot()
  if (has_shp) {
    deptos <- suppressMessages(sf::st_read(shp_deptos, quiet = TRUE))
    if (is.na(sf::st_crs(deptos))) deptos <- sf::st_set_crs(deptos, 4326)
    else if (sf::st_crs(deptos)$epsg != 4326) deptos <- sf::st_transform(deptos, 4326)
    g <- g + geom_sf(data = deptos, fill = NA, color = "grey55", linewidth = 0.5)
  }
  if (!is.null(poly_bb_city)) g <- g + geom_sf(data = poly_bb_city, fill = NA, color = "black", linewidth = 0.6, linetype = "dashed")
  
  g <- g +
    geom_sf(data = pts_city, alpha = 0.85, size = 1.9, color = "#2C7FB8") +
    labs(title = paste0("Ubicación de encuestas (", city, ")"),
         subtitle = stringr::str_wrap(title_suffix %||% "", width = 90),
         caption = "Fuente: Encuesta Movilidad NATURA") +
    theme_clean() + theme(panel.grid.major = element_blank())
  
  # Ajusta límites con bbox usado
  if (!is.null(poly_bb_city)) {
    bbp <- sf::st_bbox(poly_bb_city)
    g <- g + coord_sf(xlim = c(bbp$xmin, bbp$xmax), ylim = c(bbp$ymin, bbp$ymax), expand = FALSE)
  }
  ggsave(out, g, width = 8.5, height = 7.3, dpi = 240)
}

`%||%` <- function(a, b) if (is.null(a) || length(a)==0 || is.na(a)) b else a

# --- Lector hoja fija ---
read_xlsx_sheet <- function(path, sheet){
  stopifnot(file.exists(path))
  readxl::read_excel(path, sheet = sheet) |> janitor::clean_names()
}

# --- Normaliza ESTRATO a etiquetas "Estrato 1..6" si vienen numéricas ---
normalize_estrato <- function(x){
  x_chr <- as.character(x)
  if (all(grepl("^\\s*estrato\\s*\\d\\s*$", tolower(x_chr[!is.na(x_chr)])))) return(x_chr)
  x_num <- suppressWarnings(as.numeric(x_chr))
  if (sum(is.finite(x_num), na.rm = TRUE) > 0) {
    out <- ifelse(is.finite(x_num), paste0("Estrato ", x_num), x_chr)
    return(out)
  }
  x_chr
}

# ==== PIPE POR CIUDAD ====
graficos_ciudad <- function(xlsx_path, hoja, tag_ciudad){
  out_dir <- file.path(out_root, tag_ciudad); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  df <- read_xlsx_sheet(xlsx_path, hoja) |> fix_utf8_df()
  N  <- nrow(df)
  
  var_depto   <- detect_depto(df)
  var_mpio    <- detect_mpio(df)
  var_genero  <- detect_genero(df)
  var_estrato <- detect_estrato(df)
  var_modo    <- detect_modo(df)
  var_lat     <- detect_lat(df)
  var_lon     <- detect_lon(df)
  if ("p17" %in% names(df)) var_modo <- "p17"
  
  message("Vars -> genero=", var_genero, " | estrato=", var_estrato,
          " | modo=", var_modo, " | lat=", var_lat, " | lon=", var_lon)
  
  gN <- ggplot() + annotate("text", x = 0, y = 0,
                            label = paste0("Tamaño de muestra ", tag_ciudad, ": N = ", format(N, big.mark=",")),
                            size = 6, fontface = "bold") + theme_void()
  ggsave(file.path(out_dir, "00_tamano_muestra.png"), gN, width = 8, height = 2.5, dpi = 200)
  
  # --- GÉNERO ---
  if (!is.null(var_genero))  {
    plot_bar_pct(df, var_genero,  paste0("Género (N=", N, ")"),
                 file.path(out_dir, "01_genero_barras.png"), top_n = Inf)
    plot_pie(df, var_genero, "Género — distribución porcentual",
             file.path(out_dir, "01b_genero_torta.png"), top_n = Inf)
  }
  
  # --- ESTRATO ---
  if (!is.null(var_estrato)) {
    df[[var_estrato]] <- normalize_estrato(df[[var_estrato]])
    df[[var_estrato]] <- forcats::fct_relevel(as.factor(df[[var_estrato]]),
                                              "Estrato 1","Estrato 2","Estrato 3","Estrato 4","Estrato 5","Estrato 6", after = 0)
    plot_bar_pct(df, var_estrato, paste0("Estrato (N=", N, ")"),
                 file.path(out_dir, "02_estrato_barras.png"), top_n = Inf)
  }
  
  # --- MODO (P17) ---
  if (!is.null(var_modo)) {
    df[[var_modo]] <- shorten_labels(df[[var_modo]])
    plot_bar_pct(df, var_modo, paste0("Medio de transporte más usado (P17) (N=", N, ")"),
                 file.path(out_dir, "03_modo_barras.png"), top_n = 15)
    plot_pie(df, var_modo, "Medio de transporte más usado (P17) — Top 10 + Otros",
             file.path(out_dir, "03_modo_torta.png"), top_n = 10)
    df$modo_agr <- group_modo(df[[var_modo]])
    plot_pie(df, "modo_agr", "Medio de transporte (P17) — Agrupado",
             file.path(out_dir, "03b_modo_agr_torta.png"), top_n = Inf)
  }
  
  # --- GÉNERO × ESTRATO ---
  if (!is.null(var_genero) && !is.null(var_estrato)) {
    plot_stacked(df, var_estrato, var_genero,
                 paste0("Composición por género dentro de cada estrato (N=", N, ")"),
                 file.path(out_dir, "02b_genero_x_estrato_apilado.png"))
  }
  
  # --- MODO × GÉNERO ---
  if (!is.null(var_modo) && !is.null(var_genero)) {
    plot_stacked(df, var_modo, var_genero,
                 paste0("Modo de transporte por género (P17) (N=", N, ")"),
                 file.path(out_dir, "04_modo_x_genero_apilado.png"))
  }
  
  # --- MAPA ---
  pts <- build_points_sf(df, var_lat, var_lon)
  if (!is.null(pts)) {
    subt <- NULL
    if (!is.null(var_mpio) || !is.null(var_depto)) {
      subt <- paste(
        c(if (!is.null(var_mpio))  paste0("Mpio: ",  paste0(unique(na.omit(df[[var_mpio]])),  collapse=", ")),
          if (!is.null(var_depto)) paste0("Depto: ", paste0(unique(na.omit(df[[var_depto]])), collapse=", "))),
        collapse = " | ")
    }
    plot_mapa_puntos(pts, tag_ciudad, out_dir, title_suffix = subt)
  }
  
  # --- Tablas % a Excel ---
  sheets <- list(meta = tibble(indicador = "n_muestra", valor = N))
  if (!is.null(var_genero))   sheets[["genero_%"]]     <- tbl_pct(df[[var_genero]])
  if (!is.null(var_estrato))  sheets[["estrato_%"]]    <- tbl_pct(df[[var_estrato]])
  if (!is.null(var_modo)) {
    sheets[["p17_modo_%"]]     <- tbl_pct(df[[var_modo]], top_n = 10)
    if ("modo_agr" %in% names(df)) sheets[["p17_modo_agr_%"]] <- tbl_pct(df[["modo_agr"]], top_n = Inf)
  }
  writexl::write_xlsx(sheets, file.path(out_dir, "tablas_porcentajes.xlsx"))
  
  message("OK -> ", tag_ciudad, " (gráficos en ", out_dir, ")")
}

# ==== Ejecutar ====
if (file.exists(in_cali)) graficos_ciudad(in_cali, "cali_limpio", "Cali")
if (file.exists(in_med))  graficos_ciudad(in_med,  "medellin_limpio", "Medellin")

message("Terminado: 2.plots con fixes.")



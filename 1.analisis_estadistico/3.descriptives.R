# ============================================================
# Descriptivos: normalidad, resumen continuas y tablas n(%)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(janitor)
  library(stringr); library(purrr); library(writexl); library(kableExtra)
  library(moments); library(forcats)
})

# ------- Helpers básicos -------
is_whole_number <- function(x) is.finite(x) & (abs(x - round(x)) < .Machine$double.eps^0.5)

# Evalúa normalidad con Shapiro si 3<=n<=5000, si no, usa criterio por asimetría/kurtosis
normality_one <- function(x){
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(list(n = n, skew = NA_real_, kurt = NA_real_, W = NA_real_, p = NA_real_, normal = NA))
  sk <- moments::skewness(x); ku <- moments::kurtosis(x)
  if (n >= 3 && n <= 5000) {
    sw <- tryCatch(shapiro.test(x), error = function(e) NULL)
    if (!is.null(sw)) {
      return(list(n = n, skew = sk, kurt = ku,
                  W = as.numeric(sw$statistic), p = sw$p.value,
                  normal = (sw$p.value >= 0.05)))
    }
  }
  # Criterio práctico si no usamos SW: |skew|<1 & |kurtosis(excess)|<1
  normal_rule <- (abs(sk) < 1 & abs(ku - 3) < 1)
  list(n = n, skew = sk, kurt = ku, W = NA_real_, p = NA_real_, normal = normal_rule)
}

# -------- Normalidad para varias variables continuas --------
normalidad_df <- function(data, continuous_vars) {
  cont <- continuous_vars
  map_dfr(cont, function(v){
    x <- suppressWarnings(as.numeric(pull(data, !!sym(v))))
    res <- normality_one(x)
    tibble(variable = v, n = res$n, skewness = res$skew, kurtosis = res$kurt,
           W = res$W, p_value = res$p, normal = res$normal)
  }) %>%
    mutate(across(c(skewness, kurtosis, W, p_value), ~round(.x, 4)))
}

# -------- Resumen de continuas (total) ----------------------
resumen_cont_total <- function(data, normal_tbl){
  map_dfr(normal_tbl$variable, function(v){
    x <- suppressWarnings(as.numeric(pull(data, !!sym(v))))
    x <- x[is.finite(x)]
    if (!length(x)) return(tibble(variable = v, resumen = NA_character_))
    if (isTRUE(normal_tbl$normal[normal_tbl$variable == v])) {
      m <- mean(x); s <- sd(x)
      tibble(variable = v, resumen = sprintf("%.2f (DE %.2f)", m, s))
    } else {
      q <- quantile(x, probs = c(.25,.5,.75), na.rm = TRUE, type = 6)
      tibble(variable = v, resumen = sprintf("%.2f (%.2f–%.2f)", q[2], q[1], q[3]))
    }
  })
}

# -------- Resumen de continuas estratificado ----------------
resumen_cont_por_grupo <- function(data, group_var, normal_tbl){
  g <- enquo(group_var)
  vars <- normal_tbl$variable
  out_list <- map(vars, function(v){
    x <- suppressWarnings(as.numeric(pull(data, !!sym(v))))
    df <- data %>% mutate(.val = x) %>% filter(is.finite(.val))
    if (!nrow(df)) return(tibble())
    if (isTRUE(normal_tbl$normal[normal_tbl$variable == v])) {
      df %>% group_by(!!g) %>%
        summarise(res = sprintf("%.2f (DE %.2f)", mean(.val), sd(.val)), .groups = "drop") %>%
        mutate(variable = v)
    } else {
      df %>% group_by(!!g) %>%
        summarise(
          med = median(.val),
          q1  = quantile(.val, .25, type = 6),
          q3  = quantile(.val, .75, type = 6),
          .groups = "drop"
        ) %>%
        transmute(!!as_label(g), res = sprintf("%.2f (%.2f–%.2f)", med, q1, q3), variable = v)
    }
  })
  bind_rows(out_list) %>%
    select(variable, !!as_label(g), res) %>%
    tidyr::pivot_wider(names_from = !!g, values_from = res)
}

# --------- Tablas n(%) para categóricas (total) --------------
tabla_cat_total <- function(data, cat_var){
  v <- enquo(cat_var)
  data %>%
    mutate(.x = as.character(!!v)) %>%
    filter(!is.na(.x) & .x != "") %>%
    count(.x, name = "n") %>%
    mutate(pct = 100*n/sum(n),
           `n (%)` = sprintf("%s (%.1f%%)", format(n, big.mark=","), pct)) %>%
    transmute(categoria = .x, `n (%)`)
}

# ---- Tablas n(%) categóricas estratificadas por grupo -------
tabla_cat_por_grupo <- function(data, cat_var, group_var){
  v <- enquo(cat_var); g <- enquo(group_var)
  base <- data %>%
    mutate(.x = as.character(!!v), .g = as.character(!!g)) %>%
    filter(!is.na(.x) & .x != "" & !is.na(.g) & .g != "")
  if (!nrow(base)) return(tibble())
  base %>%
    count(.g, .x, name = "n") %>%
    group_by(.g) %>%
    mutate(pct = 100*n/sum(n),
           cell = sprintf("%s (%.1f%%)", format(n, big.mark=","), pct)) %>%
    ungroup() %>%
    select(grupo = .g, categoria = .x, cell) %>%
    tidyr::pivot_wider(names_from = grupo, values_from = cell) %>%
    arrange(categoria)
}

# --------------- WRAPPER PRINCIPAL --------------------------
# data: data.frame limpio
# continuous_vars: vector de continuas
# categorical_vars: vector de categóricas
# group_vars: vector con 0, 1 o 2 variables de estratificación (p.ej. c("genero", "medio"))
# out_xlsx: ruta de Excel; out_tex_dir: carpeta para .tex (opcional)
run_descriptivo <- function(data,
                            continuous_vars = character(0),
                            categorical_vars = character(0),
                            group_vars = character(0),
                            out_xlsx = "salidas/descriptivo.xlsx",
                            out_tex_dir = NULL) {
  
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(out_tex_dir)) dir.create(out_tex_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- NORMALIDAD ---
  norm_tbl <- normalidad_df(data, continuous_vars)
  
  # --- CONTINUAS: TOTAL + POR GRUPO(S) ---
  cont_total <- resumen_cont_total(data, norm_tbl) %>% rename(`Total` = resumen)
  
  cont_by_group <- list()
  for (g in group_vars) {
    cont_by_group[[paste0("cont_por_", g)]] <-
      resumen_cont_por_grupo(data, !!sym(g), norm_tbl)
  }
  
  # --- CATEGÓRICAS: TOTAL + POR GRUPO(S) ---
  cat_total_list <- map(categorical_vars, ~tabla_cat_total(data, !!sym(.x)))
  names(cat_total_list) <- categorical_vars
  
  cat_by_group <- list()
  for (g in group_vars) {
    cat_by_group[[paste0("cat_por_", g)]] <-
      map(categorical_vars, ~tabla_cat_por_grupo(data, !!sym(.x), !!sym(g)))
  }
  
  # --------- EXPORT A EXCEL ----------
  sheets <- list(
    normalidad = norm_tbl,
    continuas_total = cont_total
  )
  
  # Unir continuas por grupo (si existen)
  for (nm in names(cont_by_group)) {
    sheets[[nm]] <- cont_by_group[[nm]]
  }
  
  # Categóricas total (cada var en hoja separada para claridad)
  for (v in names(cat_total_list)) {
    sheets[[paste0("cat_total__", v)]] <- cat_total_list[[v]]
  }
  # Categóricas por grupo: cada grupo y variable a hoja
  for (nm in names(cat_by_group)) {
    lst <- cat_by_group[[nm]]
    for (v in names(lst)) {
      sheets[[paste0(nm, "__", v)]] <- lst[[v]]
    }
  }
  
  writexl::write_xlsx(sheets, path = out_xlsx)
  
  # --------- (Opcional) TEX con kable ----------
  if (!is.null(out_tex_dir)) {
    # Normalidad
    writeLines(
      kable(norm_tbl, caption = "Prueba de normalidad (Shapiro-Wilk/skew-kurtosis)", format = "latex"),
      file.path(out_tex_dir, "01_normalidad.tex")
    )
    # Continuas total
    writeLines(
      kable(cont_total, caption = "Resumen de continuas: media (DE) si normal, mediana (Q1–Q3) si no", format = "latex"),
      file.path(out_tex_dir, "02_continuas_total.tex")
    )
    # Continuas por grupo
    for (nm in names(cont_by_group)) {
      if (nrow(cont_by_group[[nm]])) {
        writeLines(
          kable(cont_by_group[[nm]], caption = paste("Continuas por", gsub("^cont_por_", "", nm)), format = "latex"),
          file.path(out_tex_dir, paste0("02b_", nm, ".tex"))
        )
      }
    }
    # Categóricas
    for (v in names(cat_total_list)) {
      writeLines(
        kable(cat_total_list[[v]], caption = paste0("Categórica ", v, " (n %) total"), format = "latex"),
        file.path(out_tex_dir, paste0("03_cat_total__", v, ".tex"))
      )
    }
    for (nm in names(cat_by_group)) {
      lst <- cat_by_group[[nm]]
      for (v in names(lst)) {
        if (nrow(lst[[v]])) {
          writeLines(
            kable(lst[[v]], caption = paste0("Categórica ", v, " por ", gsub("^cat_por_", "", nm), " (n %)"), format = "latex"),
            file.path(out_tex_dir, paste0("03b_", nm, "__", v, ".tex"))
          )
        }
      }
    }
  }
  
  invisible(list(
    normalidad = norm_tbl,
    continuas_total = cont_total,
    continuas_por_grupo = cont_by_group,
    categoricas_total = cat_total_list,
    categoricas_por_grupo = cat_by_group
  ))
}
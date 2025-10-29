##############################################
## FUNCIÓN GENERAL DE DESCRIPCIÓN POR GRUPO
## Compatible con cualquier variable de control
##############################################

library(dplyr)
library(tidyr)
library(stats)
library(rlang)
library(stringr)

describe_by_group <- function(data, vars_cat, vars_cont,
                              control_var,
                              diccionario = NULL) {
  # --- Etiquetas desde diccionario (opcional) ---
  labels <- NULL
  if (!is.null(diccionario)) {
    labels <- diccionario %>%
      dplyr::select(codigo, descripcion) %>%
      deframe()
  }
  
  # --- Asegurar factor en variable de control y guardar niveles totales ---
  data[[control_var]] <- as.factor(data[[control_var]])
  control_levels <- levels(data[[control_var]])
  n_groups <- length(control_levels)
  
  results <- list()
  
  # =========================================================
  # 1) VARIABLES CATEGÓRICAS
  # =========================================================
  for (v in vars_cat) {
    # Tabla con todas las columnas de los niveles del control
    tab <- table(data[[v]], data[[control_var]])
    prop <- suppressWarnings(prop.table(tab, 2) * 100)
    
    # Data frame a mostrar
    df_display <- data.frame(
      Variable  = v,
      Categoria = rownames(tab),
      stringsAsFactors = FALSE
    )
    
    # Añadir columnas dinámicas para cada nivel del control
    for (g in control_levels) {
      # Si una columna no existe (shouldn't, pero por seguridad), la creamos en 0
      if (!(g %in% colnames(tab))) {
        df_display[[g]] <- "0 (0.0%)"
      } else {
        nums  <- as.integer(tab[, g])
        props <- if (!is.null(dim(prop))) round(prop[, g], 1) else rep(NA_real_, length(nums))
        df_display[[g]] <- sprintf("%d (%.1f%%)", nums, props)
      }
    }
    
    # Chi-cuadrado solo si hay ≥2 filas y ≥2 columnas con totales > 0
    rows_use <- rowSums(tab) > 0
    cols_use <- colSums(tab) > 0
    if (sum(rows_use) >= 2 && sum(cols_use) >= 2) {
      pval <- suppressWarnings(chisq.test(tab[rows_use, cols_use], correct = FALSE)$p.value)
    } else {
      pval <- NA_real_
    }
    
    df_display$p_value <- round(pval, 4)
    df_display$Label   <- if (!is.null(labels[[v]])) labels[[v]] else v
    results[[v]] <- df_display
  }
  
  # =========================================================
  # 2) VARIABLES CONTINUAS
  # =========================================================
  for (v in vars_cont) {
    # Resumen por grupo (manteniendo niveles aunque no tengan datos)
    df_summary <- data %>%
      group_by(!!sym(control_var), .drop = FALSE) %>%
      summarise(
        Median = suppressWarnings(median(.data[[v]], na.rm = TRUE)),
        Q1     = suppressWarnings(quantile(.data[[v]], 0.25, na.rm = TRUE)),
        Q3     = suppressWarnings(quantile(.data[[v]], 0.75, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(Resumen = ifelse(is.finite(Median),
                              sprintf("%.1f (%.1f–%.1f)", Median, Q1, Q3),
                              NA_character_)) %>%
      dplyr::select(!!sym(control_var), Resumen) %>%
      pivot_wider(names_from = !!sym(control_var), values_from = Resumen)
    
    # Subconjunto con datos no-NA para la variable y el control
    sub <- data %>%
      dplyr::select(all_of(c(v, control_var))) %>%
      filter(!is.na(.data[[v]]), !is.na(.data[[control_var]]))
    
    # Número de grupos con datos
    ng_present <- n_distinct(droplevels(sub[[control_var]]))
    
    # Elegir test (o NA) según disponibilidad de grupos
    if (ng_present < 2) {
      pval <- NA_real_
    } else if (ng_present == 2) {
      # Mann–Whitney
      pval <- tryCatch(
        wilcox.test(sub[[v]] ~ droplevels(sub[[control_var]]), exact = FALSE)$p.value,
        error = function(e) NA_real_
      )
    } else {
      # Kruskal–Wallis
      pval <- tryCatch(
        kruskal.test(sub[[v]] ~ droplevels(as.factor(sub[[control_var]])))$p.value,
        error = function(e) NA_real_
      )
    }
    
    resumen <- df_summary %>%
      mutate(
        Variable  = v,
        Categoria = "",
        p_value   = round(pval, 4),
        Label     = if (!is.null(labels[[v]])) labels[[v]] else v
      ) %>%
      relocate(Variable, Label, Categoria)
    
    # Asegurar que existan todas las columnas de niveles del control (si pivot_wider no las creó)
    for (g in control_levels) {
      if (!(g %in% names(resumen))) resumen[[g]] <- NA_character_
    }
    resumen <- resumen[, c("Variable", "Label", "Categoria", control_levels, "p_value")]
    
    results[[v]] <- resumen
  }
  
  # =========================================================
  # 3) UNIR Y ORDENAR
  # =========================================================
  final <- bind_rows(results)
  
  # Orden de columnas: Variable | Label | Categoria | grupos... | p_value
  keep_order <- c("Variable", "Label", "Categoria", control_levels, "p_value")
  missing_cols <- setdiff(keep_order, names(final))
  if (length(missing_cols)) final[missing_cols] <- NA
  final <- final[, keep_order]
  
  final
}

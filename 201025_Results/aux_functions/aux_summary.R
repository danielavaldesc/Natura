##############################################
## FUNCIÓN GENERAL DE DESCRIPCIÓN POR GRUPO
## Compatible con cualquier variable de control
##############################################

library(dplyr)
library(tidyr)
library(stats)
library(rlang)

describe_by_group <- function(data, vars_cat, vars_cont,
                              control_var,
                              diccionario = NULL) {
  
  # Crear vector de etiquetas (si se pasa el diccionario)
  labels <- NULL
  if (!is.null(diccionario)) {
    labels <- diccionario %>%
      select(codigo, descripcion) %>%
      deframe()
  }
  
  # Asegurar que la variable de control es factor
  data[[control_var]] <- as.factor(data[[control_var]])
  control_levels <- levels(data[[control_var]])
  n_groups <- length(control_levels)
  
  results <- list()
  
  # --- VARIABLES CATEGÓRICAS ---
  for (v in vars_cat) {
    tab <- table(data[[v]], data[[control_var]])
    prop <- prop.table(tab, 2) * 100
    
    # Crear tabla con n(%) por grupo dinámicamente
    df_cat <- as.data.frame.matrix(tab)
    df_prop <- as.data.frame.matrix(round(prop, 1))
    
    df_display <- data.frame(
      Variable = v,
      Categoria = rownames(tab),
      stringsAsFactors = FALSE
    )
    
    # Agregar columnas dinámicas (una por categoría del control)
    for (g in control_levels) {
      df_display[[g]] <- sprintf("%d (%.1f%%)",
                                 tab[, g], prop[, g])
    }
    
    # Test chi-cuadrado
    pval <- if (nrow(tab) > 1) suppressWarnings(chisq.test(tab)$p.value) else NA
    
    df_display$p_value <- round(pval, 4)
    df_display$Label <- if (!is.null(labels[[v]])) labels[[v]] else v
    
    results[[v]] <- df_display
  }
  
  
  # --- VARIABLES CONTINUAS ---
  for (v in vars_cont) {
    
    # Calcular mediana y rango intercuartílico por grupo
    df <- data %>%
      group_by(!!sym(control_var)) %>%
      summarise(
        Median = median(.data[[v]], na.rm = TRUE),
        Q1 = quantile(.data[[v]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[v]], 0.75, na.rm = TRUE)
      ) %>%
      mutate(Resumen = sprintf("%.1f (%.1f–%.1f)", Median, Q1, Q3)) %>%
      select(!!sym(control_var), Resumen) %>%
      pivot_wider(names_from = !!sym(control_var), values_from = Resumen)
    
    # Test estadístico según número de grupos
    if (n_groups == 2) {
      test <- wilcox.test(data[[v]] ~ data[[control_var]], exact = FALSE)
    } else {
      test <- kruskal.test(data[[v]] ~ as.factor(data[[control_var]]))
    }
    pval <- round(test$p.value, 4)
    
    resumen <- df %>%
      mutate(
        Variable = v,
        Categoria = "",
        p_value = pval,
        Label = if (!is.null(labels[[v]])) labels[[v]] else v
      ) %>%
      relocate(Variable, Label, Categoria)
    
    results[[v]] <- resumen
  }
  
  # --- UNIR RESULTADOS ---
  final <- bind_rows(results)
  
  # Asegurar columnas en orden (Variable, Label, Categoria, grupos..., p-value)
  final <- final %>%
    relocate(Variable, Label, Categoria)
  
  # Asegurar que p-value es la última columna
  pval_col <- which(names(final) == "p_value")
  if (pval_col < ncol(final)) {
    final <- final %>% select(-p_value, everything(), p_value)
  }
  
  return(final)
}

################################################
## Funciones auxiliares: análisis descriptivo ##
################################################

##########################################################
## Función 1: creación e importación de tablas cruzadas ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##########################################################

cross_tab <- function(data, row_var, col_var, caption, tex_path, xlsx_path) {
  library(kableExtra)
  table_data <- prop.table(table(data[[row_var]], data[[col_var]])) * 100
  table_data <- round(table_data, digits = 2) # Round values
  
  # LaTeX
  table_latex <- kable(table_data, caption = caption, format = "latex")
  writeLines(table_latex, tex_path)
  
  # Excel
  table_df <- as.data.frame.array(table_data)
  table_df$rownames <- rownames(table_df)
  table_df <- table_df[c("rownames", setdiff(colnames(table_df), "rownames"))]
  writexl::write_xlsx(table_df, xlsx_path)
  return(table_df)
}

##########################################################
## Función 2: prueba de normalidad univariada           ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##########################################################

normality <- function(data, continuous_vars, output_tex, output_xlsx) {
  library(moments)
  var.aux <- setdiff(continuous_vars, c("id", "p17"))
  
  # Dataframe de recepción
  normality_df <- data.frame(Variable = var.aux, Kurtosis = NA,
                             Skewness = NA, W = NA, p_value = NA
  )
  
  # Bucle para calcular estadísticas
  for (var in var.aux) {
    print(paste0(var, " OK"))
    temp_data <- data[[var]]
    row_index <- which(normality_df$Variable == var)
    normality_df$Kurtosis[row_index] <- kurtosis(temp_data)
    normality_df$Skewness[row_index] <- skewness(temp_data)
    shapiro_res <- shapiro.test(temp_data)
    normality_df$W[row_index] <- shapiro_res$statistic
    normality_df$p_value[row_index] <- shapiro_res$p.value
  }
  
  normality_df[, -1] <- round(normality_df[, -1], 2)
  
  # LaTeX
  normality_tex <- kable(normality_df, 
                         caption = "Tabla 4: prueba de normalidad de Shapiro-Wilk", 
                         format = "latex"
  )
  writeLines(normality_tex, output_tex)
  
  # Excel
  writexl::write_xlsx(normality_df, output_xlsx)
  
  return(normality_df)
}


#############################################################
## Función 3: Resumen descriptivo para variables continuas ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#############################################################

cont_descriptive <- function(data, control, continuous_vars,
                             output_tex, output_xlsx) {
  activos = data %>% dplyr::filter(p17 == "Activos")
  app_auto = data %>% dplyr::filter(p17 == "Aplicación viajes / Taxi (auto)")
  app_moto = data %>% dplyr::filter(p17 == "Aplicación viajes (moto)")
  auto = data %>% dplyr::filter(p17 == "Auto")
  moto = data %>% dplyr::filter(p17 == "Moto")
  publico_formal = data %>% dplyr::filter(p17 == "Público (formal)")
  pub_inf_auto = data %>% dplyr::filter(p17 == "Público (informal - auto)")
  pub_inf_moto = data %>% dplyr::filter(p17 == "Público (informal - moto taxi)")
  
  medios.output = list(total = data.frame(), activos = data.frame(), app_auto = data.frame(),
                       app_moto = data.frame(), auto = data.frame(), moto = data.frame(), 
                       publico_formal = data.frame(), pub_inf_auto = data.frame(), 
                       pub_inf_moto = data.frame())
  
  medios.input = list(total = dataset, activos = activos, app_auto = app_auto,
                      app_moto = app_moto, auto = auto, moto = moto, 
                      publico_formal = publico_formal,pub_inf_auto = pub_inf_auto, 
                      pub_inf_moto = pub_inf_auto)
  
  # Loop para calcular mediana y cuartiles para cada categoría
  for (i in control) {
    print(paste0(i, " OK"))
    index.input = which(names(medios.input) == i)
    index.output = which(names(medios.output) == i)
    medios.output[[index.output]] = data.frame(Variable = setdiff(continuous_vars,
                                                                  c("id", "p17")),
                                               x = rep(NA, length(continuous_vars)))
    
    for (k in setdiff(continuous_vars, c("id", "p17"))) {
      print(paste0(i," - ", k, " OK"))
      select_var = c("id", k)
      df.aux = medios.input[[index.input]][select_var]
      colnames(df.aux) = c("id", "continua_aux")
      
      df.aux <- na.omit(df.aux)
      
      n.row = which(medios.output[[index.output]]$Variable == k)
      medios.output[[index.output]]$x[n.row] =  paste0(round(median(df.aux$continua_aux), 2), " (",
                                                       round(quantile(df.aux$continua_aux, c(0.25), type = 6),2),
                                                       ", ", 
                                                       round(quantile(df.aux$continua_aux, c(0.75), type = 6),2), ")")
      
      rm(select_var, df.aux, n.row, k)
    }
    
    colnames(medios.output[[index.output]]) = c("Variable", i)
  }
  
  # Fusión de las tablas
  descriptivas.continuas <- Reduce(function(x, y) merge(x, y, by = "Variable"), medios.output)
  
  # Guardar en Latex
  descriptivas.continuas.ltx <- kable(descriptivas.continuas, 
                                      caption = "Resumen descriptivo: variables continuas no-gaussianas, mediana (IQR)", 
                                      format = "latex"
  )
  writeLines(descriptivas.continuas.ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(descriptivas.continuas), output_xlsx)
  return(as.data.frame(descriptivas.continuas))
}

cont_descriptive_gender <- function(data, control, continuous_vars,
                                    output_tex, output_xlsx) {
  
  female = data %>% dplyr::filter(genero == "Femenino")
  male = data %>% dplyr::filter(genero == "Masculino")
  
  medios.output = list(total = data.frame(), 
                       female = data.frame(),
                       male = data.frame()
  )
  
  medios.input = list(total = dataset,
                      female = female,
                      male = male)
  
  # Loop para calcular mediana y cuartiles para cada categoría
  for (i in control) {
    print(paste0(i, " OK"))
    index.input = which(names(medios.input) == i)
    index.output = which(names(medios.output) == i)
    medios.output[[index.output]] = data.frame(Variable = setdiff(continuous_vars,
                                                                  c("id", "p17")),
                                               x = rep(NA, length(continuous_vars)))
    
    for (k in setdiff(continuous_vars, c("id"))) {
      print(paste0(i," - ", k, " OK"))
      select_var = c("id", k)
      df.aux = medios.input[[index.input]][select_var]
      colnames(df.aux) = c("id", "continua_aux")
      
      df.aux <- na.omit(df.aux)
      
      n.row = which(medios.output[[index.output]]$Variable == k)
      medios.output[[index.output]]$x[n.row] =  paste0(round(median(df.aux$continua_aux), 2), " (",
                                                       round(quantile(df.aux$continua_aux, c(0.25), type = 6),2),
                                                       ", ", 
                                                       round(quantile(df.aux$continua_aux, c(0.75), type = 6),2), ")")
      
      rm(select_var, df.aux, n.row, k)
    }
    
    colnames(medios.output[[index.output]]) = c("Variable", i)
  }
  
  # Fusión de las tablas
  descriptivas.continuas <- Reduce(function(x, y) merge(x, y, by = "Variable"), medios.output)
  
  # Guardar en Latex
  descriptivas.continuas.ltx <- kable(descriptivas.continuas, 
                                      caption = "Resumen descriptivo: variables continuas no-gaussianas, mediana (IQR)", 
                                      format = "latex"
  )
  writeLines(descriptivas.continuas.ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(descriptivas.continuas), output_xlsx)
  return(as.data.frame(descriptivas.continuas))
}

###############################################################
## Función 3: Resumen descriptivo para variables categóricas ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###############################################################

cat_descriptive <- function(data, cat_vars,
                            output_tex, output_xlsx){
  library(dplyr)
  # filter dataset to include only discrete variables
  data_discretas <- data[c("id", "p17", cat_vars)]
  
  # filtering by new categories of "medio"
  activos_discretas = data_discretas %>% dplyr::filter(p17 == "Activos")
  app_auto_discretas = data_discretas %>% dplyr::filter(p17 == "Aplicación viajes / Taxi (auto)")
  app_moto_discretas = data_discretas %>% dplyr::filter(p17 == "Aplicación viajes (moto)")
  auto_discretas = data_discretas %>% dplyr::filter(p17 == "Auto")
  moto_discretas = data_discretas %>% dplyr::filter(p17 == "Moto")
  publico_formal_discretas = data_discretas %>% dplyr::filter(p17 == "Público (formal)")
  pub_inf_auto_discretas = data_discretas %>% dplyr::filter(p17 == "Público (informal - auto)")
  pub_inf_moto_discretas = data_discretas %>% dplyr::filter(p17 == "Público (informal - moto taxi)")
  
  # Lists for input and output
  lista_input <- list(total = data_discretas, activos = activos_discretas, app_auto = app_auto_discretas,
                      app_moto = app_moto_discretas, auto = auto_discretas, moto = moto_discretas, 
                      publico_formal = publico_formal_discretas, pub_inf_auto = pub_inf_auto_discretas, 
                      pub_inf_moto = pub_inf_moto_discretas)
  
  # New variable names for lista_output
  lista_output <- vector(mode = "list", length = length(cat_vars))
  names(lista_output) = cat_vars
  
  for (j in setdiff(cat_vars, c("id", "p17"))) {
    print(paste0(j, " OK"))
    medios <- c("total", "activos", "auto", "moto", "app_auto", "app_moto",
                "publico_formal", "pub_inf_auto", "pub_inf_moto")
    lista_medios <- list(total = data.frame(), activos = data.frame(), app_auto = data.frame(),
                         app_moto = data.frame(), auto = data.frame(), moto = data.frame(), 
                         publico_formal = data.frame(), pub_inf_auto = data.frame(), 
                         pub_inf_moto = data.frame())
    
    item_output <- which(names(lista_output) == j)
    
    df_aux_discretas <- data_discretas[c("id", j)]
    colnames(df_aux_discretas) <- c("id", "x")
    
    niveles <- levels(as.factor(df_aux_discretas$x))
    
    for (i in medios) {
      print(paste0(i, " OK"))
      item <- which(names(lista_medios) == i)
      item_base <- which(names(lista_input) == i)
      
      lista_medios[[item]] <- data.frame(Variable = c(j, niveles), 
                                         id = seq(1, (length(niveles) + 1))) 
      
      df_aux_disc_2 <- lista_input[[item_base]][j]
      colnames(df_aux_disc_2) <- c("x")
      count_aux <- plyr::count(df_aux_disc_2$x)
      
      if(nrow(count_aux) > 0){
        count_aux$share <- (count_aux$freq / sum(count_aux$freq)) * 100
        count_aux$n_per <- paste0(count_aux$freq, " (", 
                                  round(count_aux$share, digits = 2), "%)")
        
        colnames(count_aux) <- c("Variable", "freq", "share", "n_per")} else {
          aux.else <- lista_input[[which(names(lista_input) == "total")]][j]
          colnames(aux.else) <- c("x")
          count_else <- plyr::count(aux.else$x)
          count_aux <- data.frame(Variable = count_else$x,
                                  freq = NA,
                                  share = NA,
                                  n_per = NA)
        }
      
      lista_medios[[item]] <- merge(lista_medios[[item]], 
                                    count_aux[c("Variable", "n_per")],
                                    by = "Variable", all.x = TRUE)
      
      lista_medios[[item]] <- lista_medios[[item]][order(lista_medios[[item]]$id),]
      
      lista_medios[[item]] <- lista_medios[[item]][c("Variable", "n_per")]
      
      colnames(lista_medios[[item]]) <- c("Variable", paste0("n_per_", i))
      
    }
    
    lista_medios[[1]]$id <- seq(1, nrow(lista_medios[[1]]))
    
    # Merge all media output into one data frame
    lista_output[[item_output]] <- merge(lista_medios[[1]], 
                                         lista_medios[[2]], by = "Variable")
    for (k in 3:length(lista_medios)) {
      lista_output[[item_output]] <- merge(lista_output[[item_output]], 
                                           lista_medios[[k]], by = "Variable")
    }
    
    lista_output[[item_output]] <- lista_output[[item_output]][order(lista_output[[item_output]]$id),]
    
    lista_output[[item_output]] <- lista_output[[item_output]][setdiff(colnames(lista_output[[item_output]]), "id")]
  }
  
  # Combine all output into a single data frame
  descriptivas_discretas <- do.call(rbind, lista_output)
  
  # Guardar en Latex
  descriptivas_discretas_ltx = kable(descriptivas_discretas, 
                                     caption = "Resumen descriptivo: variables discretas, n (%)",
                                     format = "latex")
  writeLines(descriptivas_discretas_ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(descriptivas_discretas), output_xlsx)
  return(as.data.frame(descriptivas_discretas))
}

cat_descriptive_gender <- function(data, cat_vars,
                                   output_tex, output_xlsx){
  # filter dataset to include only discrete variables
  data_discretas <- data[c("id", cat_vars)]
  
  # filtering by new categories of "medio"
  female_discretas <- data_discretas %>% dplyr::filter(genero == "Femenino")
  male_discretas <- data_discretas %>% dplyr::filter(genero == "Masculino")
  
  # Lists for input and output
  lista_input <- list(total = data_discretas, 
                      female = female_discretas,
                      male = male_discretas)
  
  # New variable names for lista_output
  lista_output <- vector(mode = "list", length = length(cat_vars))
  names(lista_output) = cat_vars
  
  for (j in setdiff(cat_vars, c("id"))) {
    print(paste0(j, " OK"))
    medios <- c("total", "female", "male")
    lista_medios <- list(total = data.frame(), 
                         female = data.frame(),
                         male = data.frame())
    
    item_output <- which(names(lista_output) == j)
    
    df_aux_discretas <- data_discretas[c("id", j)]
    colnames(df_aux_discretas) <- c("id", "x")
    
    niveles <- levels(as.factor(df_aux_discretas$x))
    
    for (i in medios) {
      print(paste0(i, " OK"))
      item <- which(names(lista_medios) == i)
      item_base <- which(names(lista_input) == i)
      
      lista_medios[[item]] <- data.frame(Variable = c(j, niveles), 
                                         id = seq(1, (length(niveles) + 1))) 
      
      df_aux_disc_2 <- lista_input[[item_base]][j]
      colnames(df_aux_disc_2) <- c("x")
      count_aux <- plyr::count(df_aux_disc_2$x)
      
      if(nrow(count_aux) > 0){
        count_aux$share <- (count_aux$freq / sum(count_aux$freq)) * 100
        count_aux$n_per <- paste0(count_aux$freq, " (", 
                                  round(count_aux$share, digits = 2), "%)")
        
        colnames(count_aux) <- c("Variable", "freq", "share", "n_per")} else {
          aux.else <- lista_input[[which(names(lista_input) == "total")]][j]
          colnames(aux.else) <- c("x")
          count_else <- plyr::count(aux.else$x)
          count_aux <- data.frame(Variable = count_else$x,
                                  freq = NA,
                                  share = NA,
                                  n_per = NA)
        }
      
      lista_medios[[item]] <- merge(lista_medios[[item]], 
                                    count_aux[c("Variable", "n_per")],
                                    by = "Variable", all.x = TRUE)
      
      lista_medios[[item]] <- lista_medios[[item]][order(lista_medios[[item]]$id),]
      
      lista_medios[[item]] <- lista_medios[[item]][c("Variable", "n_per")]
      
      colnames(lista_medios[[item]]) <- c("Variable", paste0("n_per_", i))
      
    }
    
    lista_medios[[1]]$id <- seq(1, nrow(lista_medios[[1]]))
    
    # Merge all media output into one data frame
    lista_output[[item_output]] <- merge(lista_medios[[1]], 
                                         lista_medios[[2]], by = "Variable")
    for (k in 3:length(lista_medios)) {
      lista_output[[item_output]] <- merge(lista_output[[item_output]], 
                                           lista_medios[[k]], by = "Variable")
    }
    
    lista_output[[item_output]] <- lista_output[[item_output]][order(lista_output[[item_output]]$id),]
    
    lista_output[[item_output]] <- lista_output[[item_output]][setdiff(colnames(lista_output[[item_output]]),
                                                                       "id")]
  }
  
  # Combine all output into a single data frame
  descriptivas_discretas <- do.call(rbind, lista_output)
  
  # Guardar en Latex
  descriptivas_discretas_ltx = kable(descriptivas_discretas, 
                                     caption = "Resumen descriptivo: variables discretas, n (%)",
                                     format = "latex")
  writeLines(descriptivas_discretas_ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(descriptivas_discretas), output_xlsx)
  return(as.data.frame(descriptivas_discretas))
}

cat_descriptive_mnl <- function(data, cat_vars,
                                output_tex, output_xlsx){
  library(dplyr)
  # filter dataset to include only discrete variables
  data_discretas <- data[c("id", "p17", cat_vars)]
  
  # filtering by new categories of "medio"
  auto_discretas <- data_discretas %>% dplyr::filter(p17 == "Auto")
  moto_discretas <- data_discretas %>% dplyr::filter(p17 == "Moto")
  publico_formal_discretas <- data_discretas %>% dplyr::filter(p17 == "Público (formal)")
  
  # Lists for input and output
  lista_input <- list(total = data_discretas,
                      auto = auto_discretas, moto = moto_discretas,
                      publico_formal = publico_formal_discretas)
  
  # New variable names for lista_output
  lista_output <- vector(mode = "list", length = length(cat_vars))
  names(lista_output) = cat_vars
  
  for (j in setdiff(cat_vars, c("id", "p17"))) {
    print(paste0(j, " OK"))
    medios <- c("total",  "auto", "moto", "publico_formal")
    lista_medios <- list(total = data.frame(), auto = data.frame(), 
                         moto = data.frame(), publico_formal = data.frame())
    
    item_output <- which(names(lista_output) == j)
    
    df_aux_discretas <- data_discretas[c("id", j)]
    colnames(df_aux_discretas) <- c("id", "x")
    
    niveles <- levels(as.factor(df_aux_discretas$x))
    
    for (i in medios) {
      print(paste0(i, " OK"))
      item <- which(names(lista_medios) == i)
      item_base <- which(names(lista_input) == i)
      
      lista_medios[[item]] <- data.frame(Variable = c(j, niveles), 
                                         id = seq(1, (length(niveles) + 1))) 
      
      df_aux_disc_2 <- lista_input[[item_base]][j]
      colnames(df_aux_disc_2) <- c("x")
      
      count_aux <- plyr::count(df_aux_disc_2$x)
      count_aux$share <- (count_aux$freq / sum(count_aux$freq)) * 100
      count_aux$n_per <- paste0(count_aux$freq, " (", 
                                round(count_aux$share, digits = 2), "%)")
      
      colnames(count_aux) <- c("Variable", "freq", "share", "n_per")
      
      lista_medios[[item]] <- merge(lista_medios[[item]], 
                                    count_aux[c("Variable", "n_per")],
                                    by = "Variable", all.x = TRUE)
      
      lista_medios[[item]] <- lista_medios[[item]][order(lista_medios[[item]]$id),]
      
      lista_medios[[item]] <- lista_medios[[item]][c("Variable", "n_per")]
      
      colnames(lista_medios[[item]]) <- c("Variable", paste0("n_per_", i))
      
    }
    
    lista_medios[[1]]$id <- seq(1, nrow(lista_medios[[1]]))
    
    # Merge all media output into one data frame
    lista_output[[item_output]] <- merge(lista_medios[[1]], 
                                         lista_medios[[2]], by = "Variable")
    for (k in 3:length(lista_medios)) {
      lista_output[[item_output]] <- merge(lista_output[[item_output]], 
                                           lista_medios[[k]], by = "Variable")
    }
    
    lista_output[[item_output]] <- lista_output[[item_output]][order(lista_output[[item_output]]$id),]
    
    lista_output[[item_output]] <- lista_output[[item_output]][setdiff(colnames(lista_output[[item_output]]), "id")]
  }
  
  # Combine all output into a single data frame
  descriptivas_discretas <- do.call(rbind, lista_output)
  
  # Guardar en Latex
  descriptivas_discretas_ltx = kable(descriptivas_discretas, 
                                     caption = "Resumen descriptivo: variables discretas, n (%)",
                                     format = "latex")
  writeLines(descriptivas_discretas_ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(descriptivas_discretas), output_xlsx)
  return(as.data.frame(descriptivas_discretas))
}

###############################################################
## Función 4: función sobre la prueba de Kruskal-Wallis     ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###############################################################

kruskal_test = function(data, continuas, discretas,
                        output_tex, output_xlsx){
  
  lista_kruskal = vector(mode = "list", length = length(discretas))
  names(lista_kruskal) = discretas
  
  data_kruskal = dataset[c("id", discretas, continuas)]
  
  hipotesis = continuas
  
  for (i in discretas) {
    print(paste0(i, " OK"))
    item_cat = which(names(lista_kruskal) ==i)
    lista_kruskal[[item_cat]] = data.frame(Variables = hipotesis,
                                           Chi_2 = rep(NA, length(hipotesis)),
                                           p = rep(NA, length(hipotesis)))
    
    for (j in hipotesis) {
      print(paste0(i, " - ", j, " OK"))
      item_hyp = which(lista_kruskal[[item_cat]]$Variables == j )  
      
      kruskal_aux = data_kruskal[c(j,i)] 
      colnames(kruskal_aux) = c("continua", "discreta")
      kruskal_aux$continua = as.numeric(kruskal_aux$continua)
      kruskal_aux$discreta = as.factor(kruskal_aux$discreta)
      kruskal_test_aux = kruskal.test(kruskal_aux$continua~kruskal_aux$discreta)
      lista_kruskal[[item_cat]]$Chi_2[item_hyp] = paste0(round(kruskal_test_aux$statistic, digits = 1),
                                                         " (",
                                                         kruskal_test_aux$parameter
                                                         , ")")
      
      lista_kruskal[[item_cat]]$p[item_hyp]  = round(kruskal_test_aux$p.value, digits = 2)
    }
    
    colnames(lista_kruskal[[item_cat]]) = c("Variables", 
                                            paste0(i,"_Chi_2"),
                                            paste0(i,"_p"))
  }
  
  # Resultados generales
  kruskal <- Reduce(function(x, y) merge(x, y, by = "Variables"), lista_kruskal)
  
  # Guardar en Latex
  kruskal_ltx = kable(kruskal, caption = "Test de Kruskal-Wallis", format = "latex")
  writeLines(kruskal_ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(kruskal), output_xlsx)
  return(as.data.frame(kruskal))
}


##################################################################
## Función 5: función sobre la prueba Chi-2 de independencia    ##------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##################################################################

chi2_test = function(data, discretas,
                     output_tex, output_xlsx){
  pares_no_ordenados <- combn(cat, 2, simplify = FALSE)
  chi_2 <- data.frame(V1 = sapply(pares_no_ordenados, `[[`, 1),
                      V2 = sapply(pares_no_ordenados, `[[`, 2),
                      st = rep(NA, length(pares_no_ordenados)),
                      df = rep(NA, length(pares_no_ordenados)),
                      p = rep(NA, length(pares_no_ordenados)))
  
  # Caso generalizado
  for (k in 1:nrow(chi_2)) {
    print(k)
    var_1 = chi_2$V1[k]
    var_2 = chi_2$V2[k]
    
    chi_2_aux = data[c("id", var_1, var_2)]
    colnames(chi_2_aux) = c("id", "var_1", "var_2")
    chi_2_aux$var_1 = as.factor(chi_2_aux$var_1)
    chi_2_aux$var_2 = as.factor(chi_2_aux$var_2)
    
    if (nrow(na.omit(chi_2_aux)) == 0) {
      chi_2$st[n_row] = NA
      chi_2$df[n_row] = NA
      chi_2$p[n_row] = NA
    } else {
      
      chi_2_test_aux = chisq.test(chi_2_aux$var_1, chi_2_aux$var_2)
      
      n_row = which(chi_2$V1 == var_1 & chi_2$V2 == var_2)
      
      chi_2$st[n_row] = chi_2_test_aux$statistic
      chi_2$df[n_row] = chi_2_test_aux$parameter
      chi_2$p[n_row] = round(chi_2_test_aux$p.value, 6)
    }
    
  }
  # Guardar en Latex
  chi_2_ltx = kable(chi_2, caption = "Test de Chi-2", format = "latex")
  writeLines(chi_2_ltx, output_tex)
  
  # Guardar en Excel
  writexl::write_xlsx(as.data.frame(chi_2), output_xlsx)
  return(as.data.frame(chi_2))
}
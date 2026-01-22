############################################################
## Predicción de probabilidades por perfil (MNL – Cali)   ##
############################################################

library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(stringr)
library(writexl)

# ==========================================================
# 1) Crear una fila newdata completa para predict()
# ==========================================================
make_newdata_row <- function(df_base, overrides) {
  
  nd <- df_base %>% slice(1)
  
  for (v in names(df_base)) {
    
    if (v %in% c("medio", "id")) next
    
    if (is.numeric(df_base[[v]])) {
      nd[[v]] <- mean(df_base[[v]], na.rm = TRUE)
      
    } else {

      x <- df_base[[v]]
      if (is.factor(x)) {
        tab <- sort(table(x), decreasing = TRUE)
        nd[[v]] <- factor(names(tab)[1], levels = levels(x))
      } else {
        tab <- sort(table(x), decreasing = TRUE)
        nd[[v]] <- names(tab)[1]
      }
    }
  }

  for (nm in names(overrides)) {
    if (!nm %in% names(nd)) stop(paste("Variable no existe en df.mnl:", nm))
    
    if (is.factor(df_base[[nm]])) {
      val <- overrides[[nm]]
      if (!val %in% levels(df_base[[nm]])) {
        stop(paste0("Nivel no válido para ", nm, ": '", val, "'. Niveles: ",
                    paste(levels(df_base[[nm]]), collapse = " | ")))
      }
      nd[[nm]] <- factor(val, levels = levels(df_base[[nm]]))
    } else {
      nd[[nm]] <- overrides[[nm]]
    }
  }
  
  nd
}

# ==========================================================
# 2) Helper: predecir probs + armar tabla “modo/prob”
# ==========================================================
predict_profile <- function(model, df_base, profile_name, overrides) {
  
  nd <- make_newdata_row(df_base, overrides)
  
  pr <- predict(model, newdata = nd, type = "probs")
  
  if (is.matrix(pr)) pr <- as.numeric(pr[1, ])
  names_pr <- colnames(predict(model, newdata = nd, type = "probs"))
  if (is.null(names_pr)) names_pr <- names(predict(model, newdata = nd, type = "probs"))
  
  out <- tibble(
    modo = names_pr,
    prob = as.numeric(pr),
    perfil = profile_name
  ) %>%
    arrange(desc(prob))
  
  meta_vars <- intersect(
    c("edad_r2","p40","educ_3cat","p23_agr5","dist_recod","sitlab","p9_estrato3"),
    names(nd)
  )
  
  meta <- nd %>%
    select(any_of(meta_vars)) %>%
    mutate(across(everything(), ~ as.character(.x)))
  
  bind_cols(out, meta[rep(1, nrow(out)), , drop = FALSE])
}

# ==========================================================
# 3) Define perfiles (Mujer y Hombre)
#    OJO: ajusta los niveles EXACTOS según tus factores:
#    levels(df.mnl$edad_r2), levels(df.mnl$educ_3cat), etc.
# ==========================================================

# (Opcional) mirar niveles para no fallar:
levels(df.mnl$edad_r2)
levels(df.mnl$p40)
levels(df.mnl$educ_3cat)
levels(df.mnl$p23_agr5)
levels(df.mnl$dist_recod)
levels(df.mnl$sitlab)
levels(df.mnl$p9_estrato3)

# ---- Perfil 1 (base): “35, profesional/terciaria, laboral, >12 km, estrato alto, asalariado/indep”
p1_common <- list(
  edad_r2   = "35 - 54 años",
  educ_3cat = "Terciaria",
  p23_agr5  = "Trabajo",
  dist_recod= "Más de 12 km",
  sitlab    = "Asalariado o independiente",
  p9_estrato3 = "Alto"
)

p1_mujer <- c(p1_common, list(p40 = "Mujer"))
p1_hombre<- c(p1_common, list(p40 = "Hombre"))

# ---- Perfil 2 (base): “60, secundaria, personal, 4-12 km, estrato bajo, tdom no remunerado”
p2_common <- list(
  edad_r2   = "55 - 80 años",
  educ_3cat = "Secundaria",
  p23_agr5  = "Tiempo personal",
  dist_recod= "Entre 4 y 12 km",
  sitlab    = "Trabajo doméstico no remunerado",
  p9_estrato3 = "Bajo"
)

p2_mujer <- c(p2_common, list(p40 = "Mujer"))
p2_hombre<- c(p2_common, list(p40 = "Hombre"))

# ==========================================================
# 4) Predicciones
# ==========================================================
tables <- list(
  
  "P1_Mujer__SINcomunas"  = predict_profile(mnl.ctrl2_sin, df.mnl,
                                            "Perfil 1 – Mujer (35, Terciaria, Trabajo, >12km, Alto, Asal/Ind)",
                                            p1_mujer),
  
  "P1_Hombre__SINcomunas" = predict_profile(mnl.ctrl2_sin, df.mnl,
                                            "Perfil 1 – Hombre (35, Terciaria, Trabajo, >12km, Alto, Asal/Ind)",
                                            p1_hombre),
  
  "P2_Mujer__SINcomunas"  = predict_profile(mnl.ctrl2_sin, df.mnl,
                                            "Perfil 2 – Mujer (60, Secundaria, Personal, 4-12km, Bajo, Dom no rem)",
                                            p2_mujer),
  
  "P2_Hombre__SINcomunas" = predict_profile(mnl.ctrl2_sin, df.mnl,
                                            "Perfil 2 – Hombre (60, Secundaria, Personal, 4-12km, Bajo, Dom no rem)",
                                            p2_hombre)
)

# (Opcional) redondear probs
tables <- lapply(tables, \(x) x %>% mutate(prob = round(prob, 3)))

# Exportar
out_dir <- "201025_Results_Cali\\MNL"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write_xlsx(tables, path = file.path(out_dir, "predicciones_perfiles_MNL_Cali.xlsx"))
message("Listo: ", file.path(out_dir, "predicciones_perfiles_MNL_Cali.xlsx"))


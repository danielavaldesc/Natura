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

# ==========================================================
# 3) Perfiles específicos (categorías seleccionadas)
# ==========================================================

# ---- Perfil A
pA_common <- list(
  edad_r2     = "18 - 34 años",
  educ_3cat   = "Primaria o menos",
  p23_agr5    = "Trabajo",
  dist_recod  = "Entre 4 y 12 km",
  sitlab      = "Asalariado o independiente",
  p9_estrato3 = "Bajo"
)

pA_mujer  <- c(pA_common, list(p40 = "Mujer"))
pA_hombre <- c(pA_common, list(p40 = "Hombre"))


# ---- Perfil B
pB_common <- list(
  edad_r2     = "18 - 34 años",
  educ_3cat   = "Secundaria",
  p23_agr5    = "Salud",
  dist_recod  = "Entre 4 y 12 km",
  sitlab      = "Trabajo doméstico no remunerado",
  p9_estrato3 = "Medio"
)

pB_mujer  <- c(pB_common, list(p40 = "Mujer"))
pB_hombre <- c(pB_common, list(p40 = "Hombre"))


# ==========================================================
# 4) Predicciones
# ==========================================================

tables <- list(
  
  "PerfilA_Mujer__SINcomunas" = predict_profile(
    mnl.ctrl2_sin, df.mnl,
    "Perfil A – Mujer (18-34, Primaria o menos, Trabajo, 4-12km, Bajo, Asal/Ind)",
    pA_mujer
  ),
  
  "PerfilA_Hombre__SINcomunas" = predict_profile(
    mnl.ctrl2_sin, df.mnl,
    "Perfil A – Hombre (18-34, Primaria o menos, Trabajo, 4-12km, Bajo, Asal/Ind)",
    pA_hombre
  ),
  
  "PerfilB_Mujer__SINcomunas" = predict_profile(
    mnl.ctrl2_sin, df.mnl,
    "Perfil B – Mujer (18-34, Secundaria, Salud, 4-12km, Medio, Dom no rem)",
    pB_mujer
  ),
  
  "PerfilB_Hombre__SINcomunas" = predict_profile(
    mnl.ctrl2_sin, df.mnl,
    "Perfil B – Hombre (18-34, Secundaria, Salud, 4-12km, Medio, Dom no rem)",
    pB_hombre
  )
)

# Redondear probabilidades
tables <- lapply(tables, \(x) x %>% mutate(prob = round(prob, 3)))

# Exportar
out_dir <- "201025_Results_Cali\\MNL"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write_xlsx(
  tables,
  path = file.path(out_dir, "predicciones_perfiles_MNL_Cali.xlsx")
)

message("Listo: ", file.path(out_dir, "predicciones_perfiles_MNL_Cali.xlsx"))
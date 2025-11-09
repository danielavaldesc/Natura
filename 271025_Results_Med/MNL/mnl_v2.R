# ================================
#  MODELO LOGIT MULTINOMIAL – MEDELLÍN (versión estable y depurada)
# ================================

suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(tidyr); library(tibble)
  library(nnet);  library(stargazer); library(writexl); library(stringr)
})

# --------------------------
# 1) CARGA DE ARCHIVOS
# --------------------------
ruta_entrada <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Natura\\271025_Results_Med\\output\\input_famd_med_29102025.xlsx"
ruta_salida  <- "C:/Users/danie/OneDrive/Escritorio/Natura/271025_Results_Med/MNL"
dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

base <- read_excel(ruta_entrada)

# --------------------------
# 2) RECODIFICACIÓN DE VARIABLES
# --------------------------
# Ocupación: mover “Otro” a “Desocupado o inactivo”
if ("p7_agregado" %in% names(base)) {
  base$p7_agregado <- as.character(base$p7_agregado)
  base$p7_agregado[base$p7_agregado == "Otro"] <- "Desocupado o inactivo"
  base$p7_agregado <- factor(base$p7_agregado)
}

# Motivo del viaje: agrupar todas las variantes de “Cuidado y familia (…)”
if ("p23_agregado" %in% names(base)) {
  base$p23_agregado <- as.character(base$p23_agregado)
  base$p23_agregado[grepl("^Cuidado y familia", base$p23_agregado)] <- "Cuidado y familia"
  base$p23_agregado <- factor(base$p23_agregado)
}

# Género: eliminar respuestas no representativas
if ("p40" %in% names(base)) {
  base <- subset(base, !(p40 %in% c("Otro", "Prefiere no responder", "Otras identidades de género")))
  base$p40 <- droplevels(as.factor(base$p40))
}

# --------------------------
# 3) AGRUPAR CATEGORÍAS CON MUY POCAS RESPUESTAS
# --------------------------
variables_categoricas <- c(
  "edad_r2","p3_agregado","p5_agregado","p7_agregado","p9_estrato3",
  "p12_dificultad_binaria","p40","p15_autos_agregado","p16_motos_agregado",
  "p19comuna","p22","p23_agregado","p38p38_dummy"
)

umbral_minimo <- 10  # Mínimo de casos por categoría
registro_agrupaciones <- list()

agrupar_categorias_pequenas <- function(x, nombre_variable){
  if (!is.factor(x)) x <- factor(x)
  conteo <- sort(table(x), decreasing = TRUE)
  categorias_pequenas <- names(conteo[conteo < umbral_minimo])
  if (length(categorias_pequenas) > 0 && length(levels(x)) > 2) {
    nuevo_nombre <- "Otros"
    while (nuevo_nombre %in% levels(x)) nuevo_nombre <- paste0(nuevo_nombre, "_")
    x <- forcats::fct_collapse(x, !!nuevo_nombre := categorias_pequenas)
    registro_agrupaciones[[length(registro_agrupaciones)+1]] <<- 
      tibble(variable = nombre_variable, categorias_unidas = paste(categorias_pequenas, collapse = " | "), nuevo_grupo = nuevo_nombre)
  }
  droplevels(x)
}

for (v in variables_categoricas) if (v %in% names(base)) base[[v]] <- agrupar_categorias_pequenas(base[[v]], v)

# --------------------------
# 4) CREAR VARIABLES DUMMY
# --------------------------
crear_dummies <- function(df, var) {
  df %>%
    mutate(!!var := paste(var, .data[[var]], sep = "_")) %>%
    mutate(valor = 1) %>%
    pivot_wider(names_from = !!rlang::sym(var), values_from = valor, values_fill = 0)
}

for (v in variables_categoricas) if (v %in% names(base)) base <- crear_dummies(base, v)
if ("id" %in% names(base)) base <- subset(base, select = -id)

# --------------------------
# 5) DEPURAR VARIABLES DUMMY SIN INFORMACIÓN
# --------------------------
patron <- paste0("^(", paste(variables_categoricas, collapse="|"), ")_")
dummies <- names(base)[grepl(patron, names(base))]

conteo_1 <- colSums(base[, dummies, drop = FALSE] == 1, na.rm = TRUE)
sin_datos <- names(conteo_1[conteo_1 == 0])
if (length(sin_datos)) base <- base[, setdiff(names(base), sin_datos)]

# Eliminar dummies con menos de 5 casos en alguna categoría del resultado
minimo_categoria <- 5
variables_eliminadas <- c()
niveles_medio <- unique(as.character(base$medio))

for (dc in names(base)[grepl(patron, names(base))]) {
  valido <- TRUE
  for (nivel in niveles_medio) {
    n_casos <- sum(base[[dc]] == 1 & base$medio == nivel, na.rm = TRUE)
    if (n_casos < minimo_categoria) { valido <- FALSE; break }
  }
  if (!valido) variables_eliminadas <- c(variables_eliminadas, dc)
}
if (length(variables_eliminadas)) base <- base[, setdiff(names(base), variables_eliminadas)]

# Guardar registro de depuración
reporte_control_calidad <- list(
  "categorias_unidas" = if (length(registro_agrupaciones)) bind_rows(registro_agrupaciones) else tibble(),
  "sin_datos"  = tibble(variable = sin_datos),
  "eliminadas_por_baja_frecuencia" = tibble(variable = variables_eliminadas)
)
writexl::write_xlsx(reporte_control_calidad, file.path(ruta_salida, "Control_de_Calidad_Modelo.xlsx"))

# --------------------------
# 6) VARIABLE DEPENDIENTE Y REFERENCIA
# --------------------------
if (!("Moto privada" %in% unique(as.character(base$medio))))
  stop("No se encuentra la categoría de referencia 'Moto privada' en la variable 'medio'.")
base$medio <- relevel(factor(base$medio), ref = "Moto privada")

# --------------------------
# 7) FORMULACIÓN AUTOMÁTICA DEL MODELO
# --------------------------
vars_continuas <- intersect(c(
  "p24","p28_importancia_costo_compra","p28_importancia_costo_uso",
  "p28_importancia_comodidad","p28_importancia_tiempo",
  "p28_importancia_riesgo_robo","p28_importancia_riesgo_acoso",
  "p28_importancia_discriminacion","p28_importancia_emisiones",
  "p28_importancia_siniestralidad","p32_contaminacion_likert",
  "p36_influencia_amigos","p37_influencia_familia","tiempo_total","p1edad"
), names(base))

formatear <- function(x) sprintf("`%s`", x)
todas_dummies <- names(base)[grepl(patron, names(base))]
formula_con_comunas <- as.formula(paste("medio ~", paste(c(formatear(todas_dummies), vars_continuas), collapse = " + ")))
formula_sin_comunas <- as.formula(paste("medio ~", paste(c(formatear(todas_dummies[!grepl("^p19comuna_", todas_dummies)]), vars_continuas), collapse = " + ")))

# --------------------------
# 8) MODELO LOGIT MULTINOMIAL
# --------------------------
set.seed(123)
modelo_con_comunas  <- nnet::multinom(formula_con_comunas,  data = base, trace = FALSE, decay = 1e-3, MaxNWts = 30000)
modelo_sin_comunas  <- nnet::multinom(formula_sin_comunas,  data = base, trace = FALSE, decay = 1e-3, MaxNWts = 30000)

# --------------------------
# 9) TABLAS DE RESULTADOS (OR, IC, z, p)
# --------------------------
crear_tabla_OR <- function(modelo){
  sm  <- summary(modelo)
  bet <- sm$coefficients; se <- sm$standard.errors
  coef_long <- bet %>% as.data.frame() %>% rownames_to_column("categoria") %>%
    pivot_longer(-categoria, names_to = "variable", values_to = "estimacion")
  se_long   <- se %>% as.data.frame() %>% rownames_to_column("categoria") %>%
    pivot_longer(-categoria, names_to = "variable", values_to = "error_estandar")
  resultado <- left_join(coef_long, se_long, by = c("categoria","variable")) %>%
    mutate(z = estimacion / error_estandar,
           p = 2 * pnorm(abs(z), lower.tail = FALSE),
           OR = exp(estimacion),
           IC_inferior = exp(estimacion - 1.96 * error_estandar),
           IC_superior = exp(estimacion + 1.96 * error_estandar)) %>%
    dplyr::select(categoria, variable, OR, IC_inferior, IC_superior, z, p)
  list(largo = resultado %>% mutate(across(c(OR, IC_inferior, IC_superior), ~round(.x,3)),
                                    z = round(z,3), p = round(p,4)),
       ancho = exp(coef(modelo)) %>% as.data.frame() %>% round(3))
}

resultados_con  <- crear_tabla_OR(modelo_con_comunas)
resultados_sin  <- crear_tabla_OR(modelo_sin_comunas)

writexl::write_xlsx(list("Resultados_Detallados" = resultados_con$largo, "Resumen_OR" = resultados_con$ancho),
                    file.path(ruta_salida, "Modelo_Medellin_ConComunas.xlsx"))
writexl::write_xlsx(list("Resultados_Detallados" = resultados_sin$largo, "Resumen_OR" = resultados_sin$ancho),
                    file.path(ruta_salida, "Modelo_Medellin_SinComunas.xlsx"))

# --------------------------
# 10) TABLAS STARGAZER 
# --------------------------
stargazer(modelo_con_comunas, type="text",
          title="Modelo Logit Multinomial – Medellín (con comunas)",
          single.row=TRUE, out=file.path(ruta_salida,"Resumen_ConComunas.txt"))
stargazer(modelo_sin_comunas, type="text",
          title="Modelo Logit Multinomial – Medellín (sin comunas)",
          single.row=TRUE, out=file.path(ruta_salida,"Resumen_SinComunas.txt"))

cat("\n✅ Archivos generados en:", normalizePath(ruta_salida),
    "\n- Control_de_Calidad_Modelo.xlsx",
    "\n- Modelo_Medellin_ConComunas.xlsx",
    "\n- Modelo_Medellin_SinComunas.xlsx",
    "\n- Resumen_ConComunas.txt",
    "\n- Resumen_SinComunas.txt\n")

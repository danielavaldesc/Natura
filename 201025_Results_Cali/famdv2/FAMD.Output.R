
# Librerías
library(tidyverse)

# Definir directorio
setwd("C:/Users/Portatil/Desktop/Natura/201025_Results_Cali/")

# Cargar base de datos
dataset = readxl::read_excel("output/input_famd_cali_29102025.xlsx")


vars_dummy <- c(
  "edad_r2","p3_agregado","p5_agregado","p7_agregado","p9_estrato3",
  "p12_dificultad_binaria",
  "p40","p13","p14","p15_autos_agregado","p16_motos_agregado",
  "p19comuna","p22","p23_agregado",
  "p38p38_dummy")

vars_cont <- c("p28_importancia_costo_compra",
                 "p28_importancia_costo_uso",
                 "p28_importancia_comodidad",
                 "p28_importancia_tiempo",
                 "p28_importancia_riesgo_robo",
                 "p28_importancia_riesgo_acoso",
                 "p28_importancia_discriminacion",
                 "p28_importancia_emisiones",
                 "p28_importancia_siniestralidad",
                 "p32_contaminacion_likert",
                 "p36_influencia_amigos",
                 "p37_influencia_familia",
                 "tiempo_total",
                 "p1edad")


dataset <- dataset %>%
  dplyr::select(c("medio",vars_dummy, vars_cont)) %>%
  dplyr::mutate(
    across(all_of(vars_dummy), ~ as.factor(.x)),    # Categóricas a factor
    across(all_of(vars_cont), ~ as.numeric(.x))   # Continuas a numérico
  )  

#----------------------------------------------------------------------------#
# Análisis preliminar: coeficiente de correlación o razón de correlación     #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------#

# Creación de la función para la creación de la matriz
corr_f <- function(df){
  # crear output
  cor_out <- matrix(NA, nrow = length(colnames(df)), ncol = length(colnames(df)))
  colnames(cor_out) <- colnames(df); rownames(cor_out) <- colnames(df)
  for (i in colnames(df)) {
    for (j in colnames(df)) {
      print(paste0(i, " vs ", j))
      df.aux <- df %>% dplyr::select(i,j)
      
      if (ncol(df.aux) ==  1) {
        df.aux <- cbind(df.aux, df.aux)
        colnames(df.aux) <- c("x","y")
      } else{colnames(df.aux) <- c("x","y")}
      
      
      nrow <- which(rownames(cor_out) == i)
      ncol <- which(colnames(cor_out) == j)
      
      if (i == j & class(df.aux$x) != "numeric") {
        cor_out[nrow,ncol] = length(levels(as.factor(df.aux$x))) - 1
      }
      
      if (i == j & class(df.aux$x) == "numeric") {
        cor_out[nrow,ncol] = 1
      }
      
      if (i != j & (class(df.aux$x) == "numeric" & class(df.aux$y) == "numeric")) {
        cor_out[nrow,ncol] = cor(df.aux$x, df.aux$y)
        cor_out[ncol, nrow] =  cor(df.aux$x, df.aux$y)
      }
      library(sjstats)
      if (i != j & (class(df.aux$x) != "numeric" & class(df.aux$y) == "numeric")) {
        cor_out[nrow,ncol] = anova_stats(car::Anova(aov(
          y ~ as.factor(x),
          data = df.aux
        ), type = 2))$etasq[1]
        cor_out[ncol, nrow] =  anova_stats(car::Anova(aov(
          y ~ as.factor(x),
          data = df.aux
        ), type = 2))$etasq[1]
      }
      
      if (i != j & (class(df.aux$y) != "numeric" & class(df.aux$x) == "numeric")) {
        cor_out[nrow,ncol] = anova_stats(car::Anova(aov(
          x ~ as.factor(y),
          data = df.aux
        ), type = 2))$etasq[1]
        cor_out[ncol, nrow] =  anova_stats(car::Anova(aov(
          x ~ as.factor(y),
          data = df.aux
        ), type = 2))$etasq[1]
      }
      library(rcompanion)
      if (i != j & (class(df.aux$y) != "numeric" & class(df.aux$x) != "numeric")) {
        cor_out[nrow,ncol] <- cramerV(table(df.aux$x, df.aux$y))
        cor_out[ncol, nrow] <- cramerV(table(df.aux$x, df.aux$y))
      }
      
      
    }
    
  }
  return(cor_out)
}

# Usar la función sin añadir id
relationship <- corr_f(dataset %>% dplyr::select(-id, medio))

writexl::write_xlsx(as.data.frame(relationship), "famdv2/relationship_famd_29102025.xlsx")

#----------------------------------------------------------------------------#
# Análisis Factorial para Datos Mixtos (FAMD) para p variables explicativas  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------#
# Eliminar id
famd.dataset <- dataset %>% dplyr::select(-c("medio", "p39_lugar_agregado_mod")) 

##########  Implementar FAMD
library(FactoMineR)
library(factoextra)
set.seed(291025)
model <-FAMD(famd.dataset, graph = FALSE, ncp = 45)
# model <-FAMD(famd.dataset, graph = TRUE, ncp = 20)

###### Primero, examinar los valores propios y la proporción de varianza (inercia) explicada
model$eig
plot_eig = fviz_eig(model,choice='eigenvalue', geom='line') + theme_bw()

ggsave("famdv2/plot_eigenval_famd_29102025.png", plot = plot_eig, width = 6, height = 4, dpi = 300)

# Computar matriz de correlación siguiendo a Páges:
# En sus entradas para variables cuantitativas, se calcula la correlación
# En sus entradas para variables cuantitativas y cualitativas, se calcula el eta2

library(paran)
library(recipes)
rec <- recipe(medio ~ ., dataset) %>%
  step_dummy(all_nominal_predictors(), one_hot = T)

paran.dataset <- rec %>% prep() %>% juice() %>% as.data.frame() %>% dplyr::select(-medio)

cat.index <- 32:ncol(paran.dataset)

for (j in setdiff(1:ncol(paran.dataset), cat.index)) {
  paran.dataset[,j] = scale(paran.dataset[,j], center = T, scale = T)
}

for (i in cat.index) {
  paran.dataset[,i] = scale(paran.dataset[,i]/sqrt(nrow(paran.dataset)/sum(paran.dataset[,i])),
                            center = T, scale = F)
}

# Método paralelo de Horn
set.seed(123)
paran <- paran::paran(paran.dataset,
                      iterations = 5000)

writexl::write_xlsx(data.frame(pc = 1:23, AdjEV = paran$AdjEv[1:23], 
                               UnadjEV = paran$Ev[1:23], Bias = paran$Bias[1:23]),
                    "famdv2/corr_horn_paran_29102025.xlsx")

# REVISAR: LOS VALORES PROPIOS NO AJUSTADOS NO SON IDÉNTICOS A LOS OBTENIDOS DE model$eig
# PROBLEMA: ESTAMOS TOMANDO MAL LA DESCOMPOSICIÓN SVD (NO TENGO DOCUMENTACIÓN SOBRE ESA SALIDA)

###### Segundo, examinar la relación entre las variables cuantitativas
# Note: PDs are linear combinatios of the original variables.
# The factor loading of a variable describes the correlation
# between it and a given PD
# Squared factor loading correspond to squared cosine (cos2)
# This provides a measure of the proportion of variance in a variable that is

# captures by a particular PD

library(PCAmixdata)
split <- splitmix(famd.dataset)
res.pcamix <- PCAmixdata::PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali, ndim = 23, 
                     rename.level = T)

plot(res.pcamix, choice="cor") 

round(res.pcamix$quanti.cor, 2)

# Nótese que el círculo de correlación en FAMD viene dado por
quantvar = round(model$quanti.var$coord[,1:23], 2)
quantvar = cbind(rownames(quantvar), quantvar)

writexl::write_xlsx(quantvar %>%
                      as.data.frame(),
                    "famdv2/quanti_var_cor_29102025.xlsx")

###### Tercero, squared loading plots: it allow us to visualize
# qualitative and quantitative variables in the new feature space
# "According to the authors of the package, the coordinates are to be interpreted
# as measuring “the links (signless) between variables and principal components”
library(factoextra)
p <- fviz_famd_var(model, 'var', 
                   axes = c(1,2),
                   col.var = 'cos2')
ggsave("famdv2/plot_cos2_famd_29102025.png",
       plot = p, width = 12, height = 12, dpi = 300)


fviz_add(p, model$var$coord,
         col.var = 'cos2')
# Esto corresponde a:
coord_var = round(model$var$coord[,1:7],4)
coord_var = cbind(rownames(coord_var), coord_var)

writexl::write_xlsx(coord_var %>%
                      as.data.frame(),
                    "famdv2/coord_var_29102025.xlsx")

# Ad hoc: coordenadas individuos
library(plotly)
df_ind <- as.data.frame(model$ind$coord[, 1:3])
df_ind$p17_modo_agregado <- model$call$X$p17_modo_agregado

p3d <- plot_ly(
  data = df_ind,
  x = ~Dim.1,
  y = ~Dim.2,
  z = ~Dim.3,
  color = ~p17_modo_agregado,
  colors = "Set1",
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.85)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Dim 1", range = c(-5, 5)),
      yaxis = list(title = "Dim 2", range = c(-5, 5)),
      zaxis = list(title = "Dim 3")
    ),
    legend = list(title = list(text = "Modo"))
  )

p3d


###### Cuarto, whereas factor loading and squared loading measure shows how well
# a given PD describes variation capture in a variable
# Contribution describes the converse: how much a variable accounts for the
# total variation captured by a PD.
library(ggpubr)
contrib = ggarrange(fviz_contrib(model, choice = "var", axes = 1),
          fviz_contrib(model, choice = "var", axes = 2),
          fviz_contrib(model, choice = "var", axes = 3),
          fviz_contrib(model, choice = "var", axes = 4), nrow = 2, ncol = 2)

ggsave("famdv2/plot_contrib_famd_29102025.png",
       plot = contrib, width =12, height = 12, dpi = 300)



# Quinto, Varimax rotation:
# To facilitate interpretation of the relationships between variables and PCs,
# additional rotation can be applied to PCs to result in high factor loadings
# for a few variables and low fator loadings for the rest.
# Other words: a small number of variables will become highly correlated with each PC.
# We used the most common form of rotation (Varimax rotation), a generalized form
# of which is implemented in the PCAmixdata package for mixed data
pd.rot <- PCArot(res.pcamix, dim=23,
                 graph=FALSE)

plot_rot = plot(pd.rot, choice="sqload", 
     coloring.var=TRUE, axes=c(1, 2))


ggsave("famdv2/plot_sqload_rotation_famd_29102025.png",
       plot = plot_rot, width = 12, height = 12, dpi = 300)



# Squared loadings sobre los ejes rotados
round(pd.rot$sqload, 2)

writexl::write_xlsx(cbind(variable = rownames(pd.rot$sqload),as.data.frame(round(pd.rot$sqload,4))),
                    "famdv2/correlations_famd_29102025.xlsx")

round(pd.rot$sqload[,1][pd.rot$sqload[,1] > 0.4], 3)
round(pd.rot$sqload[,2][pd.rot$sqload[,2] > 0.4], 3)
round(pd.rot$sqload[,3][pd.rot$sqload[,3] > 0.4], 3)
round(pd.rot$sqload[,4][pd.rot$sqload[,4] > 0.4], 3)
round(pd.rot$sqload[,5][pd.rot$sqload[,5] > 0.4], 3)
round(pd.rot$sqload[,6][pd.rot$sqload[,6] > 0.4], 3)
round(pd.rot$sqload[,7][pd.rot$sqload[,7] > 0.4], 3)
round(pd.rot$sqload[,8][pd.rot$sqload[,8] > 0.4], 3)
round(pd.rot$sqload[,9][pd.rot$sqload[,9] > 0.4], 3)
round(pd.rot$sqload[,10][pd.rot$sqload[,10] > 0.4], 3)
round(pd.rot$sqload[,11][pd.rot$sqload[,11] > 0.4], 3)
round(pd.rot$sqload[,12][pd.rot$sqload[,12] > 0.4], 3)




#-------------------------------------#
# Factorial Analysis of Mixed Data    #
#-------------------------------------#

library(factoextra)
library(FactoMineR)

#------------------------------------------#
# Ejemplo 1: Páges (2002) (small dataset)  #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------#

# Páges (2002) propone un ejemplo con una base de datos
# pequeña con información biométrica

# Base de datos definida como
df <- data.frame(i = c("a", "b", "c", "d", "e", "f"),
                 Hair_Colour = c("Blonde", "Blonde", "Brown",
                                 "Brown", "Black", "Black"),
                 Height = c(1,2,3,4,5,6),
                 Weight = c(1,2,3,3,2,1))

# Variables continuas no-correlacionadas estandarizadas
df$CR_Height = c(-1.464, -0.878, -0.293, 0.293, 0.878, 1.464)
df$CR_Weight = c(-1.225, 0, 1.225, 1.225, 0, -1.225)

mean(df$CR_Height); mean(df$CR_Weight)
sd(df$CR_Height); sd(df$CR_Weight)

# Relationship matrix (squared correlation coefficient or
# Computing correlation ratio (eta squared)

z <- df %>% dplyr::select("Hair_Colour", "CR_Height", "CR_Weight")

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
relationship <- corr_f(z)
diag(relationship) <- c(1,1,1)

paran::paran(mat = as.matrix(relationship), n = nrow(z), iterations = 5000)

# Implementar FAMD
library(FactoMineR)
famd <- FAMD(df[c("Hair_Colour", "CR_Height", "CR_Weight")],
             graph = T, ncp = 5)

# Proporción de inercia (%) explicada por cada factor
famd$eig
u = famd$svd$U
d = famd$svd$vs
v = famd$svd$V

x <- u%*%diag(d[1:4])%*%t(v)

x_sd <- scale(x) %>% unlist() %>% as.data.frame()

row.w <- rep(1/nrow(x_sd), nrow(x_sd))
col.w <- rep(1,ncol(x_sd))

x_sd <- t(t(x_sd)*sqrt(col.w))*sqrt(row.w)

svd <- svd.triplet(x, ncp = 5)
svd$vs

svd.base <- svd(x_sd, nu = 5, nv = 5)
svd.base$d

paran::paran(mat = cor(x_sd), n = nrow(x_sd), iterations = 5000)


# La contribución de una variable a la inercia es interpretada
# como la medida de la relación entre la variable y el eje (dimensión)
# (squared correlation, ratio or coefficient)
round(famd$var$coord,4)

fviz_famd_var(famd, repel = TRUE)

tab_3 <- t(famd$eig[,c(2,1)])
round(rbind(tab_3, 
            famd$var$coord[,1:4]),2)

#----------------------------------------------------------#
# Ejemplo 2: IBM dataset: https://rpubs.com/nchelaru/famd  #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------#

# Cargar IBM Telco customer churn dataset to
# gain insights into the relationships between varios aspects 
# of customer behaviour
df <- read.csv('https://github.com/nchelaru/data-prep/raw/master/telco_cleaned_renamed.csv')

# Standarized of numerical variables is critical for valid results
# This is done automatically by FactoMineR and PCAmixdata

# First, we inspect the calculated principal dimensions (PC)
# which are linear combinations of the original variables
# Inspecting eigenvalues and percentage variance explained:
res.famd <- FAMD(df, 
                 sup.var = 20,  
                 graph = FALSE, 
                 ncp=25)

res.famd$eig

fviz_eig(res.famd,choice='eigenvalue', geom='line') 

# Second, plot individual observations in the new feature space
# Overlapping distributions suggested that this dataset are not suited
# or sufficient to capture the diference between customers who churn
# and those who do not.
library(plotly)
val_df <- as.data.frame(res.famd$ind)

plot_ly(cbind(df, val_df[1:3]), x = ~coord.Dim.1, 
        y = ~coord.Dim.2,  z = ~coord.Dim.3, color = ~Churn) 

# Third, to examine relationship between quantitative variables
# PDs are linear combinatios of the original variables.
# The factor loading of a variable describes the correlation
# between it and a given PD
# Squared factor loading correspond to squared cosine (cos2)
# This provides a measure of the proportion of variance in a variable that is
# captures by a particular PD
library(PCAmixdata)
split <- splitmix(df)
res.pcamix <- PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali)
plot(res.pcamix, choice="cor") 
round(res.pcamix$quanti.cor, 2)

test <- PCA(df %>% dplyr::select(Tenure, MonthlyCharges, TotalCharges))

# Nótese que el círculo de correlación en FAMD viene dado por
round(res.famd$quanti.var$coord[,1:5], 2)

# Fourth, squared loading plots: it allow us to visualize
# qualitative and quantitative variables in the new feature space
# "According to the authors of the package, the coordinates are to be interpreted
# as measuring “the links (signless) between variables and principal components”
p <- fviz_famd_var(res.famd, 'var', 
                   axes = c(1, 2),
                   col.var = 'cos2')

fviz_add(p, res.famd$var$coord.sup,
         col.var = 'cos2')

# Esto corresponde a:
round(res.famd$var$coord[,1:2],4)

# Results: 1. MonthlyCharges is more closely correlated to PD1,
#             whereas Ternure is described by a combination of PD1 and PD2
#
#          2. Contract, Internet service and Monthly Charges have the highest
#             squared loading values and so are more important in explaining the variance
#             captured by PD1 and PD2 than variables near to origin (Gender, PhoneService, SeniorCitizen)
#


# Fifth, whereas factor loading and squared loading measure shows how well
# a given PD describes variation capture in a variable
# Contribution describes the converse: how much a variable accounts for the
# total variation captured by a PD.
fviz_contrib(res.famd, choice = "var", axes = 1)
fviz_contrib(res.famd, choice = "var", axes = 2)

# Results: From the variables that meet the cut-off, we can glean some 
#          insights into what are the most important variables in this dataset,
#          such as MonthlyCharges, InternetService and Tenure

# Sixth, Varimax rotation:
# To facilitate interpretation of the relationships between variables and PCs,
# additional rotation can be applied to PCs to result in high factor loadings
# for a few variables and low fator loadings for the rest.
# Other words: a small number of variables will become highly correlated with each PC.
# We used the most common form of rotation (Varimax rotation), a generalized form
# of which is implemented in the PCAmixdata package for mixed data
# see: https://stats.stackexchange.com/questions/151653/what-is-the-intuitive-reason-behind-doing-rotations-in-factor-analysis-pca-how

# Here we see a slightly different version of the squared loading plot.
# There are higher factor loadings of MonthlyCharges and InternetService for
# the rotated PD1, and Ternure and Contract for the rotated PD2

res.pcarot <- PCArot(res.pcamix, dim=2,
                     graph=FALSE)

plot(res.pcarot, choice="sqload", 
     coloring.var=TRUE, axes=c(1, 2))

# Squared loadings sobre los ejes rotados
round(res.pcarot$sqload, 2)





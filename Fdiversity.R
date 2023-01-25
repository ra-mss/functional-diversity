#' ---
#' title: "Diversidad funcional en tres sitios del ANES"
#' date: "`r format(Sys.time(), '%d de %B de %Y')`"
#' ---

#'Borra la memoria de trabajo
rm(list=ls())

#'Sincroniza carpeta de trabajo con R
Div_fun<- "/home/ra_mss/Desktop/R_Projects/P6" 

#'###Importación la base de datos 
#'Dos bases de datos: datos de abundancia y de categorías (tamaño, color, tipo de alimentación, etc.)  
#' -Accede a la carpeta de los datos (archivos .txt)
setwd( paste(Div_fun,"/FishData", sep="") )
 
#' SITIO 1
#' -Importa valores de abundancia de spp desde el archivo .txt para el sitio 1. El nombre de spp en columnas y el sitio en filas
PecesFM<-read.table("AbundanciaFM.txt", header=T, row.names = 1)
 
#' -Importa valores de categorías (rasgos) de spp. El nombre de las spp en la columna “Especie” 
Cat_FM<-read.table("CategFM.txt", header=T, row.names = "Especie")
 
#'Comproba que nombres de spp son iguales en ambas matrices
sum( row.names(Cat_FM) %in% colnames(PecesFM) ) == ncol(PecesFM)

#'Observamos la codificación de rasgos después de importar los datos
Cat_FM
#'Algunos rasgos son categorías y otros ordinales
summary(Cat_FM)

#'Almacena y ordena los rasgos
Categ_PecesFM<-as.data.frame( matrix(NA, nrow(Cat_FM), ncol(Cat_FM), 
                                 dimnames=list( row.names(Cat_FM), names(Cat_FM) ) ))  

#'Convierte valores ordinales a valores ordenados 
Categ_PecesFM[,"Talla"]<-factor(Cat_FM[,"Talla"], 
                               levels=c("1", 
                                        "2", 
                                        "3",
                                        "4", 
                                        "5"), 
                               ordered = TRUE )

Categ_PecesFM[,"Movilidad"]<-factor(Cat_FM[,"Movilidad"],
                                    levels=c("1", 
                                             "2", 
                                             "3"), 
                                    ordered = TRUE )

Categ_PecesFM[,"Actividad"]<-factor(Cat_FM[,"Actividad"])

Categ_PecesFM[,"Agrupamiento"]<-factor(Cat_FM[,"Agrupamiento"], 
                                  levels=c("1", 
                                           "3", 
                                           "4" ), 
                                  ordered = TRUE )

Categ_PecesFM[,"Posicion"]<-as.numeric(Cat_FM[,"Posicion"])

Categ_PecesFM[,"Dieta"]<-factor(Cat_FM[,"Dieta"])

#'Antes y después de la conversion
is.ordered(Cat_FM[,"Talla"])
is.ordered(Categ_PecesFM[,"Talla"])

#'Activa el paquete FD
#'install.packages("FD")
library("FD") 

#'Ver de manera general la distancia (disimilitud) entre spp de peces (menor es el valor, menos diferentes son). gowdis: para medir la distancia entre spp
DF_pecesfm <- gowdis(Categ_PecesFM)

#'Calcula el # de grupos que se forman mediante función dbFD. Otros métodos: ward, single, complete, average, etc.
clus_singlefm<- dbFD(Categ_PecesFM, PecesFM, calc.FGR = TRUE, clust.type = "single", corr = ("lingoes")) 

clus_wardfm<- dbFD(Categ_PecesFM, PecesFM, calc.FGR = TRUE, clust.type = "ward.D", corr = ("cailliez")) 

clus_completefm<- dbFD(Categ_PecesFM, PecesFM, calc.FGR = TRUE, clust.type = "complete", corr = ("cailliez")) 

#' -Indices funcionales: 
#' Riqueza funcional
clus_wardfm$FRic

#' Equidad funcional
clus_wardfm$FEve

#' Dispersión funcional
clus_wardfm$FDis

#' Riqueza de grupos funcionales
clus_wardfm$FGR

#'SITIO 2
#'Base de datos 
PecesLB<-read.table("AbundanciaLB.txt", header=T, row.names = 1)

#' -Importa valores de categorías de spp. 
Cat_LB<-read.table("CategLB.txt", header=T, row.names = "Especie")

#'Comprueba que los nombres sean iguales
sum( row.names(Cat_LB) %in% colnames(PecesLB) ) == ncol(PecesLB)

#'Algunos rasgos son categorías y otros ordinales
summary(Cat_LB)

#'Almacena y ordena los rasgos
Categ_PecesLB<-as.data.frame( matrix(NA, nrow(Cat_LB), ncol(Cat_LB), 
                                     dimnames=list( row.names(Cat_LB), names(Cat_LB) ) ))  

#'Convierte valores ordinales a valores ordenados 
Categ_PecesLB[,"Talla"]<-factor(Cat_LB[,"Talla"], 
                                levels=c("1", 
                                         "2", 
                                         "3",
                                         "4", 
                                         "5",
                                         "6"), 
                                ordered = TRUE )

Categ_PecesLB[,"Movilidad"]<-factor(Cat_LB[,"Movilidad"],
                                    levels=c("1", 
                                             "2", 
                                             "3",
                                             "4"), 
                                    ordered = TRUE )

Categ_PecesLB[,"Actividad"]<-factor(Cat_LB[,"Actividad"])

Categ_PecesLB[,"Agrupamiento"]<-factor(Cat_LB[,"Agrupamiento"], 
                                       levels=c("1", 
                                                "3", 
                                                "4" ), 
                                       ordered = TRUE )

Categ_PecesLB[,"Posicion"]<-as.numeric(Cat_LB[,"Posicion"])

Categ_PecesLB[,"Dieta"]<-factor(Cat_LB[,"Dieta"])

#'Antes y después de la conversion
is.ordered(Cat_LB[,"Talla"])
is.ordered(Categ_PecesLB[,"Talla"])

#'Grupos funcionales
clus_singlelb<- dbFD(Categ_PecesLB, PecesLB, calc.FGR = TRUE, clust.type = "single", corr = ("cailliez")) 

clus_wardlb<- dbFD(Categ_PecesLB, PecesLB, calc.FGR = TRUE, clust.type = "ward.D", corr = ("cailliez")) 

clus_completelb<- dbFD(Categ_PecesLB, PecesLB, calc.FGR = TRUE, clust.type = "complete", corr = ("cailliez"))  

#'-Indices funcionales: 
#' Riqueza funcional
clus_completelb$FRic

#' Equidad funcional
clus_completelb$FEve

#' Dispersión funcional
clus_completelb$FDis

#' Riqueza de grupos funcionales
clus_completelb$FGR

#'SITIO 3
#'Base de datos 
PecesLI<-read.table("AbundanciaLI.txt", header=T, row.names = 1)

#' -Importa los valores de rasgo de spp 
Cat_LI<-read.table("CategLI.txt", header=T, row.names = "Especie")

#'Comprueba si los nombres son iguales
sum( row.names(Cat_LI) %in% colnames(PecesLI) ) == ncol(PecesLI)

#'Almacena y ordena los rasgos
Categ_PecesLI<-as.data.frame(matrix(NA, nrow(Cat_LI), ncol(Cat_LI), 
                                     dimnames=list( row.names(Cat_LI), names(Cat_LI))))  

#'Convierte valores ordinales a valores ordenados 
Categ_PecesLI[,"Talla"]<-factor(Cat_LI[,"Talla"], 
                                levels=c("1", 
                                         "2", 
                                         "3",
                                         "4", 
                                         "5",
                                         "6"), 
                                ordered = TRUE )

Categ_PecesLI[,"Movilidad"]<-factor(Cat_LI[,"Movilidad"],
                                    levels=c("1", 
                                             "2", 
                                             "3"), 
                                    ordered = TRUE )

Categ_PecesLI[,"Actividad"]<-factor(Cat_LI[,"Actividad"])

Categ_PecesLI[,"Agrupamiento"]<-factor(Cat_LI[,"Agrupamiento"], 
                                       levels=c("1", 
                                                "3", 
                                                "4" ), 
                                       ordered = TRUE )

Categ_PecesLI[,"Posicion"]<-as.numeric(Cat_LI[,"Posicion"])

Categ_PecesLI[,"Dieta"]<-factor(Cat_LI[,"Dieta"])

#'Antes y después de la conversion
is.ordered(Cat_LI[,"Talla"])
is.ordered(Categ_PecesLI[,"Talla"])
 
#'Grupos funcionales
clus_singleli<- dbFD(Categ_PecesLI, PecesLI, calc.FGR = TRUE, clust.type = "single", corr = ("cailliez")) 

clus_wardli<- dbFD(Categ_PecesLI, PecesLI, calc.FGR = TRUE, clust.type = "ward.D", corr = ("cailliez")) 

clus_completeli<- dbFD(Categ_PecesLI, PecesLI, calc.FGR = TRUE, clust.type = "complete", corr = ("cailliez"))  

#'-Indices funcionales:
#' Riqueza funcional
clus_wardli$FRic
 
#' Equidad funcional
clus_wardli$FEve
 
#' Dispersión funcional
clus_wardli$FDis

#' Riqueza de grupos funcionales
clus_wardli$FGR

#'Base de datos de los índices obtenidos para observar si hay diferencia entre sitios 
#'Importamos la nueva base de datos 
ind_func<- read.csv("IF.csv", header = TRUE, sep = ",", fileEncoding = "UTF-8-BOM")

library(nortest)
#'Boxplot para la riqueza funcional por sitio
boxplot(ind_func$Riq_fun~factor(ind_func$Sitio), xlab = "Sitio", ylab = "Riqueza funcional",col=c("red2", "cyan3", "gold1"), names=c("Fang Ming", "La Ballena", "Los Islotes"))

#'Test Shapiro-Wilk
shapiro.test(ind_func$Riq_fun)

#' Test Anderson-Darling
ad.test(ind_func$Riq_fun)

#'Al encontrar normalidad en nuestros datos, realiza ANOVA
anova_rf<- aov(ind_func$Riq_fun~ind_func$Sitio)
summary(anova_rf)

#'Boxplot para la equidad funcional por sitio
boxplot(ind_func$Equi_fun~factor(ind_func$Sitio), xlab = "Sitio", ylab = "Equidad funcional", col=c("red2", "cyan3", "gold1"), names=c("Fang Ming", "La Ballena", "Los Islotes"))

#'Test Shapiro-Wilk
shapiro.test(ind_func$Equi_fun)

#'Test Anderson-Darling
ad.test(ind_func$Equi_fun)

#'ANOVA
anova_ef<- aov(ind_func$Equi_fun~ind_func$Sitio)
summary(anova_ef)

#'Boxplot para la dispersión funcional por sitio
boxplot(ind_func$Disp_fun~factor(ind_func$Sitio), xlab = "Sitio", ylab = "Dispersión funcional", col=c("red2", "cyan3", "gold1"), names=c("Fang Ming", "La Ballena", "Los Islotes"))

#'Test Shapiro-Wilk
shapiro.test(ind_func$Disp_fun)

#'Test Anderson-Darling
ad.test(ind_func$Disp_fun)

#'ANOVA
anova_df<- aov(ind_func$Disp_fun~ind_func$Sitio)
summary(anova_df)

#'Boxplot para la riqueza de grupos funcionales por sitio. 
boxplot(ind_func$Riq_grup~factor(ind_func$Sitio), xlab = "Sitio", ylab = "Riqueza de grupos funcionales",col=c("red2", "cyan3", "gold1"), names=c("Fang Ming", "La Ballena", "Los Islotes"), ylim=c(1.0, 7.0))

#'Test Anderson-Darling
ad.test(ind_func$Riq_grup)

#'Test Kolmogorov-Smirnov
ks.test(ind_func$Riq_grup,"pnorm", mean = mean(ind_func$Riq_grup), sd = sd(ind_func$Riq_grup))

#'Test Shapiro-Wilk
shapiro.test(ind_func$Riq_grup)

#'ANOVA
anova_rg<- aov(ind_func$Riq_grup~ind_func$Sitio)
summary(anova_rg)

#'Si no hay normalidad, se hace una prueba no paramétrica 
#'Test Kruskal-Wallis
kruskal.test(ind_func$Riq_grup~ind_func$Sitio)

#' En caso de encontrar diferencias, usar esto:
pairwise.wilcox.test(ind_func$Riq_grup, ind_func$Sitio)

sessionInfo()
#'
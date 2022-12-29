#Rezago Habitacional, metodología CONAVI

library("ggplot2")
library("tidyverse")
library("readxl")
library("mapdata")

rm(list = ls())
#cargamos la base completa
ENIGH2020 <- read.csv("C:/Users/abiga/OneDrive/Escritorio/Prueba CONAVI/viviendas.csv")

#filtramos las variables que utilizamos para la metodología (las principales, posteriormente se peuden ir agregando las demás conforme se vayan utilizando)
Rez_Hab <- ENIGH2020 %>% select(mat_pared, mat_pisos, mat_techos)

# A) Materiales deteriorados

#Dummys de ma_pared
Rez_Hab$pared_det <- (Rez_Hab$mat_pared  <= "2")*1
Rez_Hab$pared_det <- (Rez_Hab$mat_pared  >= "4")*1
Rez_Hab$pared_det <- (Rez_Hab$mat_pared  <= "5")*1

#Dummyes de ma_techo
Rez_Hab$techo_det <- (Rez_Hab$mat_techos <= 3)*1

#Variable de material deteriorado, toma el valor de 1 cuando la variable pared_Det y/o techo_det == 1
Rez_Hab$mat_det <- (Rez_Hab$techo_det == 1 & Rez_Hab$pared_det == 1)*1

# B) Materiales regulares

#pared regular
Rez_Hab$pared_reg = (Rez_Hab$mat_pared == "3")*1
Rez_Hab$pared_reg = (Rez_Hab$mat_pared == "6")*1

#techo regular
Rez_Hab$techo_reg = (Rez_Hab$mat_techos >= 3)*1
Rez_Hab$techo_reg = (Rez_Hab$mat_techos <= 4)*1
Rez_Hab$techo_reg = (Rez_Hab$mat_techos == 7)*1
Rez_Hab$techo_reg = (Rez_Hab$mat_techos == 9)*1

#piso regular
Rez_Hab$piso_Reg = (Rez_Hab$mat_pisos = 1)*1

#Materiales regulares
Rez_Hab$mat_reg = (Rez_Hab$pared_reg == 1 & Rez_Hab$techo_reg == 1 & Rez_Hab$piso_Reg == 1)*1


# C) Precariedad de los espacios
#Residentes por cuarto
Rez_Hab$res_cuarto = (ENIGH2020$tot_resid/ENIGH2020$num_cuarto)
Rez_Hab$res_cuarto

Rez_Hab$hacin = (Rez_Hab$res_cuarto > 2.5)*1

#excusado

Rez_Hab$excu = (ENIGH2020$excusado = 1)*1

Rez_Hab$prec_esp = (Rez_Hab$hacin == 1 & Rez_Hab$excu == 1)*1

#Rezago Habitacional 
Rez_Hab$rezago = (Rez_Hab$mat_det == 1 & Rez_Hab$mat_reg == 1 & Rez_Hab$prec_esp == 1)*1

setwd( "C:/Users/abiga/OneDrive/Escritorio/Prueba CONAVI" )
write.csv( Rez_Hab, " Datos.Final.csv " )

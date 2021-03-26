library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(caTools)
library(caret)
library(Amelia)
library(ClustOfVar)
library(cluster.datasets)
library(InformationValue)
library(scales)
library(fastDummies)
library(cluster)
library(PMCMRplus)
library(nortest)
library(outliers)
library(smbinning)
library(scorecard)
library(car)
library(ROCit)
library(ROCR)
library(WriteXLS)
library(corrplot)

################# SEPARACI�N PUEBA Y ENTRENAMIENTO #############################

Datos <- read_excel("bank-additional-full.xlsx")
Datos2 <- read_excel("bank-additional-full.xlsx", sheet = "Serie")
a<-as.numeric(Datos$emp.var.rate)
b<-as.numeric(Datos$cons.conf.idx)
Datos<-select(Datos,-emp.var.rate,-cons.conf.idx)
Datos$emp.var.rate<-a
Datos$cons.conf.idx<-b
Datos <- Datos %>% mutate(y = case_when(y=="no"~0,y=="yes"~1))
Datos$serit<-Datos$monthd
Datos <- Datos %>% 
  mutate(
    serit = case_when(
      serit == 1 ~ round(Datos2$Desempleo[1],2),
      serit == 2 ~ round(Datos2$Desempleo[2],2),
      serit == 3 ~ round(Datos2$Desempleo[3],2),
      serit == 4 ~ round(Datos2$Desempleo[4],2),
      serit == 5 ~ round(Datos2$Desempleo[5],2),
      serit == 6 ~ round(Datos2$Desempleo[6],2),
      serit == 7 ~ round(Datos2$Desempleo[7],2),
      serit == 8 ~ round(Datos2$Desempleo[8],2),
      serit == 9 ~ round(Datos2$Desempleo[9],2),
      serit == 10 ~ round(Datos2$Desempleo[10],2),
      serit == 11 ~ round(Datos2$Desempleo[11],2),
      serit == 12 ~ round(Datos2$Desempleo[12],2),
      serit == 13 ~ round(Datos2$Desempleo[13],2),
      serit == 14 ~ round(Datos2$Desempleo[14],2),
      serit == 15 ~ round(Datos2$Desempleo[15],2),
      serit == 16 ~ round(Datos2$Desempleo[16],2),
      serit == 17 ~ round(Datos2$Desempleo[17],2),
      serit == 18 ~ round(Datos2$Desempleo[18],2),
      serit == 19 ~ round(Datos2$Desempleo[19],2),
      serit == 20 ~ round(Datos2$Desempleo[20],2),
      serit == 21 ~ round(Datos2$Desempleo[21],2),
      serit == 22 ~ round(Datos2$Desempleo[22],2),
      serit == 23 ~ round(Datos2$Desempleo[23],2),
      serit == 24 ~ round(Datos2$Desempleo[24],2),
      serit == 25 ~ round(Datos2$Desempleo[25],2),
      serit == 26 ~ round(Datos2$Desempleo[26],2)))

Datos<-Datos[ ,c(20,1,2,3,4,6,7,8,9,10,11,23,12,13,14,15,16,17,18,19,21,22)]
buenos <- Datos %>% dplyr::filter(Datos$y==1)
malos <- Datos %>% dplyr::filter(Datos$y==0)

Datos_entrenamiento<-rbind(buenos[createDataPartition(buenos$age,p=0.8,list = F),],
                           malos[createDataPartition(malos$age,p=0.8,list = F),])
Datos_prueba<-rbind(buenos[-createDataPartition(buenos$age,p=0.8,list = F),],
                    malos[-createDataPartition(malos$age,p=0.8,list = F),])


############################# ANALISIS DESCRIPTIVO #############################
########################## VARIABLES NUMERICAS: ################################

##################### variable Y
ggplot(Datos_entrenamiento) +
  aes(x = y, fill = y) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "Riesgo") +
  theme_minimal()

# Aquí se puede observar la cantidad de personas a las que no se les ha aprobado un prestamos (0)
# vs las personas a quienes se les ha aprobado un credito (1). Y no necesita cambios.


##################### variable AGE
hist(Datos_entrenamiento$age, freq = F, xlim = c(17,98), col= "lightskyblue1")
lines(density(Datos_entrenamiento$age))
curve(dnorm(x, mean(Datos_entrenamiento$age), sd(Datos_entrenamiento$age)), lwd = 2, col = "lightseagreen", add = T)
legend("topright", c("curva observada", "curva (normal) teórica"),
       lty = 1, lwd = 2, col = c("black", "lightseagreen"), bty = "n",
       cex = 0.8)

# Verificamos en primer lugar si se trata de una distribución normal con el rest de lilifor
lillie.test(Datos_entrenamiento$age)
# De donde concluimos que la variable edad sigue una distribución normal.

# Ahora aplicaremos un grubbs test para comprobar la existencia de atipicos:
grubbs.test(Datos_entrenamiento$age)
# De donde concluimos que hay fuerte evidencia de la existencia de un valor atípico.

# Ahora bien, notemos que hay sesgo a partir de cuando la edad es 60 por lo cual se hace; 
# en concordancia con lo visto en clase el reemplazo de toos los valores a la derecha
# de 60 por el 60, obteniendo los siguientes resultados:

Datos_entrenamiento<-Datos_entrenamiento %>% mutate(age = replace(age, age > 60, 62))

# Con esto el nuevo histograma sería:
hist(Datos_entrenamiento$age, freq = F, xlim = c(17,70), col= "lightskyblue1")
lines(density(Datos_entrenamiento$age))
curve(dnorm(x, mean(Datos_entrenamiento$age), sd(Datos_entrenamiento$age)), lwd = 2, col = "lightseagreen", add = T)
legend("topright", c("curva observada", "curva (normal) teórica"),
       lty = 1, lwd = 2, col = c("black", "lightseagreen"), bty = "n",
       cex = 0.8)

grubbs.test(Datos_entrenamiento$age)
# Y aplicando grubbs test podemos ver que se ha corregido la existencia de atípicos.


##################### variable MONTHD
hist(Datos_entrenamiento$monthd, freq = F, xlim = c(0,30), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$monthd, main = "DC para varible monthd", xlab="Mes", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$monthd)

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable monthd y 
# el numero de malos sobre el total:

CM <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CM <- CM %>% mutate(y = replace(y,y==0,1))

pruebaM<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CM$y),2) ),data = CM)
aM<-pruebaM$y
aM<-as.data.frame(aM)
bM<-pruebaM$monthd
bM<-as.data.frame(bM)
dibM<-cbind(aM,bM)
# Para eliminar los atipicos de la variable monthd procedemos a realizar 3 clustes mediante el siguiente criterio:

#- gr1 -> [0.20-0.14]  
#- gr2 -> [0.13-0.07]
#- gr3 -> [0.06-0.00]

dibM$gr<-c("gr1","gr2","gr1","gr2","gr3","gr2","gr3","gr3","gr2","gr1","gr3","gr3",
           "gr3","gr3","gr3","gr3","gr3","gr3","gr3","gr3","gr3","gr3","gr3","gr3",
           "gr3","gr3")
dibM
# Antes cluster
ggplot(dibM) +
  geom_point(mapping = aes(x = bM, y = Porcentaje, color=bM, size=2))

# Despues cluster
ggplot(dibM) +
  geom_point(mapping = aes(x = bM, y = Porcentaje, color = gr, size=2))

# Con lo cual se han eliminado los atípicos de la vaiable monthd
Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    monthd = case_when(
      monthd == "1" ~ "gr1",
      monthd == "2" ~ "gr2",
      monthd == "3" ~ "gr1",
      monthd == "4" ~ "gr2",
      monthd == "5" ~ "gr3",
      monthd == "6" ~ "gr2",
      monthd == "7" ~ "gr3",
      monthd == "8" ~ "gr3",
      monthd == "9" ~ "gr2",
      monthd == "10" ~ "gr1",
      monthd == "11" ~ "gr3",
      monthd == "12" ~ "gr3",
      monthd == "13" ~ "gr3",
      monthd == "14" ~ "gr3",
      monthd == "15" ~ "gr3",
      monthd == "16" ~ "gr3",
      monthd == "17" ~ "gr3",
      monthd == "18" ~ "gr3",
      monthd == "19" ~ "gr3",
      monthd == "20" ~ "gr3",
      monthd == "21" ~ "gr3",
      monthd == "22" ~ "gr3",
      monthd == "23" ~ "gr3",
      monthd == "24" ~ "gr3",
      monthd == "25" ~ "gr3",
      monthd == "26" ~ "gr3"))

# Con lo cual la variable monthd se veria de la siguiente manera:

ggplot(Datos_entrenamiento) +
  aes(x = monthd, fill = monthd, colour = monthd) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Monthd") +
  theme_minimal() +
  facet_wrap(vars(y))

##################### variable SERIT
hist(Datos_entrenamiento$serit, freq = F, xlim = c(0,1), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$serit, main = "DC para varible serit", xlab="Serit", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$serit)

# Gracias al  diagrama de caja y la funcion "boxplot.stats" podemos ver que no existen datos atipicos
# por lo cual no es necesario manipular esta variable.


##################### variable DURATION
hist(Datos_entrenamiento$duration, freq = F, xlim = c(0,1000), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$duration, main = "DC para varible monthd", xlab="Mes", col= c("orange3"))

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable duration y 
# el numero de malos sobre el total:

intervald = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,4918)
durcat = c('0-8min','8-16min', '16-25min', '25-33min', '33-41min', '41-50min', '50-58min', '58-66min', '66-75min', '75-82min')
Datos_entrenamiento[,"dur_cat"] = cut(Datos_entrenamiento$duration, breaks = intervald, labels=durcat)

CDur <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CDur <- CDur %>% mutate(y = replace(y,y==0,1))
pDur<-aggregate(y ~ dur_cat, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CDur$y),5) ),data = CDur)

aD<-pDur$y
aD<-as.data.frame(aD)
bD<-pDur$dur_cat
bD<-as.data.frame(bD)
dibD<-cbind(aD,bD)
## Es decir las llamadas menores a 8 min representan un 92% del numero de malos sobre el total, por lo cual
## para corregir los atipicos de esta variable se procedio a realizar 2 cluster, uno para las llamadas menores 
## a 8 min y otro para las mayores a 8 min

# d1 [0-8min]
# d2 [8-82min]
dibD$gr<-c("d1","d2","d2","d2","d2","d2","d2","d2","d2")
dibD
# Antes del cluster
ggplot(dibD) +
  geom_point(mapping = aes(x = bD, y = Porcentaje, color=bD, size=2))

# Despues del cluster
ggplot(dibD) +
  geom_point(mapping = aes(x = bD, y = Porcentaje, color = gr, size=2))

Datos_entrenamiento <- mutate(Datos_entrenamiento, duration = ifelse(duration >= 500,"d2","d1"))
Datos_entrenamiento <- select(Datos_entrenamiento, -dur_cat)

# Con lo cual la variable duration se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = duration, fill = duration, colour = duration) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Duration") +
  theme_minimal() +
  facet_wrap(vars(y))


##################### variable CAMPAIGN
hist(Datos_entrenamiento$campaign, freq = F, xlim = c(0,57), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$campaign, main = "DC para varible campaign", xlab="Campaign", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$campaign)

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable monthd y 
# el numero de malos sobre el total:

intervalc = c(0,6,11,17,22,28,34,39,45,50,56)
camcat = c('0-6','6-11', '11-17', '17-22', '22-28', '28-34', '34-39', '39-45', '45-50', '50-56')
Datos_entrenamiento[,"cam_cat"] = cut(Datos_entrenamiento$campaign, breaks = intervalc, labels=camcat)

CCam <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CCam <- CCam %>% mutate(y = replace(y,y==0,1))

pCam<-aggregate(y ~ cam_cat, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CCam$y),5) ),data = CCam)

aCam<-pCam$y
aCam<-as.data.frame(aCam)
bCam<-pCam$cam_cat
bCam<-as.data.frame(bCam)
dibCam<-cbind(aCam,bCam)
# Es decir el numero de contactos menores a 6 representan un 93% del numero de malos sobre el total,
# por lo cual para corregir los atipicos de esta variable se procedio a realizar 2 cluster, uno para 
# El numero de contactos menores a 6 y otro para los mayores a 6

# d1 [0-6]
# d2 [6-56]
dibCam$gr<-c("d1","d2","d2","d2","d2","d2","d2","d2","d2")
dibCam
# Antes del cluster
ggplot(dibCam) +
  geom_point(mapping = aes(x = bCam, y = Porcentaje, color=bCam, size=2))

# Despues del cluster
ggplot(dibCam) +
  geom_point(mapping = aes(x = bCam, y = Porcentaje, color = gr, size=2))


Datos_entrenamiento <- mutate(Datos_entrenamiento, campaign = ifelse(campaign >= 6,"d2","d1"))
Datos_entrenamiento <- select(Datos_entrenamiento, -cam_cat)

# Con lo cual la variable duration se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = campaign, fill = campaign, colour = campaign) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Campaign") +
  theme_minimal() +
  facet_wrap(vars(y))


##################### variable PDAYS
# El análisis de esta variable será diferente, en primer lugar quitaremos los valores 999 que representa que un 
# cliente no ha sido contactado, posterior a eso con los datos restantes realizaremos el analisis de atipicos
# y de ser el caso la realizacion de clusters como en las variables anteriores.

cosi<-filter(Datos_entrenamiento,Datos_entrenamiento$pdays!=999)
# Una vez retirados los valores 999 tenemos
hist(cosi$pdays, freq = F, xlim = c(0,27), col= "lightskyblue1")
boxplot(x = cosi$pdays, main = "DC para varible pdays", xlab="Pdays", col= c("orange3"))

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable campaign y 
# el numero de malos sobre el total:

intervalp = c(0,7,14,20,27)
pdaycat = c('0-7','7-14', '14-20', '20-27')
cosi[,"pday_cat"] = cut(cosi$pdays, breaks = intervalp, labels=pdaycat)

CP <- cosi %>% dplyr::filter(cosi$y==0)
CP <- CP %>% mutate(y = replace(y,y==0,1))

pP<-aggregate(y ~ pday_cat, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CP$y),5) ),data = CP)

aP<-pP$y
aP<-as.data.frame(aP)
bP<-pP$pday_cat
bP<-as.data.frame(bP)
dibP<-cbind(aP,bP)

# Es decir el numero de dias desde el ultimo contacto menor a 14 dias representan un 94% del numero de malos sobre el total,
# por lo cual para corregir los atipicos de esta variable se procedio a realizar 3 cluster, uno para 
# El numero de dias menores a 14, otro para los mayores a 14 y otros para quienes no han sido contactados

# d1 [0-14]
# d2 [14-27]
# d3 - no contactado

intervalcosi = c(0,14,27,990,999)
cosicat = c('0-14','14-27', '27-990', '990-999')
Datos_entrenamiento[,"cam_cat"] = cut(Datos_entrenamiento$pdays, breaks = intervalcosi, labels=cosicat)

Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    cam_cat = case_when(
      cam_cat == "0-14" ~ "d1",
      cam_cat == "14-27" ~ "d2",
      cam_cat == "27-990" ~ "d2",
      cam_cat == "990-999" ~ "no_contactado"))

Datos_entrenamiento <- select(Datos_entrenamiento, -pdays)
names(Datos_entrenamiento)[22]="pdays"

# Con lo cual la variable pdays se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = pdays, fill = pdays, colour = pdays) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Pdays") +
  theme_minimal() +
  facet_wrap(vars(y))


##################### variable PREVIOUS
hist(Datos_entrenamiento$previous, freq = F, xlim = c(0,7), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$previous, main = "DC para varible Previous", xlab="Previous", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$previous)

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable previous y 
# el numero de malos sobre el total:

CPrev <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CPrev <- CPrev %>% mutate(y = replace(y,y==0,1))

pruebaPrev<-aggregate(y ~ previous, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CPrev$y),2) ),data = CPrev)
aPrev<-pruebaPrev$y
aPrev<-as.data.frame(aPrev)
bPrev<-pruebaPrev$previous
bPrev<-as.data.frame(bPrev)
dibPrev<-cbind(aPrev,bPrev)
# Para eliminar los atipicos de la variable monthd procedemos a realizar 2 clustes mediante el siguiente criterio:

#- gr1 -> [0.89-0.45]  
#- gr2 -> [0.45-0.00]

dibPrev$gr<-c("gr1","gr2","gr2","gr2","gr2","gr2","gr2","gr2")
dibPrev
# Antes cluster
ggplot(dibPrev) +
  geom_point(mapping = aes(x = bPrev, y = Porcentaje, color=bPrev, size=2))

# Despues cluster
ggplot(dibPrev) +
  geom_point(mapping = aes(x = bPrev, y = Porcentaje, color = gr, size=2))

# Con lo cual se han eliminado los atípicos de la vaiable previous
Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    previous = case_when(
      previous == "0" ~ "gr1",
      previous == "1" ~ "gr2",
      previous == "2" ~ "gr2",
      previous == "3" ~ "gr2",
      previous == "4" ~ "gr2",
      previous == "5" ~ "gr2",
      previous == "6" ~ "gr2",
      previous == "7" ~ "gr2"))

# Con lo cual la variable previous se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = previous, fill = previous, colour = previous) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Previous") +
  theme_minimal() +
  facet_wrap(vars(y))


##################### variable CONST.PRICE.IDX
hist(Datos_entrenamiento$cons.price.idx, freq = F, xlim = c(93,94767), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$cons.price.idx, main = "DC para varible Cons.price.idx", xlab="Cons.ptice.idx", col= c("orange3"))

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable cons.price.idx y 
# el numero de malos sobre el total:


intervalcpi = c(93,9561,19028,28495,37963,47430,56897,66365,75832,85300,94767)
cpicat = c('93-9561', '9561-19028', '19028-28495', '28495-37963', '37963-47430', '47430-56897',
           '56897-66365', '66365-75832', '75832-85300','85300-94767')
Datos_entrenamiento[,"cpi_cat"] = cut(Datos_entrenamiento$cons.price.idx, breaks = intervalcpi, labels=cpicat)

Ccpi <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
Ccpi <- Ccpi %>% mutate(y = replace(y,y==0,1))
pcpi<-aggregate(y ~ cpi_cat, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(Ccpi$y),5) ),data = Ccpi)

acpi<-pcpi$y
acpi<-as.data.frame(acpi)
bcpi<-pcpi$cpi_cat
bcpi<-as.data.frame(bcpi)
dibcpi<-cbind(acpi,bcpi)
## Es decir si el indice de precio al consumidor esta entre 93 y 9561 representa un 0.1 de malos sobre el total,
## y si el indice de precio al consumidor esta entre 85300 y 94767 representa un 0.9 de malos sobre el total, por
# tanto se crearan 2 clusters para cada uno de estos grupos

# d1 [93-9561]
# d2 [85300-94767]
dibcpi$gr<-c("d1","d2")
dibcpi

# Despues del cluster
ggplot(dibcpi) +
  geom_point(mapping = aes(x = bcpi, y = Porcentaje, color = gr, size=2))

# Con lo cual se han eliminado los atipicos
Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    cpi_cat = case_when(
      cpi_cat == "93-9561" ~ "d1",
      cpi_cat == "85300-94767" ~ "d2"))

Datos_entrenamiento <- select(Datos_entrenamiento, -cons.price.idx)
names(Datos_entrenamiento)[22]="cons.price.idx"

# Con lo cual la variable pdays se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = cons.price.idx, fill = cons.price.idx, colour = cons.price.idx) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Pdays") +
  theme_minimal() +
  facet_wrap(vars(y))


##################### variable EURIBORN3M
hist(Datos_entrenamiento$euribor3m, freq = F, xlim = c(0,5045), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$euribor3m, main = "DC para varible euriborn3m", xlab="Euriborn3m", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$euribor3m)

# Gracias al  diagrama de caja y la funcion "boxplot.stats" podemos ver que no existen datos atipicos
# por lo cual no es necesario manipular esta variable.



##################### variable NREMPLOYED
hist(Datos_entrenamiento$nr.employed, freq = F, xlim = c(4960,5228), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$nr.employed, main = "DC para varible nremployed", xlab="Neemloyed", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$nr.employed)

# Gracias al  diagrama de caja y la funcion "boxplot.stats" podemos ver que no existen datos atipicos
# por lo cual no es necesario manipular esta variable.



##################### variable EMP.VAR.RATE
hist(Datos_entrenamiento$emp.var.rate, freq = F, xlim = c(-4,2), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$emp.var.rate, main = "DC para varible emp.var.rate", xlab="Emp.var.rate", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$emp.var.rate)

# Gracias al  diagrama de caja y la funcion "boxplot.stats" podemos ver que no existen datos atipicos
# por lo cual no es necesario manipular esta variable.



##################### variable CONS.CONF.IDX
hist(Datos_entrenamiento$cons.conf.idx, freq = F, xlim = c(-50,-26), col= "lightskyblue1")
boxplot(x = Datos_entrenamiento$cons.conf.idx, main = "DC para varible cons.conf.idx", xlab="Cons.conf.idx", col= c("orange3"))
boxplot.stats(x = Datos_entrenamiento$cons.conf.idx)

# Gracias al histograma y el diagrama de caja dado que no se tiene una distribuión conocida y se tiene datos 
# atípicos; acorde a los visto en clase podemos verficar la relación existente entre la variable cons.conf.idx y 
# el numero de malos sobre el total:

intervalConf = c(-51, -45, -40, -35, -30, -25)
catConf = c('-50-45', '-45-40', '-40-35', '-35-30', '-30-25')
Datos_entrenamiento[ ,"conf_cat"] = cut(Datos_entrenamiento$cons.conf.idx, breaks = intervalConf, labels=catConf)
CC <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CC <- CC %>% mutate(y = replace(y,y==0,1))
pC <- aggregate(y ~ conf_cat , FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CC$y),2) ),data = CC)

aC<-pC$y
aC<-as.data.frame(aC)
bC<-pC$conf_cat
bC<-as.data.frame(bC)
dibC<-cbind(aC,bC)
dibC$gr<-c("media","alto","alto","bajo","bajo")
dibC

ggplot(dibC) +
  geom_point(mapping = aes(x = bC, y = Porcentaje, color=bC, size=2)) 

ggplot(dibC) +
  geom_point(mapping = aes(x = bC, y = Porcentaje, color = gr, size=2))

# alto [40-27]
# media [26-14]
# bajo [13-0]

Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    conf_cat = case_when(
      conf_cat == "-50-45" ~ "media",
      conf_cat == "-45-40" ~ "alto",
      conf_cat == "-40-35" ~ "alto",
      conf_cat == "-35-30" ~ "bajo",
      conf_cat == "-30-25" ~ "bajo"))

Datos_entrenamiento<-select(Datos_entrenamiento, -cons.conf.idx)
names (Datos_entrenamiento)[22] = "cons.conf.idx"

# Con lo cual la variable cons.conf,idx se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = cons.conf.idx, fill = cons.conf.idx, colour = cons.conf.idx) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Cons.conf.idx") +
  theme_minimal() +
  facet_wrap(vars(y))


###################### VARIABLES CATEGORICAS: ##################################
# En esta seccion se busca reducir la cardinalidad de variables 

##################### variable JOB

# La variable job se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = job, fill = job, colour = job) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "job") +
  theme_minimal() +
  facet_wrap(vars(y))

# Para reducir la cardinalidad se crearan clusters verificando la cantidad de malos sobre el total como en la 
# parte anterior
CJ <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CJ <- CJ %>% mutate(y = replace(y,y==0,1))
prueba<-aggregate(y ~ job, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CJ$y),2) ),data = CJ)

a<-prueba$y
a<-as.data.frame(a)
b<-prueba$job
b<-as.data.frame(b)
dib<-cbind(a,b)

# Crearemos clusters segun la siguiente estructura:
#- status alto -> [0.25-0.17]  
#- status medio -> [0.16-0.08]
#- status bajo -> [0.07-0.00]


dib$gr<-c("st.alto","st.alto","st.bajo","st.bajo","st.medio","st.bajo","st.bajo","st.medio","st.bajo","st.medio","st.bajo","st.bajo")
dib

ggplot(dib) +
  geom_point(mapping = aes(x = b, y = Porcentaje, color=b, size=2))

ggplot(dib) +
  geom_point(mapping = aes(x = b, y = Porcentaje, color = gr, size=2))

# Con lo cual se ha reducido la cardinalidad a 3
Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    job = case_when(
      job == "admin." ~ "st.alto",
      job == "blue-collar" ~ "st.alto",
      job == "entrepreneur" ~ "st.bajo",
      job == "housemaid" ~ "st.bajo",
      job == "management" ~ "st.medio",
      job == "retired" ~ "st.bajo",
      job == "self-employed" ~ "st.bajo",
      job == "services" ~ "st.medio",
      job == "student" ~ "st.bajo",
      job == "technician" ~ "st.medio",
      job == "unemployed" ~ "st.bajo",
      job == "unknown" ~ "st.bajo"))


# La nueva variable job se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = job, fill = job, colour = job) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "job") +
  theme_minimal() +
  facet_wrap(vars(y))



##################### variable EDUCATION

# La variable education se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = education, fill = education, colour = education) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Education") +
  theme_minimal() +
  facet_wrap(vars(y))

# Para reducir la cardinalidad se crearan clusters verificando la cantidad de malos sobre el total como en la 
# parte anterior
prueba1<-aggregate(y ~ education, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CJ$y),2) ),data = CJ)

####
a1<-prueba1$y
a1<-as.data.frame(a1)
b1<-prueba1$education
b1<-as.data.frame(b1)
dib1<-cbind(a1,b1)

# Crearemos clusters segun la siguiente estructura
#- superior   -> [0.29-0.19]  
#- media    -> [0.18-0.10]
#- basica -> [0.09-0.00]


dib1$gr<-c("media","basica","media","superior","basica","media","superior","basica")
dib1

ggplot(dib1) +
  geom_point(mapping = aes(x = b1, y = Porcentaje, color=b1, size=2))

ggplot(dib1) +
  geom_point(mapping = aes(x = b1, y = Porcentaje, color = gr,  size=2))


# Con lo cual se ha reducido la cardinalidad a 3
Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    education = case_when(
      education == "basic.4y" ~ "media",
      education == "basic.6y" ~ "basica",
      education == "basic.9y" ~ "basica",
      education == "high.school" ~ "superior",
      education == "illiterate" ~ "basica",
      education == "professional.course" ~ "media",
      education == "university.degree" ~ "superior",
      education == "unknown" ~ "basica"))


# La nueva variable education se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = education, fill = education, colour = education) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "education") +
  theme_minimal() +
  facet_wrap(vars(y))


##################### variable MARITAL

# La variable marital se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = marital, fill = marital, colour = marital) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Marital") +
  theme_minimal() +
  facet_wrap(vars(y))

# Dado que se tiene una cardinalidad igual a 4 no se manipula esta variable.


##################### variable HOUSING

# La variable housing se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = housing, fill = housing, colour = housing) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Housing") +
  theme_minimal() +
  facet_wrap(vars(y))

# Dado que se tiene una cardinalidad igual a 3 no se manipula esta variable.


##################### variable LOAN

# La variable loan se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = loan, fill = loan, colour = loan) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Loan") +
  theme_minimal() +
  facet_wrap(vars(y))

# Dado que se tiene una cardinalidad igual a 3 no se manipula esta variable.


##################### variable CONTACT

# La variable contact se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = contact, fill = contact, colour = contact) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Contact") +
  theme_minimal() +
  facet_wrap(vars(y))

# Dado que se tiene una cardinalidad igual a 2 no se manipula esta variable.


##################### variable MONT

# La variable contact se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = month, fill = month, colour = month) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Month") +
  theme_minimal() +
  facet_wrap(vars(y))

# Para reducir la cardinalidad se crearan clusters verificando la cantidad de malos sobre el total como en la 
# parte anterior
CMonth <- Datos_entrenamiento %>% dplyr::filter(Datos_entrenamiento$y==0)
CMonth <- CMonth %>% mutate(y = replace(y,y==0,1))
pruebaMonth<-aggregate(y ~ month, FUN = function(x) c(Suma = sum(x), Porcentaje = round(sum(x)/length(CMonth$y),2) ),data = CMonth)

aMonth<-pruebaMonth$y
aMonth<-as.data.frame(aMonth)
bMonth<-pruebaMonth$month
bMonth<-as.data.frame(bMonth)
dibMonth<-cbind(aMonth,bMonth)

# Crearemos clusters segun la siguiente estructura:
#- mesesg1 -> [0.35-0.24]  
#- mesesg2 -> [0.23-0.12]
#- mesesg3 -> [0.11-0.00]


dibMonth$gr<-c("mesesg3","mesesg2","mesesg3","mesesg2","mesesg2","mesesg3","mesesg1","mesesg2","mesesg3","mesesg3")
dibMonth

ggplot(dibMonth) +
  geom_point(mapping = aes(x = bMonth, y = Porcentaje, color=bMonth, size=2))

ggplot(dibMonth) +
  geom_point(mapping = aes(x = bMonth, y = Porcentaje, color = gr, size=2))

# Con lo cual se ha reducido la cardinalidad a 3
Datos_entrenamiento <- Datos_entrenamiento %>% 
  mutate(
    month = case_when(
      month == "apr" ~ "mesg3",
      month == "aug" ~ "mesg2",
      month == "dec" ~ "mesg3",
      month == "jul" ~ "mesg2",
      month == "jun" ~ "mesg2",
      month == "mar" ~ "mesg3",
      month == "may" ~ "mesg1",
      month == "nov" ~ "mesg2",
      month == "oct" ~ "mesg3",
      month == "sep" ~ "mesg3"))


# La nueva variable job se veria de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = month, fill = month, colour = month) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Month") +
  theme_minimal() +
  facet_wrap(vars(y))



##################### variable DAY OF WEEK

# La variable contact se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = day_of_week, fill = day_of_week, colour = day_of_week) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Day of Week") +
  theme_minimal() +
  facet_wrap(vars(y))

# Dado que se tiene una cardinalidad igual a 5 no se manipula esta variable.


##################### variable CONTACT

# La variable contact se ve de la siguiente manera:
ggplot(Datos_entrenamiento) +
  aes(x = contact, fill = contact, colour = contact) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Contact") +
  theme_minimal() +
  facet_wrap(vars(y))

# Dado que se tiene una cardinalidad igual a 2 no se manipula esta variable.




################################### Data PIT ###################################
DatosEntrPIT<- Datos_entrenamiento

DatosEntrPITIVDM<-dummy_cols(DatosEntrPIT,
    select_columns = c("job", "education","marital","housing","loan","contact","monthd","month",
    "day_of_week","duration","campaign","previous","poutcome","pdays","cons.price.idx","cons.conf.idx"))

DatosEntrPITIVDM<-select(DatosEntrPITIVDM,-job,-education,-marital,-housing,-loan,-contact,-monthd,-month,
    -day_of_week,-duration,-campaign,-previous,-poutcome,-pdays,-cons.price.idx,-cons.conf.idx)


######################## KS TEST E INFO VALUE ##################################

# aplicando ks test para numericas E info value para categoricas obtuvimos la siguiente
# tabla, guiandonos en el criterio:
# <0.02 predictor inutil
# 0.02-0.1predictor debil
# 0.1-0.3 predictor medio
# >0.3 fuerte

info_valuet = iv(DatosEntrPITIVDM, y = "y")
at<-info_valuet$variable
bt<-info_valuet$info_value
ct<-data.frame(at,round(bt,3))
ct


### ELIEGIMOS LOS PREDICTORES MEDIOS Y ALTOS
DatosEntrPITIVDM<-select(DatosEntrPITIVDM, y, euribor3m, serit, nr.employed,  monthd_gr3, emp.var.rate,
  duration_d1, duration_d2, pdays_no_contactado, pdays_d1, poutcome_success, cons.conf.idx_bajo,
  month_mesg3, monthd_gr1, cons.conf.idx_alto, previous_gr1, previous_gr2, poutcome_nonexistent,
  contact_cellular,contact_telephone,  age, month_mesg1, monthd_gr2 )

############################  MODELO LOGIT #####################################

##################### MODELO INICIAL ###########################################
 
RLOG<-glm(formula = y~euribor3m+serit+nr.employed+monthd_gr3+emp.var.rate+duration_d1+duration_d2+pdays_no_contactado
  +pdays_d1+poutcome_success+cons.conf.idx_bajo+month_mesg3+monthd_gr1+cons.conf.idx_alto
  +previous_gr1+previous_gr2+poutcome_nonexistent+contact_cellular+contact_telephone+age+month_mesg1
  +monthd_gr2, DatosEntrPITIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOG)


### MODELO SIN:
# Primero quitamos los NA
# duration_d2, previous_gr2, poutcome_nonexistent, contact_telephone,monthd_gr2 


# Luego el orden en el que salen es:
# month_mesg1
# euribor3m
# pdays_d1
# age

RLOG1<-glm(formula = y~serit+nr.employed+monthd_gr3+emp.var.rate+duration_d1+pdays_no_contactado
          +poutcome_success+cons.conf.idx_bajo+month_mesg3+monthd_gr1+cons.conf.idx_alto
          +previous_gr1+contact_cellular, DatosEntrPITIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOG1)

# Estadistico de prueba
with(RLOG1,null.deviance-deviance)
# Pvalor
with(RLOG1,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))

################################ MULTICOLINEALIDAD  ############################
## 1. Correlacion

VC<-select(DatosEntrPITIVDM, serit,nr.employed,monthd_gr3,emp.var.rate,duration_d1,pdays_no_contactado,
           poutcome_success,cons.conf.idx_bajo,month_mesg3,monthd_gr1,cons.conf.idx_alto,
           previous_gr1,contact_cellular)

M<-cor(VC)
# Dado que eXISTE UNA alta correlacion entre algunas variables nos puede dar un undicio de multicolinealidad
corrplot(M)

## 2. factor influencias de varianza
# Revisemos entonces el factor de influencia de varianza

vif(RLOG1)
# Segun los valores obtenidos estamos frente a la existencia de multicolinealidad
# pues hay 7 variables que tienen un valor superior a 5 entonces analizando vemos que monthd_gr3
# tiene un alta correlacion con todas las variables por la cual la retiramos, asi:

RLOG2<-glm(formula = y~serit+nr.employed+emp.var.rate+duration_d1+pdays_no_contactado+cons.conf.idx_alto
           +poutcome_success+cons.conf.idx_bajo+month_mesg3+monthd_gr1
           +previous_gr1+contact_cellular, DatosEntrPITIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOG2)

##### lo que hace que +cons.conf.idx_alto deje de ser significativo
RLOG2<-glm(formula = y~serit+nr.employed+emp.var.rate+duration_d1+pdays_no_contactado
           +poutcome_success+cons.conf.idx_bajo+month_mesg3+monthd_gr1
           +previous_gr1+contact_cellular, DatosEntrPITIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOG2)

# Revisamos nuevamente la correlacion
VC<-select(DatosEntrPITIVDM, serit,nr.employed,emp.var.rate,duration_d1,pdays_no_contactado,
           poutcome_success,cons.conf.idx_bajo,month_mesg3,monthd_gr1,
           previous_gr1,contact_cellular)

M1<-cor(VC)
# Dado que eXISTE UNA alta correlacion entre algunas variables nos puede dar un undicio de multicolinealidad
corrplot(M1)

# Revisemos entonces el factor de influencia de varianza
vif(RLOG2)
# Segun los valores obtenidos estamos frente a la existencia de multicolinealidad
# pues hay 2 variables que tienen un valor superior a 10 entonces procedamos eliminando la
# que tiene e mayor valor pdays_no_contactado 

RLOG3<-glm(formula = y~serit+nr.employed+emp.var.rate+duration_d1
           +poutcome_success+cons.conf.idx_bajo+month_mesg3+monthd_gr1
           +previous_gr1+contact_cellular, DatosEntrPITIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOG3)


# Revisamos nuevamente la correlacion
VC<-select(DatosEntrPITIVDM, serit,nr.employed,emp.var.rate,duration_d1,
           poutcome_success,cons.conf.idx_bajo,month_mesg3,monthd_gr1,
           previous_gr1,contact_cellular)

M2<-cor(VC)
corrplot(M2)

vif(RLOG3)
# Con esto se ha corregido la multicolinealidad
# Este es un modelo bueno corregido sin multicolinealidad
with(RLOG3,null.deviance-deviance)

# Pvalor
with(RLOG3,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))
# Y tambien es significativo


############################ arreglo data prueba #############################
# Antes de empezar con la matriz de confusion, debemos trabajar a la data de 
# prueba en el sentido de que debe tener los mismo nombres y variables que las
# que aparecen en el modelo, asi:

Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("marital","contact","poutcome"))
Datos_prueba<-select(Datos_prueba,-marital,-contact,-poutcome)
Datos_prueba <- Datos_prueba %>% 
  mutate(
    monthd = case_when(
      monthd == "1" ~ "gr1",
      monthd == "2" ~ "gr2",
      monthd == "3" ~ "gr1",
      monthd == "4" ~ "gr2",
      monthd == "5" ~ "gr3",
      monthd == "6" ~ "gr2",
      monthd == "7" ~ "gr3",
      monthd == "8" ~ "gr3",
      monthd == "9" ~ "gr2",
      monthd == "10" ~ "gr1",
      monthd == "11" ~ "gr3",
      monthd == "12" ~ "gr3",
      monthd == "13" ~ "gr3",
      monthd == "14" ~ "gr3",
      monthd == "15" ~ "gr3",
      monthd == "16" ~ "gr3",
      monthd == "17" ~ "gr3",
      monthd == "18" ~ "gr3",
      monthd == "19" ~ "gr3",
      monthd == "20" ~ "gr3",
      monthd == "21" ~ "gr3",
      monthd == "22" ~ "gr3",
      monthd == "23" ~ "gr3",
      monthd == "24" ~ "gr3",
      monthd == "25" ~ "gr3",
      monthd == "26" ~ "gr3"))
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("monthd"))
Datos_prueba<-select(Datos_prueba,-monthd)
Datos_prueba <- Datos_prueba %>% 
  mutate(
    month = case_when(
      month == "apr" ~ "mesg3",
      month == "aug" ~ "mesg2",
      month == "dec" ~ "mesg3",
      month == "jul" ~ "mesg2",
      month == "jun" ~ "mesg2",
      month == "mar" ~ "mesg3",
      month == "may" ~ "mesg1",
      month == "nov" ~ "mesg2",
      month == "oct" ~ "mesg3",
      month == "sep" ~ "mesg3"))
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("month"))
Datos_prueba<-select(Datos_prueba,-month)
Datos_prueba <- mutate(Datos_prueba, duration = ifelse(duration >= 500,"d2","d1"))
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("duration"))
Datos_prueba<-select(Datos_prueba,-duration)

Datos_prueba[,"cam_cat"] = cut(Datos_prueba$pdays, breaks = intervalcosi, labels=cosicat)
Datos_prueba <- Datos_prueba %>% 
  mutate(
    cam_cat = case_when(
      cam_cat == "0-14" ~ "d1",
      cam_cat == "14-27" ~ "d2",
      cam_cat == "27-990" ~ "d2",
      cam_cat == "990-999" ~ "no_contactado"))
Datos_prueba<-select(Datos_prueba,-pdays)
names(Datos_prueba)[33]="pdays"
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("pdays"))
Datos_prueba<-select(Datos_prueba,-pdays)
Datos_prueba[,"cpi_cat"] = cut(Datos_prueba$cons.price.idx, breaks = intervalcpi, labels=cpicat)
Datos_prueba <- Datos_prueba %>% 
  mutate(
    cpi_cat = case_when(
      cpi_cat == "93-9561" ~ "d1",
      cpi_cat == "85300-94767" ~ "d2"))
Datos_prueba<-select(Datos_prueba,-cons.price.idx)
names(Datos_prueba)[36]="cons.price.idx"
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("cons.price.idx"))
Datos_prueba<-select(Datos_prueba,-cons.price.idx)

Datos_prueba[ ,"conf_cat"] = cut(Datos_prueba$cons.conf.idx, breaks = intervalConf, labels=catConf)
Datos_prueba <- Datos_prueba %>% 
  mutate(
    conf_cat = case_when(
      conf_cat == "-50-45" ~ "media",
      conf_cat == "-45-40" ~ "alto",
      conf_cat == "-40-35" ~ "alto",
      conf_cat == "-35-30" ~ "bajo",
      conf_cat == "-30-25" ~ "bajo"))

Datos_prueba<-select(Datos_prueba,-cons.conf.idx)
names(Datos_prueba)[37]="cons.conf.idx"
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("cons.conf.idx"))
Datos_prueba<-select(Datos_prueba,-cons.conf.idx)

Datos_prueba[,"cam_cat"] = cut(Datos_prueba$campaign, breaks = intervalc, labels=camcat)
Datos_prueba <- mutate(Datos_prueba, campaign = ifelse(campaign >= 6,"d2","d1"))
Datos_prueba<-select(Datos_prueba,-cam_cat)
Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("campaign"))
Datos_prueba<-select(Datos_prueba,-campaign)

Datos_prueba <- Datos_prueba %>% 
  mutate(
    previous = case_when(
      previous == "0" ~ "gr1",
      previous == "1" ~ "gr2",
      previous == "2" ~ "gr2",
      previous == "3" ~ "gr2",
      previous == "4" ~ "gr2",
      previous == "5" ~ "gr2",
      previous == "6" ~ "gr2",
      previous == "7" ~ "gr2"))

Datos_prueba<-dummy_cols(Datos_prueba, select_columns = c("previous"))
Datos_prueba<-select(Datos_prueba,-previous)


############################ MATRIZ DE CONFUSION ###############################
p<-predict(RLOG3,Datos_prueba, type = "response")
p_class<-ifelse(p>0.5, 1, 0)
MC<-confusionMatrix(p_class, Datos_prueba$y)
MC 
# De nuestr matriz de confusion podemos obtener la siguiente información
Acurracy<-(MC[1,1]+MC[2,2])/(MC[1,1]+MC[1,2]+MC[2,1]+MC[2,2])
Acurracy
### En este punto se harán algunas aclaraciones:

# Peligro: la métrica accuracy (exactitud) no funciona bien cuando las clases están desbalanceadas. 
# Pues, la mayoría de los clientes no será acreedores a crédito, así que es muy fácil acertar
# diciendo que no se les otorgará credito.

# Acorde a lo visto en clase debemos balancer la data si la proporcion de 1 es menor al 5% y dado que para
# nuestro caso 926/(7307+926)= 0,11 es decir la proporcion es del 11% no es necesario balancear la data.
# Por tanto del valor obtenido de Acurracy podemos decir que el porcentaje de la data clasificada 
# correctamente es del 89% por lo cual tenemos un buen modelo.
Tasa_error<-(MC[1,2]+MC[2,1])/(MC[1,1]+MC[1,2]+MC[2,1]+MC[2,2])
Tasa_error
# Es decir que el porcentaje de la data clasificada incorrectamente
# es del 1% por lo cual tenemos un buen modelo.

############################### CURVA ROC ######################################
colAUC(p,Datos_prueba$y,plotROC="TRUE")
# En este punto podemos ver que el area bajo la curva esta entre 1 y 0.9 por lo
# cual obtuvimos un buen modelo de predicción.







######################### GRUPOS HOMOGENEOS DE RIESGO ##########################
pDs<-predict(RLOG3,DatosEntrPITIVDM, type = "response")
GHPIT<-DatosEntrPIT
GHPIT[,"PD"]<-pDs
GHPIT<-select(GHPIT,y,PD)

pDsP<-predict(RLOG3,Datos_prueba, type = "response")
GHPITP<-Datos_prueba
GHPITP[,"PD"]<-pDsP
GHPITP<-select(GHPITP,y,PD)

GHRPIT<-rbind(GHPIT,GHPITP)
GHRPIT[,"monthd"]<-Datos$monthd
GHRPIT <- GHRPIT[!is.na(GHRPIT$PD),]


grupos = c(0, 0.2, 0.4, 0.6, 0.8, 1)
PDcat = c('0-0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1')
GHRPIT[,"PD_cat"] = cut(GHRPIT$PD, breaks = grupos, labels=PDcat)


GHRPIT[,"cambiar"] = GHRPIT$PD_cat
GHRPIT <- GHRPIT %>%
  mutate(
    cambiar = case_when(
      cambiar == "0-0.2" ~ "A",
      cambiar == "0.2-0.4" ~ "B",
      cambiar == "0.4-0.6" ~ "C",
      cambiar == "0.6-0.8" ~ "D",
      cambiar == "0.8-1" ~ "E"))

GHRPITmalos<-filter(GHRPIT, GHRPIT$y==0)
GHRPITmalos <- GHRPITmalos %>% mutate(y = replace(y,y==0,1))
names(GHRPITmalos)[5]<-"Grupo"

## GRA
GHRPITmalosGA<-filter(GHRPITmalos, GHRPITmalos$Grupo=="A")
porGA<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeA = round(sum(x)/length(GHRPITmalosGA$y),3) ),data = GHRPITmalosGA)

aGA<-porGA$y
aGA<-as.data.frame(aGA)
mes<-porGA$monthd
mes<-as.data.frame(mes)
TGA<-cbind(aGA,mes)
TGA<-select(TGA, mes,PorcentajeA)

## GRB
GHRPITmalosGB<-filter(GHRPITmalos, GHRPITmalos$Grupo=="B")
porGB<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeB = round(sum(x)/length(GHRPITmalosGB$y),3) ),data = GHRPITmalosGB)

aGB<-porGB$y
aGB<-as.data.frame(aGB)
mes<-porGB$monthd
mes<-as.data.frame(mes)
TGB<-cbind(aGB,mes)
TGB<-select(TGB, mes,PorcentajeB)

AB<-merge(x = TGA, y = TGB, all = T)

## GRC
GHRPITmalosGC<-filter(GHRPITmalos, GHRPITmalos$Grupo=="C")
porGC<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeC = round(sum(x)/length(GHRPITmalosGC$y),3) ),data = GHRPITmalosGC)

aGC<-porGC$y
aGC<-as.data.frame(aGC)
mes<-porGC$monthd
mes<-as.data.frame(mes)
TGC<-cbind(aGC,mes)
TGC<-select(TGC, mes,PorcentajeC)

BC<-merge(x = AB, y = TGC, all = T)

## GRD
GHRPITmalosGD<-filter(GHRPITmalos, GHRPITmalos$Grupo=="D")
porGD<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeD = round(sum(x)/length(GHRPITmalosGD$y),3) ),data = GHRPITmalosGD)

aGD<-porGD$y
aGD<-as.data.frame(aGD)
mes<-porGD$monthd
mes<-as.data.frame(mes)
TGD<-cbind(aGD,mes)
TGD<-select(TGD, mes,PorcentajeD)

CD<-merge(x = BC, y = TGD, all = T)

## GRE
GHRPITmalosGE<-filter(GHRPITmalos, GHRPITmalos$Grupo=="E")
porGE<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeE = round(sum(x)/length(GHRPITmalosGE$y),3) ),data = GHRPITmalosGE)

aGE<-porGE$y
aGE<-as.data.frame(aGE)
mes<-porGE$monthd
mes<-as.data.frame(mes)
TGE<-cbind(aGE,mes)
TGE<-select(TGE, mes,PorcentajeE)

DE<-merge(x = CD, y = TGE, all = T)
MATRIZGH1<-DE
MATRIZGH1[is.na(MATRIZGH1)]<-0

#write.csv(MATRIZGH1, "GHR.csv")








############################ TTC ###############################################
################################### data TTC ###################################
# Usaremos las variables: y, age, job, marital, education, housing, loan 
# contact"     "monthd"      "month"       "day_of_week" "serit"  
DatosEntrTTC<- Datos_entrenamiento[ ,c(1,2,3,4,5,6,7,8,9,10,11,12)]

######################## KS TEST E INFO VALUE ##################################

# aplicando ks test para numericas E info value para categoricas obtuvimos la siguiente
# tabla, guiandonos en el criterio:
# <0.02 predictor inutil
# 0.02-0.1predictor debil
# 0.1-0.3 predictor medio
# >0.3 fuerte

info_valuettc = iv(DatosEntrTTC, y = "y")
attc<-info_valuettc$variable
bttc<-info_valuettc$info_value
cttc<-data.frame(attc,round(bttc,3))
cttc


### ELIMINAMOS LOS PREDICTORES INUTILES
DatosEntrTTCIV<-select(DatosEntrTTC, -day_of_week,- loan, -housing)


## Hacemos Dummies para que sea mas fácil quitar del modelo las que no sirven:

DatosEntrTTCIVDM<-dummy_cols(DatosEntrTTCIV, select_columns = c("job", "education","marital",
                                                                "contact","monthd","month"))                                                            
DatosEntrTTCIVDM<-select(DatosEntrTTCIVDM,-job, -education,-marital,-contact,-monthd,-month)


############################  MODELO LOGIT #####################################

### MODELO INICIAL
RLOGTTC<-glm(formula = y~age+serit+job_st.alto
             +job_st.bajo+job_st.medio+education_basica+education_media
             +education_superior+marital_divorced+marital_married+marital_single
             +marital_unknown+contact_cellular+contact_telephone+monthd_gr1
             +monthd_gr2+monthd_gr3+month_mesg1+month_mesg2
             +month_mesg3, DatosEntrTTCIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOGTTC)

### MODELO SIN:
# Primero quitamos los NA
# job_st.medio, education_superior, marital_unknown, contact_telephone, monthd_gr3,month_mesg3

# Luego el orden en el que salen es:
# job_st.alto
# month_mesg1
# age
# marital_single
# education_basica
# education_media

RLOGTTC1<-glm(formula = y~serit
              +job_st.bajo
              +marital_divorced+marital_married
              +contact_cellular+monthd_gr1
              +monthd_gr2+month_mesg2, DatosEntrTTCIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOGTTC1)


# Estadistico de prueba
with(RLOGTTC1,null.deviance-deviance)
# Pvalor
with(RLOGTTC1,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))


################################ MULTICOLINEALIDAD  ############################
## 1. Correlacion

VCTTC<-select(DatosEntrTTCIVDM, serit,job_st.bajo,marital_divorced,marital_married,
              contact_cellular,monthd_gr1,monthd_gr2,month_mesg2)

cor(VCTTC)
# Dado que eXISTE UNA alta correlacion entre algunas variables nos puede dar un undicio de
# que existe multicolinealidad

## 2. factor influencias de varianza
# Revisemos entonces el factor de influencia de varianza

vif(RLOGTTC1)
# Segun los valores obtenidos hay 3 variables con valores superiores a 10 por tanto eliminamos la 
# mas alta (monthd_gr1).


RLOGTTC2<-glm(formula = y~serit
              +job_st.bajo
              +marital_divorced+marital_married
              +contact_cellular
              +monthd_gr2+month_mesg2, DatosEntrTTCIVDM ,family = binomial(link = "logit"))
#options(max.print=999999)
summary(RLOGTTC2)

# Revisemos entonces el factor de influencia de varianza

vif(RLOGTTC2)

# Y podemos ver que ya no existe multicolinealidad.

with(RLOGTTC2,null.deviance-deviance)
# Pvalor
with(RLOGTTC2,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = FALSE))
# Ademas el  modelo es significativo

########################### arreglo data prueba ################################
# Antes de empezar con la matriz de confusion, debemos trabajar a la data de 
# prueba en el sentido de que debe tener los mismo nombres y variables que las
# que aparecen en el modelo, asi:

Datos_pruebaTTC<-Datos_prueba
Datos_pruebaTTC <- Datos_pruebaTTC %>% 
  mutate(
    job = case_when(
      job == "admin." ~ "st.alto",
      job == "blue-collar" ~ "st.alto",
      job == "entrepreneur" ~ "st.bajo",
      job == "housemaid" ~ "st.bajo",
      job == "management" ~ "st.medio",
      job == "retired" ~ "st.bajo",
      job == "self-employed" ~ "st.bajo",
      job == "services" ~ "st.medio",
      job == "student" ~ "st.bajo",
      job == "technician" ~ "st.medio",
      job == "unemployed" ~ "st.bajo",
      job == "unknown" ~ "st.bajo"))
Datos_pruebaTTC<-dummy_cols(Datos_pruebaTTC, select_columns = c("job"))
Datos_pruebaTTC<-select(Datos_pruebaTTC,-job)
Datos_pruebaTTC <- Datos_pruebaTTC %>% 
  mutate(
    education = case_when(
      education == "basic.4y" ~ "media",
      education == "basic.6y" ~ "basica",
      education == "basic.9y" ~ "basica",
      education == "high.school" ~ "superior",
      education == "illiterate" ~ "basica",
      education == "professional.course" ~ "media",
      education == "university.degree" ~ "superior",
      education == "unknown" ~ "basica"))
Datos_pruebaTTC<-dummy_cols(Datos_pruebaTTC, select_columns = c("education"))
Datos_pruebaTTC<-select(Datos_pruebaTTC,-education)


############################ MATRIZ DE CONFUSION ###############################

pTTC<-predict(RLOGTTC2,Datos_pruebaTTC, type = "response")
p_classTTC<-ifelse(pTTC > 0.5, 1, 0)
MCTTC<-confusionMatrix(p_classTTC, Datos_pruebaTTC$y)

# De nuestr matriz de confusion podemos obtener la siguiente información

AcurracyTTC<-(MCTTC[1,1]+MCTTC[2,2])/(MCTTC[1,1]+MCTTC[1,2]+MCTTC[2,1]+MCTTC[2,2])
AcurracyTTC

### En este punto se harán algunas aclaraciones:

# Peligro: la métrica accuracy (exactitud) no funciona bien cuando las clases están desbalanceadas. 
# Pues, la mayoría de los clientes no será acreedores a crédito, así que es muy fácil acertar
# diciendo que no se les otorgará credito.

# Acorde a lo visto en clase debemos balancer la data si la proporcion de 1 es menor al 5% y dado que para
# nuestro caso 926/(7307+926)= 0,11 es decir la proporcion es del 11% no es necesario balancear la data.
# Por tanto del valor obtenido de Acurracy podemos decir que el porcentaje de la data clasificada 
# correctamente es del 89% por lo cual tenemos un buen modelo.

Tasa_errorTTC<-(MCTTC[1,2]+MCTTC[2,1])/(MCTTC[1,1]+MCTTC[1,2]+MCTTC[2,1]+MCTTC[2,2])
Tasa_errorTTC
# Es decir que el porcentaje de la data clasificada incorrectamente
# es del 1% por lo cual tenemos un buen modelo.

############################## CURVA ROC ######################################

colAUC(pTTC,Datos_pruebaTTC$y,plotROC="TRUE")
# En este punto podemos ver que el area bajo la curva es 0.6 por lo
# cual obtuvimos un modelo aceptable de predicción.


######################### GRUPOS HOMOGENEOS DE RIESGO ##########################
pDs<-predict(RLOGTTC2,DatosEntrTTCIVDM, type = "response")
GHPIT<-DatosEntrTTC
GHPIT[,"PD"]<-pDs
GHPIT<-select(GHPIT,y,PD)

pDsP<-predict(RLOGTTC2,Datos_pruebaTTC, type = "response")
GHPITP<-Datos_pruebaTTC
GHPITP[,"PD"]<-pDsP
GHPITP<-select(GHPITP,y,PD)

GHRPIT<-rbind(GHPIT,GHPITP)
GHRPIT[,"monthd"]<-Datos$monthd
GHRPIT <- GHRPIT[!is.na(GHRPIT$PD),]


grupos = c(0, 0.2, 0.4, 0.6, 0.8, 1)
PDcat = c('0-0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1')
GHRPIT[,"PD_cat"] = cut(GHRPIT$PD, breaks = grupos, labels=PDcat)


GHRPIT[,"cambiar"] = GHRPIT$PD_cat
GHRPIT <- GHRPIT %>%
  mutate(
    cambiar = case_when(
      cambiar == "0-0.2" ~ "A",
      cambiar == "0.2-0.4" ~ "B",
      cambiar == "0.4-0.6" ~ "C",
      cambiar == "0.6-0.8" ~ "D",
      cambiar == "0.8-1" ~ "E"))

GHRPITmalos<-filter(GHRPIT, GHRPIT$y==0)
GHRPITmalos <- GHRPITmalos %>% mutate(y = replace(y,y==0,1))
names(GHRPITmalos)[5]<-"Grupo"

## GRA
GHRPITmalosGA<-filter(GHRPITmalos, GHRPITmalos$Grupo=="A")
porGA<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeA = round(sum(x)/length(GHRPITmalosGA$y),3) ),data = GHRPITmalosGA)

aGA<-porGA$y
aGA<-as.data.frame(aGA)
mes<-porGA$monthd
mes<-as.data.frame(mes)
TGA<-cbind(aGA,mes)
TGA<-select(TGA, mes,PorcentajeA)

## GRB
GHRPITmalosGB<-filter(GHRPITmalos, GHRPITmalos$Grupo=="B")
porGB<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeB = round(sum(x)/length(GHRPITmalosGB$y),3) ),data = GHRPITmalosGB)

aGB<-porGB$y
aGB<-as.data.frame(aGB)
mes<-porGB$monthd
mes<-as.data.frame(mes)
TGB<-cbind(aGB,mes)
TGB<-select(TGB, mes,PorcentajeB)

AB<-merge(x = TGA, y = TGB, all = T)

## GRC
GHRPITmalosGC<-filter(GHRPITmalos, GHRPITmalos$Grupo=="C")
porGC<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeC = round(sum(x)/length(GHRPITmalosGC$y),3) ),data = GHRPITmalosGC)

aGC<-porGC$y
aGC<-as.data.frame(aGC)
mes<-porGC$monthd
mes<-as.data.frame(mes)
TGC<-cbind(aGC,mes)
TGC<-select(TGC, mes,PorcentajeC)

BC<-merge(x = AB, y = TGC, all = T)

## GRD
GHRPITmalosGD<-filter(GHRPITmalos, GHRPITmalos$Grupo=="D")
porGD<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeD = round(sum(x)/length(GHRPITmalosGD$y),3) ),data = GHRPITmalosGD)

aGD<-porGD$y
aGD<-as.data.frame(aGD)
mes<-porGD$monthd
mes<-as.data.frame(mes)
TGD<-cbind(aGD,mes)
TGD<-select(TGD, mes,PorcentajeD)

CD<-merge(x = BC, y = TGD, all = T)

## GRE
GHRPITmalosGE<-filter(GHRPITmalos, GHRPITmalos$Grupo=="E")
porGE<-aggregate(y ~ monthd, FUN = function(x) c(Suma = sum(x), PorcentajeE = round(sum(x)/length(GHRPITmalosGE$y),3) ),data = GHRPITmalosGE)

aGE<-porGE$y
aGE<-as.data.frame(aGE)
mes<-porGE$monthd
mes<-as.data.frame(mes)
TGE<-cbind(aGE,mes)
TGE<-select(TGE, mes,PorcentajeE)

DE<-merge(x = CD, y = TGE, all = T)
MATRIZGH1<-DE
MATRIZGH1[is.na(MATRIZGH1)]<-0

#write.csv(MATRIZGH1, "GHRlala.csv")













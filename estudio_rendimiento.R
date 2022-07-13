library(readxl)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(highcharter)
library(EnvStats)
library(robustbase)
library(MASS)
library(nortest)
library(lmtest) 
library(rsm)

#Fijar el directorio donde se encuentra el conjunto de datos
#setwd("C:/Users/elvir/OneDrive/Escritorio/TFG ESTADISTICA/FORRAJES/FORRAJES")
data_lechoso <- read_excel("forrajes_lechosos.xlsx")
View(data_lechoso)

str(data_lechoso)
names(data_lechoso)

#Convertir factores las variables categoricas
data_lechoso<- mutate_at(data_lechoso, vars(1:10), as.factor)

summary(data_lechoso)

#Eliminacion de la fila que no es correcta
data_lechoso <-data_lechoso[data_lechoso$MUE != 'C', ] 

#Diferencias entre localidades
#Diagrama de cajas de la materia seca segun la localidad
ggplot(data_lechoso,
       aes(Localidad, MS))  + geom_boxplot()
aggregate(MS~ Localidad,data_lechoso, var)
aggregate(MS~ Localidad,data_lechoso, sum)

#Diferencias significativas de las concetraciones
#de materia seca en las diferentes localidades

aggregate(MS~ TRAT + Localidad,data_lechoso, sum)


#Vamos la suma de la materia seca según la densidad
  
ggplot(data_lechoso,aes(TRAT, MS))  + geom_boxplot()
aggregate(MS~ TRAT,data_lechoso, var)


#Materia seca vs localidad+tratamiento
ggplot(data_lechoso,
       aes(MS, TRAT))  + geom_boxplot(aes(colour = Localidad))

#Correlacion de las variables numéricas
chart.Correlation(data_lechoso[, c(11:19)], histogram = TRUE,
                  method = "spearman")

#Diagrama de dispersion de la materia seca VS LER
ggplot(data_lechoso, aes(MS, LER)) + geom_point()

#Diagrama de dispersion de la materia seca VS LER segun localidad
ggplot(data_lechoso, aes(MS, LER)) + geom_point(aes(colour = Localidad)) + geom_hline(yintercept = 1)

#Materia seca VS numero de malas hierbas seegún la localidad
ggplot(data_lechoso, aes(MS, PesoMS_mh)) + geom_point(aes(colour = Localidad))

#Materia seca VS peso materia seca de cebada según localidad
ggplot(data_lechoso, aes(MS, PesoMS_ceb)) + geom_point(aes(colour = Localidad))

#Materia seca VS peso materia seca de cebada según tratamiento
ggplot(data_lechoso, aes(MS, PesoMS_ceb)) + geom_point(aes(colour = TRAT))

#Materia seca VS peso materia seca de guisante segun localdiad
ggplot(data_lechoso, aes(PesoMS_guis, MS)) + geom_point(aes(colour = Localidad))

#Materia seca VS peso materia seca de guisante segun tratamiento
ggplot(data_lechoso, aes(PesoMS_guis, MS )) + geom_point(aes(colour = TRAT))


#Funcion de deteccion de outliers#

fun_outliers <- function(data, var,k = 2) {
  df <- data_to_boxplot(data, var, add_outliers=TRUE)
  highchart() %>%
    hc_add_series_list(df)
  
  print(boxplot.stats(var))
  
  #Prueba de Rosner. Deteccion de outliers.
  
  test <- rosnerTest(var, k, warn = TRUE)
  
  print(test)
}

fun_outliers(data_lechoso, data_lechoso$MS)

 ######################################################################
#Diferencias entre materia seca segun localidad y tratamiento

#Grafico de la interaccion entre localidad y tratamiento
ggplot(data = data_lechoso, aes(x = TRAT, y = MS, colour = Localidad,
                        group = Localidad)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(y  =  'mean (Rendimiento)') +

#Comprobacion de outliers
caja <- boxplot(data_lechoso$MS, col = "skyblue", ylab = "Materia seca (g/muestra)")
caja$out

#Regresion robusta
a1<-lmrob.control()
a1$k.max<-210
a1$refine.tol<-1e-07
a1$setting= "KS2014"

reg2rob<-lmrob(MS ~ TRAT + Localidad + TRAT*Localidad,
                  data = data_lechoso, na.action=na.exclude,control=a1)
summary(reg2rob)

#Error del modelo
reg2rob$s #26.4569

#Regresión robusta, menor error y mayor r-cuadrado, mismo número de regresores

#ANOVA

modelo <- aov(reg2rob, data = data_lechoso)
tabla<- as.matrix(summary(modelo))

plot(modelo)

#NORMALIDAD
qqPlot(modelo, main="QQ Plot") 
sresid <- studres(modelo)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")

xfit<-seq(min(sresid), max(sresid),length = 40) 
yfit<-dnorm(xfit)
lines(xfit, yfit)
lillie.test( modelo$residuals)
plot(modelo$residuals)

#HOMOCEDASTICIDAD
bptest(modelo)

#Grafico de materia seca VS tratamiento
ggplot(data_lechoso, aes(TRAT, MS)) + geom_boxplot()

#Se ha intentado realizar una tranformación logaritmica a los datos,
#pero esto no soluciona la falta de normalidad y homocedasticidad
#data_lechoso$MS<- log(data_lechoso$MS)

#Test de Kruskall-Wallis
kruskal.test(MS ~ Localidad, data = data_lechoso)
kruskal.test(MS ~ TRAT, data = data_lechoso)

#Test post-hoc
pairwise.wilcox.test(x = data_lechoso$MS, g = data_lechoso$Localidad, p.adjust.method = "bonferroni" )
aggregate(MS ~ Localidad, data = data_lechoso, mean)
pairwise.wilcox.test(x = data_lechoso$MS, g = data_lechoso$TRAT, p.adjust.method = "bonferroni" )
a<-aggregate(MS ~ TRAT, data = data_lechoso, mean)
a_ordenado <- a[order(a$MS,  decreasing = TRUE), ]

#Comparacion post hoc con letras
library(agricolae)
HSD.test(modelo, "TRAT", group =TRUE, console = TRUE)

#Separacion según localidades
#####ZAMADUEÑAS######

data_zam<- filter(data_lechoso, Localidad == "Zamadueñas")

modelo_zam <- aov(MS ~ TRAT ,
                  data = data_zam)

summary(modelo_zam)

lillie.test(modelo_zam$residuals)
bptest(modelo_zam)
HSD.test(modelo_zam, "TRAT", group =TRUE, console = TRUE)

#######CASTRILLO
data_cas<- filter(data_lechoso, Localidad == "Castrillo")

modelo_cas <- aov(MS ~ TRAT ,
                  data = data_cas)

summary(modelo_cas)

lillie.test(modelo_zam$residuals)
bptest(modelo_cas)
HSD.test(modelo_cas, "TRAT", group =TRUE, console = TRUE)

#######VIÑALTA
data_vin<- filter(data_lechoso, Localidad == "Viñalta")

modelo_vin <- aov(MS ~ TRAT ,
                  data = data_vin)
summary(modelo_vin)

lillie.test(modelo_vin$residuals)
bptest(modelo_vin)
HSD.test(modelo_vin, "TRAT", group =TRUE, console = TRUE)


##########################################################
#Evaluacion usando diseño de series con reemplazo

#Se calcula la seguda densidad

data_lechoso[data_lechoso$TRAT == "borde", "den_rel_cul2"] = 0.5
data_lechoso[substr(data_lechoso$TRAT,
    nchar(as.character(data_lechoso$TRAT))-1, 
    stop = nchar(as.character(data_lechoso$TRAT)))== "00", "den_rel_cul2"] = 1

data_lechoso[substr(data_lechoso$TRAT,
                    nchar(as.character(data_lechoso$TRAT))-1,
                    stop = nchar(as.character(data_lechoso$TRAT)))== "33", "den_rel_cul2"] = 0.33

data_lechoso[substr(data_lechoso$TRAT,
                    nchar(as.character(data_lechoso$TRAT))-1,
                    stop = nchar(as.character(data_lechoso$TRAT)))== "66", "den_rel_cul2"] = 0.66

data_lechoso[substr(data_lechoso$TRAT,
                    nchar(as.character(data_lechoso$TRAT))-1,
                    stop = nchar(as.character(data_lechoso$TRAT)))== "G0", "den_rel_cul2"] = 0


#Sin separación por localidades
#Calculo de variables necesarias
data_lechoso<-mutate(data_lechoso,  y_ceb = (1/LER_cultivo1)-1,
                     x_ceb = ((1-den_rel_cul1)/den_rel_cul1),
                     y_guis = (1/LER_cultivo2)-1,
                     x_guis = ((1-den_rel_cul2)/den_rel_cul2))

#Unicamente aquellos cultivos que sean un 100%
data_series<- filter(data_lechoso,( TRAT=="C66-G33" | TRAT=="C33-G66"
                                        |  TRAT=="borde" | TRAT=="C100-G0" | TRAT=="C0-G100")  )

plot(data_series$y_guis)
plot(data_series$y_ceb)

#######CEBADA#######
#Sustituimos Inf por na
data_series[data_series == Inf] <- NA
data_series[data_series == -Inf] <- NA

fun_outliers(data_series, data_series$y_ceb, 10)
#Eliminamos las observaciones que nos han resultado outliers
data_series_ceb <- data_series[-c(38),]

#Primera regresion, cebada

reg1<- lm(y_ceb ~ x_ceb,data = data_series_ceb)
summary(reg1)

ggplot(data_series_ceb, aes(x_ceb, y_ceb)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)

######GUISANTE#####
#Segunda regresion, guisante

fun_outliers(data_series, data_series$y_guis, 10)

#Eliminamos outliers
data_series_guis <- data_series[-c(85, 94, 118),]
#Segunda regresion, guisante
reg2<- lm(y_guis ~ x_guis,data = data_series_guis)
summary(reg2)

ggplot(data_series_guis, aes(x_guis, y_guis)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#####Separación por localidades#######

#####ZAMADUEÑAS######

data_zam<- filter(data_series,( TRAT=="C66-G33" | TRAT=="C33-G66"
       |  TRAT=="borde" | TRAT=="C100-G0" | TRAT=="C0-G100") & Localidad == "Zamadueñas")

#Primera regresion, cebada, zamadueñas
#Outliers
fun_outliers(data_zam, data_zam$y_ceb, 10)
#No eliminamos outliers

reg_zam_ceb<- lm(y_ceb ~ x_ceb,data = data_zam_ceb)
summary(reg_zam_ceb)

ggplot(data_zam_ceb, aes(x_ceb, y_ceb)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)


#Segunda regresión, guisante, zamadueñas
#Outliers
fun_outliers(data_zam, data_zam$y_guis, 10)

data_zam_guis <- data_zam[c(-1,-10),]
ggplot(data_zam_guis, aes(x_guis, y_guis)) + geom_point()+geom_smooth(method = "lm", se = FALSE)

reg2<- lm(y_guis ~ x_guis,data = data_zam_guis, na.action = na.omit)
summary(reg2)


#####VIÑALTA######

data_vin<- filter(data_series,( TRAT=="C66-G33" | TRAT=="C33-G66"
                                        |  TRAT=="borde" | TRAT=="C100-G0" | TRAT=="C0-G100") & Localidad == "Viñalta")


#Outliers
fun_outliers(data_vin, data_vin$y_ceb, 10)
data_vin_ceb <- data_vin[c(-30, -28),]

#Primera regresion, cebada, viñalta
reg3<- lm(y_ceb ~ x_ceb,data = data_vin_ceb)
summary(reg3)

ggplot(data_vin_ceb, aes(x_ceb, y_ceb)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)


#Segunda regresión, guisante, viñalta
#Outliers
fun_outliers(data_vin, data_vin$y_guis, 10)
data_vin_guis <- data_vin[c(-14, -13),]


reg4<- lm(y_guis ~ x_guis,data = data_vin_guis)
summary(reg4)

ggplot(data_vin_guis, aes(x_guis, y_guis)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)



#####CASTRILLO######

data_cas<- filter(data_series,( TRAT=="C66-G33" | TRAT=="C33-G66"
                                       |  TRAT=="borde" | TRAT=="C100-G0" | TRAT=="C0-G100") & Localidad == "Castrillo")


castrillo<-filter(data_lechoso,(Localidad == "Castrillo"))

#Queremos crear grupos homogeneos entre si y heterogéneos entre grupos.
ggplot(data_lechoso, aes( PesoMS_guis, LER) ) +
  geom_point(aes(colour = TRAT))

#Primera regresion, cebada

#Ouliers
fun_outliers(data_cas, data_cas$y_ceb, 10)
data_cas_ceb <- data_cas[c(-38),]

reg5<- lm(y_ceb ~ x_ceb,data = data_cas_ceb)
summary(reg5)

ggplot(data_cas, aes(x_ceb, y_ceb)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)


#Segunda regresión, guisante

#Ouliers
fun_outliers(data_cas, data_cas$y_guis, 10)
data_cas_guis <- data_cas[c(-34),]

reg6<- lm(y_guis ~ x_guis,data = data_cas_guis)
summary(reg6)

ggplot(data_cas, aes(x_guis, y_guis)) + geom_point()+geom_smooth(method = "lm", se = FALSE)


######Evaluación usando diseño de superficie respuesta#######

names(data_lechoso)[22] = "Cebada"
names(data_lechoso)[23] = "Guisante"

#Codificacion los factores

data_lechoso<- coded.data(data = data_lechoso, 
                          x1 ~ (Cebada - 0.5)/0.5, x2 ~ (Guisante - 0.5)/0.5)

ggplot(data = data_lechoso, aes(x1, x2)) +
  geom_point(col = "green") +
  geom_vline(xintercept = c(-1,1), col = "lightgrey") +
  geom_hline(yintercept = c(-1,1) , col = "lightgrey")

#Diseño de primer orden
modelo_respuesta <- rsm(MS ~ FO(x1, x2), data = data_lechoso)

summary(modelo_respuesta) 

#Diseño de segundo grado
modelo_respuesta <- rsm(MS ~ SO(x1, x2), data = data_lechoso)

summary(modelo_respuesta) 


#Modelo 2 : diseño sin interaccion
modelo_respuesta <- rsm(MS ~ FO(x1, x2) +
                          PQ(x1, x2), data = data_lechoso)


summary(modelo_respuesta)

#Sin interaccion y sin el termino cuadratico de la segunda variable
modelo_respuesta <- rsm(MS ~ FO(x1, x2) +
                          PQ(x1), data = data_lechoso)

summary(modelo_respuesta)

persp(modelo_respuesta, x2 ~ x1, 
      zlab = "Materia seca (Rend)", 
      contours = list(z = "bottom", col = "colors"), # posicion y color
      at = c(summary(modelo_respuesta$canonical$xs)),
      theta = 40, # coordenadas graficas
      phi = 10, 
      nlevels = 20)

# Grafico de contornos
contour(modelo_respuesta, ~ x1 + x2 , image = TRUE, nlevels = 20)

#Diagnostico del modelo
par(mfrow = c(2, 2))

# Diagnostico grafico de los datos del modelo
plot(modelo_respuesta)

############################################################
#Graficos de lineas y puntos

densidad_guisante <- c(0, 0.33333333333333, 0.5, 0.6666666666666, 1)
densidad_cebada <- c(1, 0.66666666666666, 0.5, 0.3333333333333, 0)
df <- data.frame(x = densidad_guisante, y = densidad_cebada)

ggplot(df, aes(x = densidad_guisante, y = densidad_cebada)) +
  geom_line(color = 4,    # Color de la línea
            lwd = 1.2,      # Ancho de la línea
            linetype = 1)+
  geom_point(lwd = 1.7) 


densidad_guisante <- c(0, 1/3, 0.5, 2/3, 1, 0, 0, 0,
                       0,1/3, 1,
                       1/3, 1/3, 2/3, 2/3,2/3, 1, 1)
densidad_cebada <- c(1, 2/3, 0.5, 1/3,
                     0,1/3,2/3, 1,
                     1/3, 1/3, 1/3,
                     0,1, 0, 2/3,1, 2/3, 1 )
df <- data.frame(x = densidad_guisante, y = densidad_cebada)

ggplot(df, aes(x = densidad_guisante, y = densidad_cebada)) +
  geom_point(lwd = 1.7) 

#############################################################

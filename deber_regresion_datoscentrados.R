install.packages("readxl", dependencies=TRUE)
library(readxl)
ls("package:readxl")

data <- read_excel("data_rls_uti.xlsx", sheet=1,na="") #na:sign la representacion de los datos perdidos
str(data)
View(data)

reg <- lm(Utilidad~Ventas, data)
str(reg)
summary(reg) 
anova <- aov(reg)
summary(anova)
qt(0.975,df=38)
qf(0.95,df=1,df2=38)

####Centrar los Datos######


mediau <- mean(data[,"Utilidad"])
mediau
mediav <- mean(data[,"Ventas"])
mediav

utilidad_c <- data[,"Utilidad"]-mediau
utilidad_c
ventas_c <- data[,"Ventas"]-mediav
ventas_c

#Regresion con los datos centrados
regresion_c <- lm(utilidad_c~ventas_c, data)
str(regresion_c)
summary(regresion_c) 
anova1 <- aov(regresion_c)
summary(anova1)

qt(0.975,df=38)
qf(0.95,df1=1,df2=38)



#INTERVALOS DE CONFIANZA
confint(regresion_c, level=0.95) #level: nivel de confianza

names(regresion_c)
str(regresion_c)
str(regresion_c[["residuals"]])
residuo1 <-regresion_c[["residuals"]]
predicciones1 <- regresion_c[["fitted.values"]]
data_c <- data.frame(utilidad_c, ventas_c, predicciones1, residuo1)
View(data_c)
#utilidad_c:Yi, ventas_c:Xi, predicciones1:Yi techo, residuo1:Ui techo

hist(residuo1,15) #Histograma de los Ui techo
mean(residuo1) #media
qqnorm(residuo1) #grafico de la normalidad
qqline(residuo1,col="purple") #grafico de la normalidad con la recta


plot(residuo1,predicciones1)
plot(ventas_c,utilidad_c)
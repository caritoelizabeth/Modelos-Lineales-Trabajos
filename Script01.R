##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Carolina Elizabeth Yépez Castillo


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()


data <- read.table("data.txt", header = TRUE, dec=",", sep="\t")

str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

edad <- data[,"Edad"]
minimo_edad <- min(edad,na.rm =T)
maximo_edad <- max(edad,na.rm =T)
media_edad <- mean(edad,na.rm = T)

minimo_edad # 14

maximo_edad # 112

media_edad # 41.73808

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

data_fem <- subset(data, subset=data[,"Genero"]=="Femenino")

table(data[,"Genero"])

table(data_fem[,"Genero"])

num_fem <- nrow(data_fem)

num_fem # 6183


# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_si_dependiente<-subset(data, subset=data[,"Dependiente"]=="Si")

table(data[,"Dependiente"])

table(data_si_dependiente[,"Dependiente"])

edad_dependiente <-data_si_dependiente[,"Edad"]

minimo_dependiente <- min(edad_dependiente, na.rm =T)
maximo_dependiente <- max(edad_dependiente, na.rm =T)
media_dependiente <- mean(edad_dependiente, na.rm = T)

minimo_dependiente # 22
maximo_dependiente # 66
media_dependiente # 41.28607

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipo_elemento <- numeric(ncol(data))
for(i in 1:ncol(data)){
  tipo_elemento[i] <- typeof(data[,i])
}

tipo_elemento

# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase_columna <- numeric(ncol(data))
for(i in 1:ncol(data)){
  clase_columna[i] <- class(data[,i])
}

clase_columna

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

media_variables <- numeric(ncol(data))
for (i in 1:ncol(data)){
  media_variables[i] <- mean(data[,i],na.rm=T)
}

media_variables

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

valores_perdidos <- numeric(ncol(data))

porcentaje <- numeric(ncol(data))

total<-nrow(data)

for(i in 1: ncol(data)){
  valores_perdidos[i] <- sum(is.na(data[,i]))
  porcentaje[i] <- (valores_perdidos[i]/total)
  
}

valores_perdidos

prop.table(valores_perdidos)

porcentaje 

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

edadmayor_40 <- subset(data, subset=data[,"Edad"]>40)

table(edadmayor_40[,"Edad"])

n <- nrow(edadmayor_40)

n
# n=8623 personas tienen una edad mayor a 40 años

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

vivienda_propia <-subset(data, subset=data[,"Vivienda"]=="Propia")

table(data[,"Vivienda"])

table(vivienda_propia[,"Vivienda"])

p <- nrow(vivienda_propia)

p
# p=784 personas tienen vivienda propia


# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

cargas_familiares <- subset(data, subset=data[,"Cargas"]>2)

table(data[,"Cargas"])

table(cargas_familiares[,"Cargas"])

q <- nrow(cargas_familiares)

q
#q=11 personas tienen más de dos cargas familiares

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

deuda_dias_de_atraso<-subset(data,(subset=data[,"Deuda"]>=500)&(subset =data[,"Dias_Atraso"]>8))

table(deuda_dias_de_atraso[,"Deuda"], deuda_dias_de_atraso[,"Dias_Atraso"])

m <- nrow(deuda_dias_de_atraso)

m 
# m=38 sujetos tienen una deusa superior a 500dolares y mas de 8 dias de atraso

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

score_edad_tarjetas<-subset(data,(subset=data[,"Score"]>="900")&(subset =data[,"Edad"]<="35")&(subset =data[,"Numero_TC"]>3))

table(score_edad_tarjetas[,"Score"], score_edad_tarjetas[,"Edad"],score_edad_tarjetas[,"Numero_TC"])

s <- nrow(score_edad_tarjetas)

s
# s=2934 personas tienen un score>=900 puntos, una edad<=35,mas de 3 tarjetas

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad, col="red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(edad, col = "green")

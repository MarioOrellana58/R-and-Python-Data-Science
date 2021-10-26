install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(DBI)
library(odbc)

#Creacion de diferentes objetos

ObjetoCadena <- "Esta es una cadena"
ObjetoEntero <- 2
ObjetoVector <- c(1:5) #concatenar todos los números del 1 al 5


#exploración de objetos

str(ObjetoCadena)
str(ObjetoEntero)
str(ObjetoVector)

typeof(ObjetoCadena)
typeof(ObjetoEntero)
typeof(ObjetoVector)

class(ObjetoCadena)
class(ObjetoEntero)
class(ObjetoVector)

#Data frames, colección de datos, aquí se instancian los datos
#atributos = columnas, registros = observaciones = filas
Temperaturas <- data.frame(#se le mandan listas de parámetros
                            Anios=c(2015,2016,2017,2018),
                            Invierno=c(5,8,7,10),
                            Primavera=c(10,12,15,13),
                            Verano=c(25,26,29,32),
                            Otonio=c(13,14,12,10)
                          )


TemperaturasRandom <- data.frame(
                                  Anios=(sample(c(2015:2018),20,replace=TRUE)),
                                  Invierno=rnorm(20, mean=2, sd=2),
                                  Primavera=rnorm(20, mean=15, sd=3),
                                  Verano=rnorm(20, mean=22, sd=4),
                                  Otonio=rnorm(20, mean=10, sd=2)
                                )

#rm(objeto) es para eliminar un objeto del script

Temperaturas$Verano
head(Temperaturas,2)
tail(Temperaturas,2)


#dplyr
  #%>% filter
  #%>% summarise resumir info y crear dataset de eso
  #%>% subset
  #%>% arrange
  #%>% prácticamente es como correr queries
  
  #filtrar información
  Temperaturas %>% filter(Anios==2018)
  Temperaturas %>% slice(1:2)
  
  #Ordenar
  Temperaturas %>% arrange(Invierno)
  Temperaturas %>% arrange(desc(Invierno))
  
  
  #GroupBys
  TemperaturasRandom %>% summarise(TemperaturaPromedio = mean(Invierno))
  TemperaturasRandom %>% group_by(Anios) %>% summarise(TemperaturaPromedio = mean(Invierno))
  
  TemperaturasRandom %>% slice(1:5) %>% group_by(Anios) %>% summarise(TemperaturaPromedio = mean(Invierno))

#ggplot2
  
TemperaturasRandomPromedio <-  TemperaturasRandom %>% group_by(Anios) %>% summarise(TemperaturaPromedio = mean(Invierno))

  #aes = aesynthetics, es la definición del plano cartesiano 
  #geom line = gráfico de líneas
  #geom text = solo añade data labels
ggplot(data=TemperaturasRandomPromedio, aes(x=Anios,y=TemperaturaPromedio)) + 
  geom_line() + 
  geom_text(
    label=TemperaturasRandomPromedio$TemperaturaPromedio,
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T
  )

con <- dbConnect(odbc(), Driver = "SQL Server", Server = "LAPTOP-MHM32UUF",
                 Database = "Admisiones_DWH")

dfsql <- dbGetQuery(conn=con, "SELECT e.*, c.NombreFacultad
                            FROM Fact.Examen E INNER JOIN
                              Dimension.Carrera c on (e.sk_carrera = c.sk_carrera)")

Df_ConteoPorFacultad <- dfsql %>% count(NombreFacultad)

ggplot(Df_ConteoPorFacultad, aes(x="",y=n, fill=NombreFacultad)) +
  geom_bar(stat="identity", width = 1) + 
  coord_polar("y", start = 0)
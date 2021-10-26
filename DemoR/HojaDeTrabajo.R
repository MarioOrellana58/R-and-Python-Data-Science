install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(DBI)
library(odbc)


#a. Cree una conexión a SQL Server a la base de datos de AdmisionesDWH
con <- dbConnect(odbc(), Driver = "SQL Server", Server = "LAPTOP-MHM32UUF", Database = "Admisiones_DWH")


#---------------------------------------------------------------------------------------------
#b. Utilice las librerías para manipulación de datos vistas en clase y cree data frames para las siguientes 
    #operaciones


  #i. Un conteo de la cantidad de exámenes que se realizaron agrupado por facultad
    
    dfsql <- dbGetQuery(conn=con, "SELECT e.Precio, e.NotaTotal, car.NombreFacultad, car.NombreCarrera, can.Genero, f.Year
                                  	FROM Fact.Examen e
                                  	INNER JOIN Dimension.Carrera car on (e.sk_carrera = car.sk_carrera)
                                  	INNER JOIN Dimension.Candidato can on (e.SK_Candidato = can.SK_Candidato)
                                    INNER JOIN Dimension.Fecha f on (e.DateKey = f.DateKey)"
                        )
    
    Df_ConteoPorFacultad <- dfsql %>% count(NombreFacultad)
  
    
    
  
  #ii. Un conteo de la cantidad de candidatos agrupados por genero
  
    Df_ConteoPorGenero <- dfsql %>% count(Genero)
  
    
    
    
  #iii. Un total de la cantidad de ingresos por evaluaciones (columna precio) que se ha recibido 
        #agrupado por carrera
    
    Df_IngresosPorCarrera <- dfsql %>% group_by(NombreCarrera)  %>% summarise(TotalIngresos = sum(Precio))
    
    
    
    
    
  #iv. Un data frame filtrado que contenga únicamente las tres facultades con el promedio de la 
        #nota mas alto
    Df_FacultadesPromMasAlto <- dfsql %>% group_by(NombreFacultad)  %>% summarise(PromedioFacultad = mean(NotaTotal)) %>% arrange(desc(PromedioFacultad)) %>% slice(1:3)

    
    
#---------------------------------------------------------------------------------------------
#c. Utilice las librerías para plot (graficas) y cree las siguientes graficas
    
    
    
    #i. Un grafico de pie con la cantidad de exámenes agrupados por facultad
    
      ggplot(Df_ConteoPorFacultad, aes(x="",y=n, fill=NombreFacultad)) +
        geom_bar(stat="identity", width = 1) + 
        coord_polar("y", start = 0)
      
      
      
      
    #ii. Un grafico de barras con el promedio de la nota agrupado por carrera
      Df_PromediosNotasCarrera <- dfsql %>% group_by(NombreCarrera)  %>% summarise(PromedioCarrera = mean(NotaTotal))
      
      #------------------ ya solo esta gráfica está mala ----------------------------
      
      ggplot(data= data.frame(Df_PromediosNotasCarrera), aes(x=NombreCarrera, y=PromedioCarrera)) + 
        geom_bar(stat="identity", position="stack") 
      #------------------ ya solo esta gráfica está mala ----------------------------
      
      
      
      
    #iii. Un grafico de línea con la cantidad de exámenes por año
      Df_ConteoPorAño <- dfsql %>% count(Year)
      
      ggplot(data=Df_ConteoPorAño, aes(x=Year,y=n)) + 
        geom_line() + 
        geom_text(
          label=Df_ConteoPorAño$n,
          nudge_x = 0.25, nudge_y = 0.25,
          check_overlap = T
        )
      
      
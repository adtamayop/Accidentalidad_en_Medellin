library(shiny)      # -_-
library(lubridate)  #parse_date_time functio
library(raster)
library(leaflet)
library(colorRamps)
library(plotly)
library(readxl)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(rgdal)      #use_iconv function
library(car)        #recode function
options(encoding = "UTF-8")

shinyServer(function(input, output) {
  
  withProgress(message = "Por favor espere",value = 0, {
    
    
    accidentes15 <-shapefile("Accidentes/Accidentalidad_2015/Accidentalidad_2015.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentes16 <-shapefile("Accidentes/Accidentalidad_2016/Accidentalidad_2016.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    accidentes17 <-shapefile("Accidentes/Accidentalidad_2017/Accidentalidad_2017.shp",encoding="UTF-8",use_iconv=TRUE)
    incProgress(25)
    
    barrios_med <- shapefile("Accidentes/Barrio/Barrio_Vereda.shp",encoding="UTF-8",use_iconv=TRUE)
    
    accidentes15@data$CLASE <- Recode(accidentes15@data$CLASE, '"Atropello"="Atropello";
    "Caída Ocupante"="Caída de Ocupante";"Caida Ocupante"="Caída de Ocupante"; "Choque"="Choque";"Incendio"="Incendio";
    "Otro"="Otro";"Volcamiento"="Volcamiento"; "Choque y Atropello"="Choque y Atropello"',as.factor.result = T)
      
      accidentes16@data$CLASE <- Recode(accidentes16@data$CLASE, '"Atropello"="Atropello";
    "Caída Ocupante"="Caída de Ocupante";"Caida Ocupante"="Caída de Ocupante"; "Choque"="Choque";"Incendio"="Incendio";
    "Otro"="Otro";"Volcamiento"="Volcamiento"; "Choque y Atropello"="Choque y Atropello"',as.factor.result = T)
      
      accidentes17@data$CLASE <- Recode(accidentes17@data$CLASE, '"Atropello"="Atropello";
    "Caída Ocupante"="Caída de Ocupante";"Caida Ocupante"="Caída de Ocupante"; "Choque"="Choque";"Incendio"="Incendio";
    "Otro"="Otro";"Volcamiento"="Volcamiento"; "Choque y Atropello"="Choque y Atropello"',as.factor.result = T)
      
    accidentes15@data$HORA<-parse_date_time(accidentes15@data$HORA, '%I:%M %p')
    accidentes16@data$HORA<-parse_date_time(accidentes16@data$HORA, '%I:%M %p')
    accidentes17@data$HORA<-parse_date_time(accidentes17@data$HORA, '%I:%M %p')
    accidentalidad <- accidentes15
    incProgress(25)
  })
  
  cargarBaseDeDatos <- reactive({
    switch(input$año,
           "2015" = accidentes15,
           "2016" = accidentes16,
           "2017" = accidentes17
    )  
  })
  
  output$Barrios <- renderUI({
    accidentalidad <- cargarBaseDeDatos()
    
    if(input$busqueda == 'Barrio'){
      selectInput("nombreBarrio","Barrios",choices = c(unique(accidentalidad@data$COMUNA)))
    }
  }) 
  
  output$tipo <- renderUI({
    
    accidentalidad <- cargarBaseDeDatos()
    
    if(input$busqueda == 'Tipo de accidente'){
      selectInput("tipoaccidente","Tipo",choices = c(as.character(unique(accidentalidad@data$CLASE))))
    }
  }) 
  
  output$año_seleccionado <- renderText({
    paste("La base de datos seleccionada es del año ",input$año) 
  })
  
  
  output$mapa <- renderLeaflet({
    
    accidentalidad <- cargarBaseDeDatos()
    
    switch(input$busqueda,
           "Barrio" = {if(is.null(input$nombreBarrio)){

           }else if(input$nombreBarrio == "NA"){
             accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
           }else if(input$nombreBarrio == "0"){
             accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == 0)
           }else{
             accidentalidad <- subset(accidentalidad, accidentalidad@data$COMUNA == input$nombreBarrio)
           }
             select <- input$nombreBarrio},
           
           "Tipo de accidente" = {if(is.null(input$tipoaccidente)){ 
             
           }else if(input$tipoaccidente == "NA"){
             accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
           }else if(input$tipoaccidente == "0"){
             accidentalidad <- subset(accidentalidad, is.na(accidentalidad@data$COMUNA))
           }else {
             accidentalidad <- subset(accidentalidad, accidentalidad@data$CLASE == input$tipoaccidente)
           }
             select <- input$tipoaccidente
             },
           
           "Hora" = {
             
             Inicio <- input$hoursRange[1] 
             Fin <- input$hoursRange[2]  
             
             accidentalidad@data$HORA <- as.double(substr(accidentalidad@data$HORA,11,13))
             
             accidentalidad <- subset(accidentalidad, accidentalidad@data$HORA >= Inicio & accidentalidad@data$HORA < Fin)}
           )
    
    
    if(!is.null(select)){
      
      popup<-paste(accidentalidad@data$BARRIO)
      
      m<-leaflet()
      m<-fitBounds(m, lng1=-75.57038, 
                   lat1=6.26143, 
                   lng2=-75.55259,
                   lat2=6.29112)
      m<-addProviderTiles(m,provider="OpenStreetMap.Mapnik")
      
      m<-addCircleMarkers(m,
                          lng = accidentalidad@coords[,1],
                          lat = accidentalidad@coords[,2],
                          popup = popup, 
                          radius = 2, 
                          stroke = FALSE,
                          fillOpacity = 0.75,
                          fillColor = "#FF0000"
      )
      m <- setView(m, mean(accidentalidad@coords[,1]), mean(accidentalidad@coords[,2]), zoom=14)
      m
    } 
  })
  
  
  output$mapa_calor <- renderLeaflet({
    
    accidentalidad <- cargarBaseDeDatos()
    
    proj4string(accidentalidad) <- proj4string(barrios_med)
    
    comuna_ocurrencia<-over(accidentalidad, barrios_med)
    
    freq_acc_comuna<-table(comuna_ocurrencia$NOMBRE)
    freq_acc_comuna<-as.data.frame(freq_acc_comuna)
          
    id_x <- match(barrios_med@data$NOMBRE,freq_acc_comuna$Var1)
    barrios_med@data$cant_acc<-freq_acc_comuna$Freq[id_x]
      
    pal <-colorNumeric(palette=blue2red(1000),domain=c(1,1000),na.color = "#FF69B4")
    popup<-paste(barrios_med@data$NOMBRE,paste(barrios_med@data$cant_acc," accidentes"),sep="<br/>")
    
    accidentes_barrios<-leaflet(barrios_med)
    accidentes_barrios<-addProviderTiles(accidentes_barrios,provider="OpenStreetMap.Mapnik")
    accidentes_barrios<-addPolygons(accidentes_barrios,
                                    popup=popup,
                                    color=pal(barrios_med@data$cant_acc),
                                    opacity = 0.5,
                                    fillOpacity = 0.5,
                                    weight = 1)
    accidentes_barrios<-addLegend(accidentes_barrios,"topright",pal=pal,values=barrios_med@data$cant_acc, 
                                  title="Cantidad de accidentes",
                                  opacity = 1)
    accidentes_barrios
  
  })
  
  output$barras <- renderPlotly({
    
    accidentalidad <- cargarBaseDeDatos()
    accidentalidad2 <- as.data.frame(accidentalidad)
    
    DIA <- accidentalidad@data$DIA
    CLASE <- accidentalidad@data$CLASE
    GRAVEDAD <- accidentalidad@data$GRAVEDAD
    DISENO <- accidentalidad@data$DISENO
    Dias <- levels(as.factor(DIA))
    clase <- levels(as.factor(CLASE))
    gravedad <- levels(as.factor(GRAVEDAD))
    diseno <- levels(as.factor(DISENO))
    t <- list(size = 12,color= "white")
    
    entrada <- input$botones #paste(input$eje_x,input$eje_y,sep = " vs ")
    
    
    if((entrada == "DIA vs CLASE")){
    
      plot_ly(accidentalidad2, x = ~Dias, y = ~table(DIA,CLASE)[,1], type = 'bar', name = 'Atropello') %>%
      add_trace(y = ~table(DIA,CLASE)[,2], name = 'Caida Ocupante') %>%
      add_trace(y = ~table(DIA,CLASE)[,3], name = 'Choque') %>%
      add_trace(y = ~table(DIA,CLASE)[,4], name = 'Incendio') %>%
      add_trace(y = ~table(DIA,CLASE)[,5], name = 'Otro') %>%
      add_trace(y = ~table(DIA,CLASE)[,6], name = 'Volcamiento') %>%
      layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Dia por Tipo de Accidente',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
    
    }
    else if((entrada == "DIA vs GRAVEDAD")){
        
        plot_ly(accidentalidad2, x = ~Dias, y = ~table(DIA,GRAVEDAD)[,1], type = 'bar', name = 'Herido') %>%
        add_trace(y = ~table(DIA,GRAVEDAD)[,2], name = 'Muerto') %>%
        add_trace(y = ~table(DIA,GRAVEDAD)[,3], name = 'Solo daños') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Dia por Gravedad',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if((entrada == "DIA vs DISEÑO")){
        
        plot_ly(accidentalidad2, x = ~Dias, y = ~table(DIA,DISENO)[,1], type = 'bar', name = 'Ciclo ruta') %>%
        add_trace(y = ~table(DIA,DISENO)[,2], name = 'Glorieta') %>%  
        add_trace(y = ~table(DIA,DISENO)[,3], name = 'Interseccion') %>%
        add_trace(y = ~table(DIA,DISENO)[,4], name = 'Lote o predio') %>%
        add_trace(y = ~table(DIA,DISENO)[,5], name = 'Paso a nivel') %>%
        add_trace(y = ~table(DIA,DISENO)[,6], name = 'Paso elevado') %>%
        add_trace(y = ~table(DIA,DISENO)[,7], name = 'Paso inferior') %>%
        add_trace(y = ~table(DIA,DISENO)[,8], name = 'Ponton') %>%
        add_trace(y = ~table(DIA,DISENO)[,9], name = 'Puente') %>%
        add_trace(y = ~table(DIA,DISENO)[,10], name = 'Tramo de via') %>%
        add_trace(y = ~table(DIA,DISENO)[,11], name = 'Tunel') %>%
        add_trace(y = ~table(DIA,DISENO)[,12], name = 'Via Peatonal') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Dia por Tipo de Diseño vial',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if((entrada == "CLASE vs DIA")){
      
      plot_ly(accidentalidad2, x = ~clase, y = ~table(CLASE,DIA)[,1], type = 'bar', name = 'Domingo') %>%
        add_trace(y = ~table(CLASE,DIA)[,2], name = 'Jueves') %>%
        add_trace(y = ~table(CLASE,DIA)[,3], name = 'Lunes') %>%
        add_trace(y = ~table(CLASE,DIA)[,4], name = 'Martes') %>%
        add_trace(y = ~table(CLASE,DIA)[,5], name = 'Miercoles') %>%
        add_trace(y = ~table(CLASE,DIA)[,6], name = 'Sabado') %>%
        add_trace(y = ~table(CLASE,DIA)[,7], name = 'Viernes') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Tipo de accidente por Dia',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
      
    }else if((entrada == "CLASE vs GRAVEDAD")){
      
        plot_ly(accidentalidad2, x = ~clase, y = ~table(CLASE,GRAVEDAD)[,1], type = 'bar', name = 'Herido') %>%
        add_trace(y = ~table(CLASE,GRAVEDAD)[,2], name = 'Muerto') %>%
        add_trace(y = ~table(CLASE,GRAVEDAD)[,3], name = 'Solo daños') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Tipo de accidente por Gravedad',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
      
    }else if((entrada == "CLASE vs DISEÑO")){
      
      plot_ly(accidentalidad2, x = ~clase, y = ~table(CLASE,DISENO)[,1], type = 'bar', name = 'Ciclo ruta') %>%
        add_trace(y = ~table(CLASE,DISENO)[,2], name = 'Glorieta') %>%
        add_trace(y = ~table(CLASE,DISENO)[,3], name = 'Interseccion') %>%
        add_trace(y = ~table(CLASE,DISENO)[,4], name = 'Lote o predio') %>%
        add_trace(y = ~table(CLASE,DISENO)[,5], name = 'Paso a nivel') %>%
        add_trace(y = ~table(CLASE,DISENO)[,6], name = 'Paso elevado') %>%
        add_trace(y = ~table(CLASE,DISENO)[,7], name = 'Paso inferior') %>%
        add_trace(y = ~table(CLASE,DISENO)[,8], name = 'Ponton') %>%
        add_trace(y = ~table(CLASE,DISENO)[,9], name = 'Puente') %>%
        add_trace(y = ~table(CLASE,DISENO)[,10], name = 'Tramo de via') %>%
        add_trace(y = ~table(CLASE,DISENO)[,11], name = 'Tunel') %>%
        add_trace(y = ~table(CLASE,DISENO)[,12], name = 'Via Peatonal') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Tipo de accidente por Tipo de diseño vial',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if((entrada == "GRAVEDAD vs DIA")){
      
      plot_ly(accidentalidad2, x = ~gravedad, y = ~table(GRAVEDAD,DIA)[,1], type = 'bar', name = 'Domingo') %>%
        add_trace(y = ~table(GRAVEDAD,DIA)[,2], name = 'Jueves') %>%
        add_trace(y = ~table(GRAVEDAD,DIA)[,3], name = 'Lunes') %>%
        add_trace(y = ~table(GRAVEDAD,DIA)[,4], name = 'Martes') %>%
        add_trace(y = ~table(GRAVEDAD,DIA)[,5], name = 'Miercoles') %>%
        add_trace(y = ~table(GRAVEDAD,DIA)[,6], name = 'Sabado') %>%
        add_trace(y = ~table(GRAVEDAD,DIA)[,7], name = 'Viernes') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Gravedad por Dias',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
      
      
    }else if((entrada == "GRAVEDAD vs CLASE")){
      
      plot_ly(accidentalidad2, x = ~gravedad, y = ~table(GRAVEDAD,CLASE)[,1], type = 'bar', name = 'Atropello') %>%
        add_trace(y = ~table(GRAVEDAD,CLASE)[,2], name = 'Caida Ocupante') %>%
        add_trace(y = ~table(GRAVEDAD,CLASE)[,3], name = 'Choque') %>%
        add_trace(y = ~table(GRAVEDAD,CLASE)[,4], name = 'Incendio') %>%
        add_trace(y = ~table(GRAVEDAD,CLASE)[,5], name = 'Otro') %>%
        add_trace(y = ~table(GRAVEDAD,CLASE)[,6], name = 'Volcamiento') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Gravedad por Tipo de Accidente',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
      
      
    }else if((entrada == "GRAVEDAD vs DISEÑO")){
      
      plot_ly(accidentalidad2, x = ~gravedad, y = ~table(GRAVEDAD,DISENO)[,1], type = 'bar', name = 'Ciclo ruta') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,2], name = 'Glorieta') %>%
        add_trace(y = ~table(GRAVEDAD ,DISENO)[,3], name = 'Interseccion') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,4], name = 'Lote o predio') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,5], name = 'Paso a nivel') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,6], name = 'Paso elevado') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,7], name = 'Paso inferior') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,8], name = 'Ponton') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,9], name = 'Puente') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,10], name = 'Tramo de via') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,11], name = 'Tunel') %>%
        add_trace(y = ~table(GRAVEDAD,DISENO)[,12], name = 'Via Peatonal') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Gravedad por Tipo de diseño vial',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if((entrada == "DISEÑO vs DIA")){
      
      plot_ly(accidentalidad2, x = ~diseno, y = ~table(DISENO,DIA)[,1], type = 'bar', name = 'Domingo') %>%
        add_trace(y = ~table(DISENO,DIA)[,2], name = 'Jueves') %>%
        add_trace(y = ~table(DISENO,DIA)[,3], name = 'Lunes') %>%
        add_trace(y = ~table(DISENO,DIA)[,4], name = 'Martes') %>%
        add_trace(y = ~table(DISENO,DIA)[,5], name = 'Miercoles') %>%
        add_trace(y = ~table(DISENO,DIA)[,6], name = 'Sabado') %>%
        add_trace(y = ~table(DISENO,DIA)[,7], name = 'Viernes') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Tipo de diseño vial por Dias',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if((entrada == "DISEÑO vs CLASE")){
      
      plot_ly(accidentalidad2, x = ~diseno, y = ~table(DISENO,CLASE)[,1], type = 'bar', name = 'Atropello') %>%
        add_trace(y = ~table(DISENO,CLASE)[,2], name = 'Caida Ocupante') %>%
        add_trace(y = ~table(DISENO,CLASE)[,3], name = 'Choque') %>%
        add_trace(y = ~table(DISENO,CLASE)[,4], name = 'Incendio') %>%
        add_trace(y = ~table(DISENO,CLASE)[,5], name = 'Otro') %>%
        add_trace(y = ~table(DISENO,CLASE)[,6], name = 'Volcamiento') %>%
        layout(yaxis = list(title = 'Recuento'), barmode = 'group', title = 'Tipo de diseño vial por Tipo de Accidente',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
      
      
    }else if((entrada == "DISEÑO vs GRAVEDAD")){
      plot_ly(accidentalidad2, x = ~diseno, y = ~table(DISENO,GRAVEDAD)[,1], type = 'bar', name = 'Herido') %>%
        add_trace(y = ~table(DISENO,GRAVEDAD)[,2], name = 'Muerto') %>%
        add_trace(y = ~table(DISENO,GRAVEDAD)[,3], name = 'Solo daños') %>%
        layout(yaxis = list(title = 'Cantidad'), barmode = 'group', title = 'Tipo de diseño vial por Gravedad',plot_bgcolor='rgb(224, 224, 224 )',paper_bgcolor='rgb(39, 42, 47)',font = t)
    }
      
  })
  
  
  output$tortas <- renderPlotly({
    
    accidentalidad <- cargarBaseDeDatos()
    accidentalidad2 <- as.data.frame(accidentalidad)
    
    DIA <- accidentalidad@data$DIA
    CLASE <- accidentalidad@data$CLASE
    GRAVEDAD <- accidentalidad@data$GRAVEDAD
    DISENO <- accidentalidad@data$DISENO
    
    t <- list(size = 12,color= "white")
    Dias <- levels(as.factor(DIA))
    
    
    
    if(input$botones2 == "ACCIDENTES POR DIA"){
      
    algo <- data.frame("Categorie"=Dias, table(DIA))
    plot_ly(algo,labels = ~Categorie, values = ~Freq, textposition = 'inside',textinfo = 'label+percent') %>%
      add_pie(hole = 0.6) %>%
      layout(title = "PORCENTAJE DE ACCIDENTES POR DIA",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             plot_bgcolor='rgb(39,42,47)',paper_bgcolor='rgb(39, 42, 47)',font = t)
    
    }else if(input$botones2 == "ACCIDENTES POR CLASE"){
      clase <- levels(as.factor(CLASE))
      algo1 <- data.frame("Categorie"=clase, table(CLASE))
      
      plot_ly(algo1,labels = ~Categorie, values = ~Freq, textposition = 'inside',textinfo = 'label+percent') %>%
        add_pie(hole = 0.6) %>%
        layout(title = "PORCENTAJE DE ACCIDENTES POR CLASE",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               plot_bgcolor='rgb(39,42,47)',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if(input$botones2 == "ACCIDENTES POR GRAVEDAD"){
      
      gravedad <- levels(as.factor(GRAVEDAD))
      algo2 <- data.frame("Categorie"=gravedad, table(GRAVEDAD))
      
      plot_ly(algo2,labels = ~Categorie, values = ~Freq, textposition = 'inside',textinfo = 'label+percent') %>%
        add_pie(hole = 0.6) %>%
        layout(title = "PORCENTAJE DE ACCIDENTES POR GRAVEDAD",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               plot_bgcolor='rgb(39,42,47)',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }else if(input$botones2 == "ACCIDENTES POR TIPO DE DISEÑO VIAL"){
      
      diseno <- levels(as.factor(DISENO))
      algo3<- data.frame("Categorie"=diseno, table(DISENO))
      
      plot_ly(algo3,labels = ~Categorie, values = ~Freq, textposition = 'inside',textinfo = 'label+percent') %>%
        add_pie(hole = 0.6) %>%
        layout(title = "PORCENTAJE DE ACCIDENTES POR TIPO DE DISEÑO VIAL",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               plot_bgcolor='rgb(39,42,47)',paper_bgcolor='rgb(39, 42, 47)',font = t)
      
    }
    
    
    
    
    
    
    
    
    
  })
    
  
})
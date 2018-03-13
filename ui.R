library(shinythemes)
library(shiny)
library(leaflet)
library(plotly)
library(rgdal)      #use_iconv function
library(car)        #recode function
options(encoding = "UTF-8")
shinyUI(
  
  fluidPage(theme = shinytheme('slate'),
            
      navbarPage("Find your safety home",
        
        tabPanel(title = "Mapas",
        
            fluidRow(
              column(4,"",
                     
                     titlePanel(h1("Accidentalidad en Medellín")), #,align = "center"
                     
                     fluidRow(
                       column(1,offset = 1.8,
                              img(src = "imagen.png",height=100,width=300))
                     ),
                     br(),
                       
                       selectInput("año",label="Seleccione el año",width = 150,
                                   choices = c("2015","2016","2017"),
                                   selected = "2015"
                       ),
                       
                       selectInput("busqueda",label = "Filtrar por:",
                                   choices = c("Tipo de accidente","Barrio","Hora")),
                       
                       conditionalPanel("input.busqueda == 'Barrio'",
                                        uiOutput("Barrios")
                       ),
                       
                       conditionalPanel("input.busqueda == 'Tipo de accidente'",
                                        uiOutput("tipo")),
                       
                       conditionalPanel("input.busqueda == 'Hora'",
                                        sliderInput("hoursRange", "Rango de horas ",
                                                    min = 0, max = 24, value = c(10,11)),
                                        hr(),
                                        fluidRow(column(3, verbatimTextOutput("value")))
                       ),
                      hr(),
                      span("Esta aplicación te ayudará a conocer como es la accidentalidad en",
                                "Medellín y así, pensando en tu seguridad, logres encontrar la mejor",
                                "ubicación para tu nueva casa"
                               ,style = "color:gray")
                     
                     
                      
                     
                     ),
              column(8," ",
                     
                     tabsetPanel(
                       id="panelTab",
                       tabPanel("Mapas",
                                textOutput("año_seleccionado"),
                                leafletOutput("mapa")
                                ),
                       tabPanel("Mapas de calor",
                                leafletOutput("mapa_calor")
                                )
                                )
                     )
     
            
      )      
    ),tabPanel(title = "Gráficos Descriptivos",
               
               fluidRow(
                 column(3,"",
                        
                        titlePanel(h2("Gráficos Descriptivos",align = "center")),
                        
                        radioButtons("tipo_gra",label = "Seleccione el tipo de gráfico que desea ver:",
                                     choices = c("Torta","Barras")),
                        
                        conditionalPanel("input.tipo_gra == 'Barras'",
                        radioButtons("botones",label = "Selecciones el gráfico de su interés:",
                                     choices = c("DIA vs CLASE","DIA vs GRAVEDAD","DIA vs DISEÑO",
                                                   "CLASE vs DIA","CLASE vs GRAVEDAD","CLASE vs DISEÑO",
                                                   "GRAVEDAD vs DIA","GRAVEDAD vs CLASE","GRAVEDAD vs DISEÑO",
                                                   "DISEÑO vs DIA","DISEÑO vs CLASE","DISEÑO vs GRAVEDAD")
                                     )),
                        conditionalPanel("input.tipo_gra == 'Torta'",
                                         radioButtons("botones2",label = "Selecciones el gráfico de su interés:",
                                                      choices = c("ACCIDENTES POR DIA","ACCIDENTES POR CLASE",
                                                                  "ACCIDENTES POR GRAVEDAD","ACCIDENTES POR TIPO DE DISEÑO VIAL"
                                                                  )
                                         ))
                        
                           
                 ),
                 column(9,"",	
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        conditionalPanel("input.tipo_gra == 'Torta'",
                                         plotlyOutput("tortas")),
                        conditionalPanel("input.tipo_gra == 'Barras'",
                                         plotlyOutput("barras",width = "100%",height = "400px"))
                                        
                        
                
               )
               )
    ),navbarMenu("Conoce más sobre FYSH",
                 
          tabPanel("Video",
                   
                   
                   fluidRow(
                     column(4,""),
                     column(4,
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            h4("    https://youtu.be/V37xAMOgbLI"),
                            img(src = "video.png",width="100%")
                            ),
                     column(4,"")
                     
                     
                   )
                   
                  
                   
                   
                   ),
          tabPanel("Integrantes",
                   
                   fluidRow(
                     column(6,"",
                                br(),
                                br(),
                                h3("Diana Cardona Hernández"),
                                h4("dcardonah@unal.edu.co"),
                                h5("Estudiante de Estadística"),
                                br(),
                                br(),
                                h3("Tomás Pabón Palacio"),
                                h4("tpabonp@unal.edu.co"),
                                h5("Estudiante de Estadística"),
                                br(),
                                br(),
                                h3("Johan stiven castaño Herrera"),
                                h4("jscastanoh@unal.edu.co"),
                                h5("Estudiante de Estadística")


                                   
                            ),
                     column(6,"",br(),
                                 br(),
                                 br(),
                                 br(),
                                  h3("Santiago Areiza Tamayo"),
                                  h4("sareizat@unal.edu.co"),
                                  h5("Estudiante de Ingeniería de Sistemas e Informática"),
                                 br(),
                                 br(),
                            h3("Andrés David Tamayo Palomino"),
                            h4("adtamayop@unal.edu.co"),
                            h5("Estudiante de Ingeniería de Sistemas e Informática")
                            
                            )
                   
                   
                   
                   
                   
                   )
                )
    )   
  
)
)
)
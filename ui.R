shinyUI(
  navbarPage("RF de supervivencia en meningitis ",
             theme = shinythemes::shinytheme("flatly"),
             
             tabPanel("Modelo inicial",
                      tabsetPanel(
                        
                        tabPanel("modelo",
                      column(3,
                             br(),br(),br(),
                             selectInput("meur","DOORMAT en el modelo",choices = c("DOORMAT1",
                                                                                   "DOORMAT2","DOORMAT3",
                                                                                   "Espectro DOORMAT0",
                                                                                   "Todos"), 
                                         selected = "DOORMAT1"),
                             
                             br(),
                             helpText("Nota: La calidad del tratamiento antibiótico se recoge con las puntuaciones DOORMAT.",
                                      "Tras en diagnóstico rapido obtenemos la puntuación DOORMAT1,",
                                      " y tras el informe provisional se calcula el DOORMAT2.",
                                      "La puntuación DOORMAT3 corresponde al tratamiento tras el informe final.",
                                      " Por último, con el tratamiento empírico usamos el espectro DOORMAT 0")
                                          ),
                      
                        column(5,
                               
                               h4(textOutput("titulin")),
                               verbatimTextOutput("mod_1")
                        ),
                      br(),br(),br(),
                        column(3,uiOutput("textomod_D1")
                        )
                      ),
                      tabPanel("Importancia por permutación",
                               column(7,offset = 1,
                                      br(),
                                      h4(textOutput("titulin2")),
                                      plotOutput("gvimp")),
                               column(4,
                                      br(),
                                      h4(textOutput("titulin3")),
                                      tableOutput("tvimp")
                               )
                      ),
                      tabPanel("Importancia por profundidad mínima",
                               column(6,offset = 1,
                                      br(),
                                      h4(textOutput("titulin4")),
                                      plotOutput("gpm")),
                               column(5,
                                      br(),
                                      h4(textOutput("titulin5")),
                                      verbatimTextOutput("tpm")
                               )
                               ),
                      tabPanel("Comparativa importancias por los dos métodos",
                               column(7,offset = 2,
                                      br(),
                                      h4(textOutput("titulin6")),
                                      plotOutput("comp"))
                      )
                               
                      )
             ),
             tabPanel("Selección de modelos",
                      tabsetPanel(
                        tabPanel("Selección de modelos",
                                 sidebarPanel(
                                   uiOutput("var"),
                                   
                                 ),
                                 mainPanel(
                                   
                                    column(8,
                                   #actionButton("accion", "Mostrar modelo seleccionado"),
                                   #),
                                   
                                   verbatimTextOutput("modelomio"),
                                 
                                 
                                        textOutput("textomodelofin1"),
                                   textOutput("textomodelofin2"),
                                   textOutput("textomodelofin3")
                                        )
                        )),
                        tabPanel("Modelo con menor error",
                                 br(),
                                 helpText("Este caculo emplea alrededor de un minuto en realizarse"),
                                 column(7,offset = 1,
                                        br(),
                                        h4(textOutput("titulin7")),
                                        tableOutput("mm"))
                                 #,
                                 #column(5,
                                        #br(),
                                        #h4(textOutput("titulin8")),
                                        #verbatimTextOutput("mmm")
                                 #)
                                 )
                        
                      )
                      ),
             tabPanel("Modelo final y gráficos de efectos parciales",
                      sidebarPanel(
                        uiOutput("var1"),
                        br(),br(),
                        uiOutput("var2"),
                        br(),br(),
                        uiOutput("tipo"),
                        conditionalPanel(condition = "input.tipo=='Superviencia en un tiempo'",
                        sliderInput("tiempos","Seleccionar tiempo para estimar supervivencia",
                                    min=0,max=120,value = 120))
                        
                      ),
                      mainPanel(
                        column(10,
                        h4(textOutput("textgraf")),
                        plotOutput("parcial")
                        )
                      )
                      )
  ))
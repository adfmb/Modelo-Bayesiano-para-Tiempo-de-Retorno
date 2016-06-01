shinyUI(fluidPage(
  titlePanel("piggo"),
  sidebarLayout(position = "right",

                  sidebarPanel(
                    helpText("Informacion de referencia:
                            Los datos utilizados fueron generados por medio de simulaciones 
                            para proteger la naturaleza e integridad de los datos reales del Corte del sistema de piggo"),
                    
                    selectInput("clase_NumeroOperaciones",
                                label = "Clase de clientes de acuerdo a su nivel de volumen de operaciones manejado", 
                                choices = c("A", "B", "C","D","E"), selected = "B"),
                    
                    sliderInput("Proporcion",label="Proporcion de datos a entrenar",
                                min=.01,max=1,value=.02),
                    
                    sliderInput("iteraciones_jags",label="Iteraciones",
                                min=5000,max=50000,value=5000,step=100),
                    
                    sliderInput("CalentamientoJags",label="Calentamiento",
                                min=100,max=5000,value=1000,step=100),
                    
                    checkboxGroupInput("checkGroup", label = "Seleccionar Variables explicativas a considerar por cada cliente-fecha", 
                                       choices = c("Número de operaciones diarias"="Numero.de.operaciones.del.día", 
                                                   "Monto abonado diario"="Monto.....Abonado.en.el.dia",
                                                   "Día de quincena"="Dia.Quincena",
                                                   "Numero de operaciones previas"= "Suma.de.numero.de.operaciones.previas",
                                                   "Suma de montos abonados anteriores"="Suma.de.montos.....anteriores",
                                                   "Promedio de operaciones por los días previos"="Promedio.de.operaciones.por.día.previos",
                                                   "Promedio de monto por los días previos"="Promedio.de.monto.....por.dia.previos",
                                                   "Días transcurridos desde el inicio de su actividad"="Dias.transcurridos.desde.el.inicio.de.actividad",
                                                   "Total de días con actividad"="Total.de.dias.con.actividad",
                                                   "Total de operaciones del cliente"="Total.de.Operaciones.del.Cliente",
                                                   "Lunes"="Lunes", "Martes"="Martes",
                                                   "Miércoles"="Miercoles","Jueves"="Jueves",
                                                   "Viernes"="Viernes","Enero"="Enero",
                                                   "Febrero"="Febrero","Marzo"="Marzo",
                                                   "Abril"="Abril",
                                                   "Mayo"="Mayo",
                                                   "Junio"="Junio",
                                                   "Julio"="Julio",
                                                   "Agosto"="Agosto",
                                                   "Septiembre"="Septiembre",
                                                   "Octubre"="Octubre",
                                                   "Noviembre"="Noviembre",
                                                   "Diciembre"),
                                       selected = "Monto.....Abonado.en.el.dia"),

                    
                    submitButton("Refresh",icon("bar-chart-o")),
                    
                    downloadButton('downloadData1', 'Tabla Clientes_fechas_con_numero_dias_siguiente_abono_'),
                    
                    downloadButton('downloadData2', 'Pesos de las variables'),
                    
                    downloadButton('downloadData3', 'Datos de entrenamiento'),
                    
                    downloadButton('downloadData4', 'Datos de prueba')
                    
                    
                    
                    
                    ),
                  
      mainPanel(
        h3("Generación de modelo para el número de días en que un cliente hará su siguiente depósito"),
        tabsetPanel(type = "tabs",  
                           
                  tabPanel("Inferencial",plotOutput("plot1"),
                           verbatimTextOutput("sumario"),
                           #tableOutput("tabla_resumen"),
                           #tableOutput("tabla_resumen_2_1"),
                           tableOutput("tabla_resumen_2_2"),
                           tableOutput("comparativa"),
                           plotOutput("plot2"),
                           tableOutput("mat_conf"),
                           tableOutput("umbrales_1_2"),
                           #submitButton("Refresh",icon("bar-chart-o")),
                           dataTableOutput("tabla_final_casillas_ordenadas"))
                  )
              )
    )
  ))
  
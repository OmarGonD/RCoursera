library(shiny)
library(googleVis)
library(ggplot2)


OmarSesiones <- read.csv("D:\\RCoursera\\OmarApp\\OmarSesiones_1415.csv",
                         header = T)




OmarSesiones$Mes <- factor(OmarSesiones$Mes,
                                   levels = c("Enero", "Febrero",
                                              "Marzo", "Abril", "Mayo",
                                              "Junio", "Julio",
                                              "Agosto", "Setiembre","Octubre",
                                              "Noviembre",
                                              "Diciembre"), ordered =T)







#colnames(OmarSesiones)

#choose.files()



#rownames(OmarSesiones) <- OmarSesiones$Fuentes



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
    myData <- reactive({
    # Filter the data based on user selection month
    #date_seq <- seq(input$dates[1], input$dates[2], by = "day")
    
    OmarSesiones <- filter(OmarSesiones, Fuentes %in% input$fuentes)
    
    
    OmarSesiones <- filter(OmarSesiones, Mes %in% input$meses)
    
    
    OmarSesiones <- filter(OmarSesiones, Ano %in% input$años)
    
    return(OmarSesiones)
    
  })
    
    
    output$barrasfuentes <- renderPlot({
      library(ggplot2)
      #OmarSesiones <- filter(OmarSesiones, Fuentes %in% input$fuentes)
      ggplot(myData(), aes(Mes, Visitas, fill=Fuentes)) +
        stat_summary(fun.y=sum, geom="bar", position="stack") #+ 
        #stat_summary(fun.y=sum, geom="text", aes(label=..y..))
      
      
    })
  
   output$tablafuentes <- renderDataTable({
    library(ggplot2)
    library(dplyr)
    #OmarSesiones <- filter(OmarSesiones, Fuentes %in% input$fuentes)
    myData2 <- myData() %>%
               group_by(Mes, Fuentes) %>%
               summarise(Visitas = prettyNum(sum(Visitas),big.mark = ",",
                                             decimal.mark ="."))
    
    myData2
    
    
  })
    
  ### 
   
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
 
})

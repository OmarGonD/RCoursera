library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dashboard Adidas!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(condition = "input.tab1 == 'Adidas'",
      dateRangeInput("dates", "Fechas:",
                     start = "2014-01-01",
                     end   = "2015-03-15"),
      
      #img(src="adidas.jpg", height = 120, width = 180)),
      div(img(src="adidas.jpg", height = 120, width = 180),
          style="text-align: center;")),
      
      
      conditionalPanel(condition = "input.tab1 == 'Adidas Visitas'",
                       
                       
                       
      radioButtons('años', "Año", unique(as.character(OmarSesiones$Ano))
                   , selected = NULL, inline = T),
      
      
      
      div(img(src="adidas.jpg", height = 120, width = 180),
          style="text-align: center;"),
      
      
      br(),
      
      checkboxGroupInput('fuentes', 'Elige fuentes:',
                         unique(as.character(OmarSesiones$Fuentes)),
                         selected = unique(as.character(OmarSesiones$Fuentes)),
                         inline = F),
      
      
      
      
      
      checkboxGroupInput('meses', 'Elige meses:',
                         unique(as.character(OmarSesiones$Mes)),
                         selected = unique(as.character(OmarSesiones$Mes)))
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(id = "tab1",
      
        
      tabPanel("Adidas Visitas",
               br(),
               "Visitas por fuente",  
      plotOutput('barrasfuentes'),
      br(),
      dataTableOutput('tablafuentes')),
      
      tabPanel("Adidas",
               br(),
               "Por fecha exacta",
               br(),
               br(),
               
               h1("Dashboard modelo"),
               br(),
               br(),
               p("Este es un dashboard modelo para mostrar
                 para mostrar gráficos de Google Analytics"),
               p("Para contactar al creador, visite la web:", a("www.omargonzalesdiaz.com",
                                                          href = "www.omargonzalesdiaz.com")),
               br(),
               p(em("Omar Gonzales")),
               p("@Digital Cook House"))
               
      
      
      )
    )
  )
)
)

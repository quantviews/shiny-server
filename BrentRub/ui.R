
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(magrittr)
library(plotly)


shinyUI(fluidPage(

  # Application title
  titlePanel('Соотношение цен на нефть и курса рубля'),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('dateRange',
                      label = 'Введите дату: yyyy-mm-dd',
                      start = first_date, end = Sys.Date(),
                      language = 'ru'
                     ),
      dateInput('date_highlight',
                label = 'Введите дату для стрелки',
                value=Sys.Date(),format = "yyyy-mm-dd", language = 'ru'),
      checkboxInput("real_prices", "В постоянных ценах", FALSE)
    ),
    

    # Show ggvis plot  
    mainPanel(
       tabsetPanel(type = "tabs", 
                   tabPanel("Диаграмма",plotlyOutput("plot")),
                   tabPanel("Рублевая цена",plotlyOutput("plot2")),
                   tabPanel("Приросты",plotlyOutput("plot3")))
    ),
   fluid = FALSE
  )
))

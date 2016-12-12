
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(magrittr)
library(xts)
#library(ggplot2)


# # функция для отрисовки tooltips
all_values <- function(x) {
  if(is.null(x)) return(NULL)

  row <- buff[buff$id == x$id, ]
  row <- row[, c('USD', 'Brent', 'date')]
  row$date <- format(row$date, '%d-%b-%Y')
  row$USD <- round(row$USD,2)
  sprintf("<table width='100%%'>%s</table>",
          paste0("<tr><td style='text-align:left'>", names(row),
                 ":</td><td style='text-align:right'>", format(row), collapse="</td></tr>"))
}

# преобразовать объект xts в dataframe с сохранением индекса даты
XtstoDf <- function(ts, ...){ 
  df <- as.data.frame(ts)
  df$date <- time(ts)
  return(df)
}




shinyServer(function(input, output) {


  data <- reactive({
    # проверка, что первая дата меньше второй. НЕ РАботает 
    validate(
      need(input$dateRange[2] > input$dateRange[1], "end date is earlier than start date")
    )
    
     dateRange <- paste0(input$dateRange[1], '::', input$dateRange[2])
     #print(dateRange)
     buff <- XtstoDf(fx[dateRange])
     buff$year <- format(as.Date(buff$date), '%Y')
     buff$id <- seq_len(nrow(buff))
     return(buff)
    })
  
  data2 <- reactive({
    date_higlight <- input$date_highlight
    data2 <- data[buff$date == date_highlight,]
    return(data2)
  })

    
  
  
  #ggplot(buff, aes(x = Brent, y = USD))+geom_point()
    # R
    data %>%
      ggvis(~Brent, ~USD) %>%
      layer_model_predictions(model = "lm", formula = USD ~ log(Brent) ) %>%  
      layer_points(fill = ~factor(year), size :=20, key := ~id) %>% 
      layer_points(data = tail(buff,1), fill := "red", size:=40, key := ~id) %>%
      #layer_points(x= as.numeric(last(fx$Brent)), y=as.numeric(last(fx$USD)), size :=60, fill = 'red') %>%
      #hide_legend("fill") %>%  
      #add_axis("x", title = 'Brent, $/баррель') %>%
      #add_axis("y", title = 'RUB/USD') %>%
      add_tooltip(all_values, "hover") %>%
      bind_shiny("ggvis", "ggvis_ui")
  

})




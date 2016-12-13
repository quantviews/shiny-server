
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(MASS)
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
             need(input$dateRange[2] > input$dateRange[1], "end date is earlier than start date"))
    
     dateRange <- paste0(input$dateRange[1], '::', input$dateRange[2])
     #print(dateRange)
     buff <- XtstoDf(fx[dateRange])
     buff$year <- format(as.Date(buff$date), '%Y')
     buff$id <- seq_len(nrow(buff))
     return(buff)
    })
  
  data2 <- reactive({
               data2 <- buff[buff$date == as.Date(input$date_highlight),]
               return(data2)
           })
  data3 <- reactive({
     real_prices <- input$real_prices
     return(real_prices)
  })
  

    
  output$plot <- renderPlotly({
  
     buff <- data()
     last_point <- data2()
     real_prices <- data3()
     #last_point <- buff[nrow(buff),]
     x <- list(title = "Брент, $/баррель")
     y <- list(title = "RUB/USD")
      if(real_prices){
         buff2 <- buff[,c("USD_real",   "Brent_real","year", 'date')]
         names(buff2)[1:2] <- c('USD', "Brent")
      }else{
         buff2 <- buff[,c("USD",   "Brent","year", 'date')]
      }
     
     z <- lm(USD ~ Brent, data = buff2)
     
     
     p <- plot_ly(data = buff2, x = ~Brent, y = ~USD, type = 'scatter', 
                  color = ~as.factor(year),
                  text = ~paste("Дата: ", date, '$<br>Брент:', Brent, '$<br>RUB/USD:', round(1/USD,2))) %>% 
        add_trace(x = ~Brent, y = fitted(z), mode = "lines") %>%
        layout(
           xaxis = x,yaxis = y,
           annotations = list(x = last_point$Brent, y = last_point$USD, text = paste0(last_point$date), showarrow = T,ax = -20, ay = -60)
        )
     
     
     p
  })
  
  output$plot2 <- renderPlotly({
     buff <- data()
     real_prices <- data3()
     x <- list(title = NA)
     y <- list(title = "Рублевая цена нефти, руб./барр.")
     
     if(real_prices){
        buff2 <- buff[,c("USD_real",   "Brent_real","year", 'date')]
        names(buff2)[1:2] <- c('USD', "Brent")
     }else{
        buff2 <- buff[,c("USD",   "Brent","year", 'date')]
     }
     p2 <- plot_ly(x = ~date, y = ~(Brent/USD),  data = buff2, mode = 'lines') %>% 
        layout(xaxis = x,yaxis = y)
     p2
     
  })
  
  # график с приростами
  output$plot3 <- renderPlotly({
     buff <- data()
     buff <- buff[complete.cases(buff),]
     x1 <- mean(buff$dlog_Brent)
     y1 <- mean(buff$dlog_USD)
     z_dlog <- rlm(dlog_USD ~ dlog_Brent, data = buff)
     
     p3 <- plot_ly(data = buff, x = ~dlog_Brent, y = ~dlog_USD, type = 'scatter',text = ~paste("Дата: ", date)) %>%  
        add_trace(x = ~dlog_Brent, y = fitted(z_dlog), mode = "lines") %>%
        layout(
           showlegend = F,
           annotations = list(x = x1, y = y1, text = paste0("Beta =",round(z_dlog$coefficients[2],2)), showarrow = T,ax = -45, ay = -45)
           )
     
     p3
     
     
  })
})




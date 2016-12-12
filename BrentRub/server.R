
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

    
  output$plot <- renderPlotly({
  
  # #ggplot(buff, aes(x = Brent, y = USD))+geom_point()
  #   # R
  #   data %>%
  #     ggvis(~Brent, ~USD) %>%
  #     layer_model_predictions(model = "lm", formula = USD ~ log(Brent) ) %>%  
  #     layer_points(fill = ~factor(year), size :=20, key := ~id) %>% 
  #     layer_points(data = tail(buff,1), fill := "red", size:=40, key := ~id) %>%
  #     #layer_points(x= as.numeric(last(fx$Brent)), y=as.numeric(last(fx$USD)), size :=60, fill = 'red') %>%
  #     #hide_legend("fill") %>%  
  #     #add_axis("x", title = 'Brent, $/баррель') %>%
  #     #add_axis("y", title = 'RUB/USD') %>%
  #     add_tooltip(all_values, "hover") %>%
  #     bind_shiny("ggvis", "ggvis_ui")
  # 
     buff <- data()
     last_point <- data2()
     #last_point <- buff[nrow(buff),]
     
     z <- lm(USD ~ Brent, data = buff)
     z_dlog <- rlm(dlog_USD ~ dlog_Brent, data = buff)
     
     p <- plot_ly(data = buff, x = ~Brent, y = ~USD, type = 'scatter', 
                  color = ~as.factor(year),
                  text = ~paste("Дата: ", date, '$<br>Брент:', Brent, '$<br>RUB/USD:', round(1/USD,2))) %>% 
        add_trace(x = ~Brent, y = fitted(z), mode = "lines") %>%
        layout(
           annotations = list(x = last_point$Brent, y = last_point$USD, text = paste0(last_point$date), showarrow = T,ax = -20, ay = -60)
        )
     
     
     p
  })
  
  output$plot2 <- renderPlotly({
     buff <- data()
     x <- list(title = NA)
     y <- list(title = "Рублевая цена нефти, руб./барр.")
     
     p2 <- plot_ly(x = ~date, y = ~(Brent/USD),  data = buff, mode = 'lines') %>% 
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




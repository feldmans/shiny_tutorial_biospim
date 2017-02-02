library(shiny)
library(survival)
library(GGally)
library(ggplot2)
library(plotly)
data("flchain")
data("retinopathy")
data1 <- retinopathy
data2 <- flchain
data1$time <- data1$futime
data2$time <- data2$futime
data1$censor <- data1$status 
data2$censor <- data2$death


shinyServer(function(input, output) {
  
  output$selectdata <- renderUI({
    ans <- radioButtons ("selectdata", label = "explore data: ", choices=c("data1", "data2"), inline = F)
    return(ans)
  })
  
  output$selecttime <- renderUI({
    #   ans <- checkboxGroupInput("selectHEP", label = "Treatments for hepatitis:", choices = c("B", "C"), selected = c("B", "C"), inline = T)
    ans <- radioButtons("selecttime", label = "time scale:", choices = c("days", "years"), selected = "days", inline = T)
    return(ans)
  })
  
  output$survPlot <- renderPlotly({
    g <- NULL
    data <- get(input$selectdata)
    mod <- survfit(Surv(time,censor)~1, data, conf.int=.95)
    g <- ggsurv(mod) +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
      scale_x_continuous(breaks=seq(0,max(data$time),365.25), labels=0:(length(seq(0,max(data$time),365.25))-1)) +
      scale_y_continuous(labels=percent) +
      labs(x="Time, years")
        g <- ggplotly(g)
        g <- plotly_build(g)
        g$layout$margin$t = 100
    return(g)
  })
  
})

  
# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
#    
#   output$distPlot <- renderPlot({
#     
#     sym.surv <- survfit(Surv(futime,status)~1, data, conf.int=.95)
#     
#     ggsurv(sym.surv) +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
#       scale_x_continuous(breaks=seq(0,max(flchain$futime),365.25), labels=0:(length(seq(0,max(flchain$futime),365.25))-1)) +
#       scale_y_continuous(labels=percent) +
#       labs(x="Time, years")
#     
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2] 
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     
#   })
#   
# })

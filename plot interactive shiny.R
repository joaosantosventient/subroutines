

plot_interactive_shiny<-function(dados,...) {
  require(shiny)
  shinyApp(
    ui<-fluidPage(
      fluidRow(
        column(width=12,
               plotOutput("plot1",dblclick="plot1_dblclick",brush=brushOpts(id="plot1_brush",resetOnNew=T),height=900)
        )
      )
    ), 
    server<-function(input, output) {
      ranges <- reactiveValues(x = NULL, y = NULL)
      output$plot1<-renderPlot({
        plot(dados[,1],dados[,2],xlim=ranges$x,ylim=ranges$y,...)
        if (ncol(dados)>2) {
            for (i in 3:ncol(dados)) {
                points(dados[,1],dados[,i],col=i-1,...)
            }
        }
        legend("topright",legend=paste("Column",2:ncol(dados)),col=1:(ncol(dados)-1),lwd=2,bty="n")
      })
      observeEvent(input$plot1_dblclick, {
        brush<-input$plot1_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
    }
  )
}


# dados<-cbind(x=1:100,y=cos(1:100),z=sin(1:100))
# plot_interactive_shiny(dados,t="l",lwd=3)

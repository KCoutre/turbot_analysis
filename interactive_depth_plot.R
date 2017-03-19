
#turbot subset

f387<-subset(df2,TAG_NUM==387)
f5282<-subset(df2,TAG_NUM==406)
fish<-f387
head(f387)
###  online code to be modified:  (source:https://github.com/rstudio/shiny-examples/blob/master/105-plot-interaction-zoom/app.R)
library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(width = 4, class = "well",
           h4("Brush and double-click to zoom"),
           plotOutput("plot1", height = 300,
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )
           )
    ),
    column(width = 8, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 6,
                    plotOutput("plot2", height = 300,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 6,
                    plotOutput("plot3", height = 300)
             )
           )
    )
    
  )
)

server <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(fish, aes(Time_UTC, -Depth_M)) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(fish, aes(Time_UTC, -Depth_M)) +
      geom_point()
  })
  
  output$plot3 <- renderPlot({
    ggplot(fish, aes(Time_UTC, -Depth_M)) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
}


shinyApp(ui, server)

###############################################################################################################################

#### Trying again with unixtime
server2 <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(fish, aes(as.numeric(Time_UTC), -Depth_M, color=Temp_C)) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(fish, aes(as.numeric(Time_UTC), -Depth_M)) +
      geom_point()
  })
  
  output$plot3 <- renderPlot({
    ggplot(fish, aes(as.numeric(Time_UTC), -Depth_M)) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
}


shinyApp(ui, server2)


library(cetaceanbcg)
library(shiny)
library(tidyverse)

server <- function(input, output, session) {
  # Hyperparameters
  hyperparams <- reactive(list(
    filter_thr = c(input$filt_thr_lower, input$filt_thr_upper),
    filter_order = as.integer(input$filt_order),
    window = input$window,
    yaw_offset = input$yaw_offset * pi / 180
  ))

  # Data
  bw_data <- reactiveVal(update_results(list(
    filter_thr = filter_thr0,
    filter_order = filter_order0,
    window = window0,
    yaw_offset = yaw_offset0
  )))

  # I/O
  depth_click <- reactiveValues(x = NULL, y = NULL)
  dive_click <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$update,
               bw_data(update_results(hyperparams())))
  observeEvent(input$depth_click, {
    depth_click$x <- convert_clickx(input$depth_click$x)
    depth_click$y <- input$depth_click$y
    dive_click$x <- NULL
    dive_click$y <- NULL
  })
  observeEvent(input$dive_click, {
    dive_click$x <- convert_clickx(input$dive_click$x)
    dive_click$y <- input$dive_click$y
  })

  # Plots
  output$depth_plot <- renderPlot(
    ggplot(bw180905_53_10hz_sparse, aes(dt, depth)) +
      geom_line(size = 0.2) +
      annotate_click(depth_click$x, depth_click$y) +
      scale_x_datetime(date_labels = "%H:%M:%S") +
      scale_y_reverse() +
      theme_minimal()
  )
  # Why do I have to do bw_data()[2]$bw_10hz? No clue why bw_data()$bw_10hz
  # doesn't work
  output$dive_plot <- renderPlot(
    bw_dive_plot(bw_data()[2]$bw_10hz, depth_click, dive_click)
  )
  output$bcg_plot <- renderPlot(
    bw_bcg_plot(bw_data()[1]$bw_400hz, dive_click)
  )
}

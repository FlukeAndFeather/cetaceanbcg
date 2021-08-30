ui <- fillPage(

  fillRow(

    fillCol(
      h1("Tune BCG Hyperparameters"),
      numericInput("filt_thr_lower",
                   "Filter lower threshold",
                   value = filter_thr0[1],
                   min = 0, max = 50),
      numericInput("filt_thr_upper",
                   "Filter upper threshold",
                   value = filter_thr0[2],
                   min = 0, max = 50),
      numericInput("filt_order",
                   "Filter order",
                   value = filter_order0,
                   min = 1, max = 40,
                   step = 1),
      numericInput("window_size",
                   "Window size (seconds)",
                   value = window0,
                   min = 0.25, max = 10),
      numericInput("yaw_offset",
                   "Yaw offset (degrees)",
                   value = yaw_offset0,
                   min = -180, max = 180),
      actionButton("update",
                   "Update")
    ),

    fillCol(
      plotOutput("depth_plot", click = "depth_click", height = "100%"),
      plotOutput("dive_plot", click = "dive_click", height = "100%"),
      plotOutput("bcg_plot", height = "100%"),
      flex = c(1, 1, 2)
    ),

    flex = c(1, 3)
  ),

  padding = 10

)

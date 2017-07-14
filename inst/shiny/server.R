## Server.R

shinyServer(function(input, output) {
  
  
  #################################
  ## TAB 1: INPUT & PLOT
  #################################


  ##########################
  ## CREATE REACTIVES
  ##########################
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # get input data
  data <- reactive({

    input_file <- input$file

    if (is.null(input_file)){

      data <- NULL

    } else {
      ext <- tools::file_ext(input_file$name)

      file.rename(input_file$datapath,
                paste(input_file$datapath, ext, sep="."))

      data <- rxylib::read_xyData(file = paste(input_file$datapath, ext, sep = "."))

    }

  })
  
  x_axis <- reactive({

    if(is.null(input$x))
      return(1)
    else
      return(as.numeric(input$x))

  })

  y_axis <- reactive({

    if(is.null(input$y))
      return(2)
    else
      return(as.numeric(input$y))

  })

  blk_nr <- reactive({
    if(is.null(input$blocks))
      return(1)
    else
      return(as.numeric(input$blocks))

  })
  
  ## check doubleklick
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ##########################
  ## OUTPUT
  ##########################
  
  output$dataset_metadata <- shiny::renderDataTable({
    if(!is.null(data()))
      if(input$dataset_meta_button)
        if(nrow(data()$metadata) > 0)
          return(data()$metadata)
        else
          return()

  })

  output$block_metadata <- shiny::renderDataTable({
    if(!is.null(data()))
      if(input$block_meta_button)
        if(nrow(data()$dataset[[blk_nr()]]$metadata_block) > 0)
          return(data()$dataset[[blk_nr()]]$metadata_block)
    else
      return()

  })
  
  ## create dropdown list with n-elements (n = number of blocks)
  output$block_ui <-   renderUI({
    if (is.null(data())) { return() }

    selectInput("blocks",
                "Blocks:",
                choices = 1:length(data()$dataset)
    )
  })
  
  #create dropdown list with n-elements (n = number of columns in one block)
  output$column_ui <-   renderUI({
    if (is.null(data())) { return() }

    list(
      selectInput("x",
                 "X:",
                 choices = 1:ncol(data()$dataset[[blk_nr()]]$data_block),
                 selected = 1),
      selectInput("y",
                  "Y:",
                  choices = 1:ncol(data()$dataset[[blk_nr()]]$data_block),
                  selected = 2)
    ) ## end list
  })
  

  # plot output
  output$plot_fitting <- output$plot <- renderPlot({
    if(!is.null(data()))
      plot(x = data()$dataset[[blk_nr()]]$data_block[,x_axis()],
           y = data()$dataset[[blk_nr()]]$data_block[,y_axis()],
           xlim = ranges$x,
           ylim = ranges$y,
           xlab = "x [a.u.]",
           ylab = "y [a.u.]")
  })

  #################################
  ## TAB 2: FITTING PANEL
  #################################
  
  output$model_formula <- renderUI({
    if (is.null(input$model_type)) { return() }
    # Depending on input$model_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$model_type,
           "linear" = withMathJax(helpText("$$y = a \\cdot x + y_0$$")),
           "exp_dec" = withMathJax(helpText("$$y = a \\cdot \\exp\\left(-\\frac{x}{t}\\right)$$"))
    )
  })
  
  output$coef_guess_ui <- renderUI({
    if (is.null(input$model_type)) { return() }
    # Depending on input$model_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$model_type,
           "linear" = list(sliderInput("a", "a:", min = 0, max = 10, value = 1, step = 0.01),
                           sliderInput("y_0", withMathJax(helpText("$$y_0$$")), min = -50, max = 0, value = 0)),
           "exp_dec" = list(sliderInput("a", "a:", min = 0, max = 10, value = 1, step = 0.01),
                         sliderInput("t", "t:", min = 0, max = 200, value = 100))
    )
  })

})

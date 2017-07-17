## Server.R

shinyServer(function(input, output) {
  
  
  #################################
  ## TAB 1: INPUT & PLOT
  #################################


  ##########################
  ## CREATE REACTIVES
  ##########################
  
  ranges <- reactiveValues(x = NULL, y = NULL, 
                           x_temp = NULL, y_temp = NULL,
                           x_fitting = NULL, y_fitting = NULL, 
                           x_transformation = NULL, y_transformation = NULL)
  
  # get input data
  data <- reactive({

    input_file <- input$file

    if (is.null(input_file)){

      return(NULL)

    } else {
      ext <- tools::file_ext(input_file$name)

      file.rename(input_file$datapath,
                paste(input_file$datapath, ext, sep="."))

      return(rxylib::read_xyData(file = paste(input_file$datapath, ext, sep = ".")))

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
  
  observeEvent(input$plot_fitting_dblclick, {
    brush <- input$plot_fitting_brush
    if (!is.null(brush)) {
      ranges$x_fitting <- c(brush$xmin, brush$xmax)
      ranges$y_fitting <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x_fiting <- NULL
      ranges$y_fitting <- NULL
    }
  })
  
  observeEvent(input$plot_transformation_dblclick, {
    brush <- input$plot_transformation_brush
    if (!is.null(brush)) {
      ranges$x_transformation  <- c(brush$xmin, brush$xmax)
      ranges$y_transformation  <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x_transformation <- NULL
      ranges$y_transformation <- NULL
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
  output$plot_fitting <- renderPlot({
    if(!is.null(data())){
      
      col_names <- colnames(data()$dataset[[blk_nr()]]$data_block)
      x_lab <- col_names[x_axis()]
      y_lab <- col_names[y_axis()]
      
      plot(x = data()$dataset[[blk_nr()]]$data_block[,x_axis()],
           y = data()$dataset[[blk_nr()]]$data_block[,y_axis()],
           xlim = ranges$x_fitting,
           ylim = ranges$y_fitting,
           xlab = x_lab,
           ylab = y_lab
      )
      if(input$show_grid_input){grid()}
    }
  })
    
    
    
    
    
    output$plot <- renderPlot({
    if(!is.null(data())){
      
      col_names <- colnames(data()$dataset[[blk_nr()]]$data_block)
      x_lab <- col_names[x_axis()]
      y_lab <- col_names[y_axis()]
      
      plot(x = data()$dataset[[blk_nr()]]$data_block[,x_axis()],
           y = data()$dataset[[blk_nr()]]$data_block[,y_axis()],
           xlim = ranges$x,
           ylim = ranges$y,
           xlab = x_lab,
           ylab = y_lab
           )
      if(input$show_grid_input){grid()}
    }
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = function() { paste(input$file, " ",Sys.Date(),".csv",sep="") },
  #   content = function(file) {
  #     
  #     
  #     
  #     write.csv(myout()$dataframe1,file,row.names=F)
  #   }
  # )
  
  #################################
  ## TAB 2: TRANSFORMATION
  #################################
  
  output$plot_transformation <- renderPlot({
      
      if(!is.null(data())){
        
        col_names <- colnames(data()$dataset[[blk_nr()]]$data_block)
        x_lab <- col_names[x_axis()]
        y_lab <- col_names[y_axis()]
        x <- data()$dataset[[blk_nr()]]$data_block[,x_axis()]
        y <- data()$dataset[[blk_nr()]]$data_block[,y_axis()]
        
        if(input$execute_normalisation){
          y <- y/max(y) 
          # ranges$y_temp <- ranges$y_transformation
          # ranges$y_transformation <- c(0,1)
        } else {
          # ranges$y_transformation <- ranges$y_temp 
        }
        
        if(input$execute_inverse){
          y <- -y 
          # ranges$x <- NULL
          # ranges$y <- NULL
        }
        
        if(input$execute_logx & !input$execute_logy){
          log = "x"
        }
        else if(!input$execute_logx & input$execute_logy){
          log = "y"
        }
        else if(input$execute_logx & input$execute_logy){
          log = "xy"  
        } else {
          log = ""
        }
        
        if(input$execute_wl2energy){
          y <-   y * x^2/(4.13566733e-015 * 299792458e+09)
          x <- 4.13566733e-015 * 299792458e+09 / x
          x_lab = "Energy [eV]"
          y_lab <- "Intensity [a.u.]"
        }
        
        if(input$execute_energy2wl){
          
          x <-  4.13566733e-015 * 299792458e+09/x
          y <-   (y * 4.13566733e-015 * 299792458e+09)/(x^2)
          x_lab = "Wavelength [nm]"
          y_lab <- "Intensity [a.u.]"
        }
        
        plot(x = x,
             y = y,
             xlim = ranges$x_transformation,
             ylim = ranges$y_transformation,
             xlab = x_lab,
             ylab = y_lab,
             log = log
        )
        if(input$show_grid_transform){grid()}
      }
      
    }) ## end renderPlot
    
  #################################
  ## TAB 3: FITTING PANEL
  #################################
  
  fit <- eventReactive(input$fitButton, {
    # input$n
 
  model_func <- switch(input$model_type,
                       "exp_dec" = exp_dec_fit,
                       "linear" = linear_fit)
  
  model_coefs <- switch(input$model_type,
                        "linear" = list("a" = input$a, "y_0" = input$y_0, "Rd" = input$Rd),
                        "exp_dec" = list("a" = input$a, "t" = input$t))
  
  linear_fit <- function(model_coefs, newx){
    a <- model_coefs$a
    y_0 <- model_coefs$y_0
    out <- a * newx + y_0
    return(out)
  }
  
  newx <- seq(from=min(ranges$x), to=max(ranges$x), length.out = 100)
  
  guess <- model_func(model_coefs, newx)
  
  fit_model <- function(mod_form, start, dat){
    fit <- try(nls(mod_form, start=start, data=dat, 
                   nls.control(minFactor=1/100000, maxiter=500)), 
               silent=T)
  }
  
  }) ## end eventReactive
  
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

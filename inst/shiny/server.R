## Server.R

shinyServer(function(input, output, session) {
  
  
  #################################
  ## TAB 1: INPUT & PLOT
  #################################


  ##########################
  ## CREATE REACTIVES
  ##########################
  
  ranges <- reactiveValues(x = NULL, y = NULL, 
                           x_temp = NULL, y_temp = NULL,
                           x_fitting = NULL, y_fitting = NULL, 
                           x_transformation = NULL, y_transformation = NULL,
                           fit = NULL)
  
  plot <- reactiveValues(plot = NULL, 
                         fitting = NULL, 
                         guess = NULL,
                         transformation = NULL,
                         newx = NULL, 
                         fit = NULL,
                         df = NULL)
  
  df_reac <- reactiveValues(
    df_transformation = NULL)
  
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
      return(input$blocks)

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
    
    if(is.null(names(data()$dataset)) || names(data()$dataset) == ""){
      blk_name <- 1:length(data()$dataset)
    } else {
      blk_name <- names(data()$dataset)
    }

    selectInput("blocks",
                "Blocks:",
                choices = blk_name
    )
  })
  
  #create dropdown list with n-elements (n = number of columns in one block)
  output$column_ui <-   renderUI({
    if (is.null(data()) || is.null(blk_nr())) { return() }

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
  
  output$plot <- renderPlot({
    if(!is.null(data())){
      
      col_names <- colnames(data()$dataset[[blk_nr()]]$data_block)
      x_lab <- col_names[x_axis()]
      y_lab <- col_names[y_axis()]
      
      x <- data()$dataset[[blk_nr()]]$data_block[,x_axis()]
      y <- data()$dataset[[blk_nr()]]$data_block[,y_axis()]
      xlim <- ranges$x
      ylim <- ranges$y

      df <- data.frame(x = x, y = y)
      gg_plot <- ggplot(data = df , aes(x = x, y = y)) +
        geom_point() +
        xlab(x_lab) + 
        ylab(y_lab) 
      
      if(!is.null(ranges$x)){
        
        gg_plot <- gg_plot + xlim(ranges$x)
      }
      if(!is.null(ranges$y)){
        
        gg_plot <- gg_plot + ylim(ranges$y)
      }
      
      plot$plot <- gg_plot
      
      return(gg_plot)
    }
  })
  
  output$download_Data <- downloadHandler(
    filename = function() { 
      paste(input$file, "_", Sys.Date(), ".txt", sep="") 
      },
    content = function(file) {

      for(i in 1:length(data()$dataset)){
        if(i ==1) {
          write.table(data.frame("Exported by rxylibShiny", "\n"), file, col.names = FALSE, row.names = FALSE, quote = FALSE)
          if(input$download_Meta){
            write.table(data.frame("# Metadata", ""), file, col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
            write.table(data()$metadata, file, row.names = FALSE, quote = FALSE, append = TRUE)
            write.table(data.frame("","\n"), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          }
          
          if(is.null(names(data()$dataset)) || names(data()$dataset) == ""){
            write.table(data.frame("#BLOCK", i), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          } else {
            write.table(data.frame(names(data()$dataset)[i], ""), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          }
          
          write.table(data()$dataset[[i]]$data_block, file, row.names = FALSE, append = TRUE)
          write.table(data.frame("\n"), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
        } else {
          if(is.null(names(data()$dataset)) || names(data()$dataset) == ""){
            write.table(data.frame("#BLOCK", i), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          } else {
            write.table(data.frame(names(data()$dataset)[i], ""), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          }
          if(input$download_Meta){
            write.table(data()$dataset[[i]]$metadata_block, file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
            write.table(data.frame("","\n"), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          }
          write.table(data()$dataset[[i]]$data_block, file, row.names = FALSE, append = TRUE, quote = FALSE)
          write.table(data.frame("","\n"), file, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
          
        }
          
      } ## end for loop
    }
  ) ## end downloadHandler
  
  #################################
  ## TAB 2: TRANSFORMATION
  #################################
  
  output$plot_transformation <- output$plot_fitting <- renderPlot({
      
      if(!is.null(data())){
        
        col_names <- colnames(data()$dataset[[blk_nr()]]$data_block)
        x_lab <- col_names[x_axis()]
        y_lab <- col_names[y_axis()]
        x <- data()$dataset[[blk_nr()]]$data_block[,x_axis()]
        y <- data()$dataset[[blk_nr()]]$data_block[,y_axis()]
        
        
        switch(input$execute_normalisation,
               
          none = {},
               
          max = {
            y <- y/max(y) 
            y_lab <- "Normalised"
          },
        
          first = {
            y <- y/y[1]
            y_lab <- "Normalised"
          },
        
          last = {
            y <- y/y[length(y)]
            y_lab <- "Normalised"
        }) ## end switch
        
        
        if(input$execute_inverse){
          y <- -y 
        }
      
        if(input$execute_wl2energy){
          y <-   y * x^2/(4.13566733e-015 * 299792458e+09)
          x <- 4.13566733e-015 * 299792458e+09 / x
          x_lab <- "Energy [eV]"
          y_lab <- "Intensity [a.u.]"
        }
        
        if(input$execute_energy2wl){
          
          x <-  4.13566733e-015 * 299792458e+09/x
          y <-   (y * 4.13566733e-015 * 299792458e+09)/(x^2)
          x_lab <- "Wavelength [nm]"
          y_lab <- "Intensity [a.u.]"
        }
        
        if(input$execute_cumsum){
          y <- cumsum(y)
        }
        
        if(input$execute_zeroy){
          y <- vapply(y, FUN = function(Y) {max(0,Y)}, FUN.VALUE = 1)
        }
        

        

        ## check if logarithmic axis
        if(input$execute_logx & !input$execute_logy){
          x <- log(x)
        }
        else if(!input$execute_logx & input$execute_logy){
          y <- log(y)
        }
        else if(input$execute_logx & input$execute_logy){
          x <- log(x)
          y <- log(x)
        }
        
        ## basic plot
        df <- data.frame(x = x, y = y)
        
        ## save as reactive value for fitting panel
        df_reac$df_transformation <- df
        
        gg_transformation <- ggplot(data = df , aes(x = x, y = y)) +
          geom_point() +
          xlab(x_lab) + 
          ylab(y_lab) 
        
        
        if(!is.null(ranges$x_transformation)){
          
          gg_transformation <- gg_transformation + xlim(ranges$x_transformation)
        }
        if(!is.null(ranges$y_transformation)){
          
          gg_transformation <- gg_transformation + ylim(ranges$y_transformation)
        }
        
        plot$transformation <- gg_transformation
        
        return(gg_transformation)

      }
      
    }) ## end renderPlot

  #################################
  ## TAB 3: FITTING PANEL
  #################################
  
  ## Function definitons ##
  
    linear_fit <- function(model_coefs, newx){
      a <- model_coefs$a
      y_0 <- model_coefs$y_0
      out <- a * newx + y_0
      return(out)
    }
  
    quadratic_fit <- function(model_coefs, newx){
      a0 <- model_coefs$a0
      a1 <- model_coefs$a1
      a2 <- model_coefs$a2
      out <- a0 + a1 * newx + a2 * newx^2
      return(out)
    
    }
    
    cubic_fit <- function(model_coefs, newx){
      a0 <- model_coefs$a0
      a1 <- model_coefs$a1
      a2 <- model_coefs$a2
      a3 <- model_coefs$a3
      out <- a0 + a1 * newx + a2 * newx^2 + a3 * newx^3
      return(out)
      
    }
    
    exp_dec_fit <- function(model_coefs, newx){
      a <- model_coefs$a
      t <- model_coefs$t
      out <- a * exp(-newx/t)
      return(out)
    }
    
    double_exp_dec_fit <- function(model_coefs, newx){
      a <- model_coefs$a
      t <- model_coefs$t
      out <- a * (1 - exp(-newx/t)) + exp(-newx/t)
      return(out)
    }
 
    fit_model <- function(mod_form, start, dat){
      fit <- try(minpack.lm::nlsLM(formula = mod_form, data=dat, start=start,
                     control = list(minFactor=1/100000, maxiter=500)), 
                 silent=T)
    }
    
    ## plot output fitting
    model_func <- reactive({
     func <- switch(input$model_type,
                           "linear" = linear_fit,
                           "quadratic" = quadratic_fit,
                           "cubic" = cubic_fit,
                           "exp_dec" = exp_dec_fit,
                           "double_exp_dec" = double_exp_dec_fit)
     
     return(list(func = func))
    })

    
    model_coefs <- reactive({
        switch(input$model_type,
                            "linear" = list("a" = input$a, "y_0" = input$y_0),
                            "quadratic" = list("a0" = input$a0, "a1" = input$a1, "a2" = input$a2),
                            "cubic" = list("a0" = input$a0, "a1" = input$a1, "a2" = input$a2, "a3" = input$a3),
                            "exp_dec" = list("a" = input$a, "t" = input$t),
                            "double_exp_dec" = list("a" = input$a, "t" = input$t))
    })
    
    guess <- reactive({

      if(is.null(ranges$x_fitting)){
        ranges$x_fitting <- c(min(data()$dataset[[blk_nr()]]$data_block[,x_axis()]),
                      max(data()$dataset[[blk_nr()]]$data_block[,x_axis()]))
      }
      
      newx <- seq(from=min(ranges$x_fitting), to=max(ranges$x_fitting), length.out = 100)
      plot$newx <- newx
      
      guess <- model_func()$func(model_coefs(), plot$newx)
      
    })
      
    observeEvent(input$fitButton, {
      
      mod_form <- switch(input$model_type,
                           "linear" = formula(y~a*x+y_0),
                           "quadratic" = formula(y~a0 + a1*x + a2*x^2 ),
                           "cubic" = formula(y~a0 + a1*x + a2*x^2 + a3*x^3),
                           "exp_dec" = formula(y~a*exp(-x/t)),
                           "double_exp_dec" = formula(y ~ a*(1-exp(-x/t)) + exp(-x/t)))
        
        
        
      fit <- fit_model(mod_form, start=model_coefs(), dat = df_reac$df_transformation)
      plot$fit <- fit
        
      if(inherits(fit, "try-error")){ 
        outmsg <- paste0("The fit failed.<br>", 
                           "The error was: <code>", attr(fit, "condition")$message, "</code><br>")
        output$fit_print_caption <- renderText("")
        output$fit_print <- renderText(outmsg)
      } else {
        optim_coefs <- as.list(coefficients(fit))
        mod_pred <- model_func()$func(optim_coefs, plot$newx)
        outtab <- t(summary(fit)$coefficients[,1:2])
        output$fit_print <- renderTable({outtab}, rownames=TRUE)
      }
      
      
    }) ## end if fitButton
    
    output$plot_fitting <- renderPlot({
        
      if(!is.null(data())){
        
        if(length(plot$newx == guess())){
        
          df_guess <- data.frame(x = plot$newx, y = guess())
        
          if(input$seeGuess)
            plot$guess <- geom_line(data = df_guess, aes(x,y), colour = "red")
          else
            plot$guess <- NULL
        
          if(input$fitButton){
          
            if(inherits(plot$fit, "try-error")){
              plot$fitting <- NULL
            } else {
              plot$fitting <- geom_line(data = data.frame(x = df_reac$df_transformation$x, 
                                                          y = fitted(plot$fit)), 
                                        aes(x,y), 
                                        colour = "green")
            }

          }
          
          make_fit_plot(plot$plot, plot$guess, plot$transformation, plot$fitting)
      } ## end if(length(plot$newx == guess())){

    } else { ## end if !is.null(data())
        return(NULL)
    }
  }) ## end renderPlot()
    
    
    make_fit_plot <- function(plot_plot, plot_guess, plot_transformation, plot_fitting){
      
      if(is.null(plot_transformation)){
        return(plot_plot + plot_guess + plot_fitting)
      } else {
        return(plot_transformation + plot_guess + plot_fitting)
      }
      
    }

  output$model_formula <- renderUI({
    if (is.null(input$model_type)) { return() }
    # Depending on input$model_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$model_type,
           "linear" = withMathJax(helpText("$$y = a \\cdot x + y_0$$")),
           "quadratic" = withMathJax(helpText("$$y = a_0 + a_1 \\cdot x + a_2 \\cdot x^2$$")),
           "cubic" = withMathJax(helpText("$$y = a_0 + a_1 \\cdot x + a_2 \\cdot x^2 + a_3 \\cdot x^3$$")),
           "exp_dec" = withMathJax(helpText("$$y = a \\cdot \\exp\\left(-\\frac{x}{t}\\right)$$")),
           "double_exp_dec" = withMathJax(helpText("$$y = a \\cdot \\left(1 - \\exp\\left(-\\frac{x}{t}\\right)\\right)+\\exp\\left(-\\frac{x}{t}\\right)$$"))
    )
  }) ##end output$model_formula
  
  output$coef_guess_ui <- renderUI({
    if (is.null(input$model_type)) { return() }
    # Depending on input$model_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$model_type,
           "linear" = list(numericInput("a", withMathJax(helpText("$$a$$")), value = 1),
                           numericInput("y_0", withMathJax(helpText("$$y_0$$")), value = 0)),
           
           "quadratic" = list(numericInput("a0", withMathJax(helpText("$$a_0$$")), value = 1),
                              numericInput("a1", withMathJax(helpText("$$a_1$$")), value = 0),
                              numericInput("a2", withMathJax(helpText("$$a_2$$")), value = 0)),
           
           "cubic" = list(numericInput("a0", withMathJax(helpText("$$a_0$$")), value = 1),
                          numericInput("a1", withMathJax(helpText("$$a_1$$")), value = 0),
                          numericInput("a2", withMathJax(helpText("$$a_2$$")), value = 0),
                          numericInput("a3", withMathJax(helpText("$$a_3$$")), value = 0)),
           
           "exp_dec" = list(numericInput("a", withMathJax(helpText("$$a$$")), value = 1),
                            numericInput("t", withMathJax(helpText("$$t$$")), value = 100)),
           
           "double_exp_dec" = list(numericInput("a", withMathJax(helpText("$$a$$")), value = 1),
                                   numericInput("t", withMathJax(helpText("$$t$$")), value = 100))
    )
  }) ##end output$coef_gues_ui
  
})


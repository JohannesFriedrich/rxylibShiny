shinyUI(
  navbarPage("rxylib Shiny",
             
             #################################
             ## TAB 1: INPUT & PLOT
             #################################
             
             tabPanel("Data",
                      
                      fluidRow(
                      
                      column(4, wellPanel(
                        h3("Data input"),
                        fileInput(inputId = "file", 
                                  label = "Choose File"),
                        textInput(inputId = "URL", 
                                  label = "Submit URL", 
                                  placeholder = "Copy URL to supported file format here"),
                        h4("Paste data"),
                        rHandsontableOutput(outputId = "paste_table"),
                        radioButtons(inputId = "table",
                                     label = "Use table",
                                     choices = c("Yes" = TRUE,
                                                 "No" = FALSE),
                                     selected = FALSE,
                                     inline = TRUE),
                        tags$hr(),
                        checkboxInput(inputId = "dataset_meta_button", 
                                      label = "Show dataset meta data",
                                      value = TRUE),
                        checkboxInput(inputId = "block_meta_button", 
                                      label = "Show block meta data", 
                                      value = TRUE),
                        br(),
                        tags$hr(),
                        h3("Data output"),
                        downloadButton(outputId = "download_Data",
                                       label = "Download data as .csv"),
                        checkboxInput(inputId = "download_Meta", 
                                      label = "Add meta data to download", 
                                      value = FALSE),
                        uiOutput(outputId = "block_ui"),
                        uiOutput(outputId = "column_ui")
                      )),
                      column(8, wellPanel(
                        h2("Plot"),
                        plotOutput(
                          outputId = "plot",
                          dblclick = "plot_dblclick",
                          brush = brushOpts(
                            id = "plot_brush",
                            resetOnNew = TRUE)
                        ),
                        helpText("Choose zoom area with mouse and double click for zoom. Double klick again for default view."),
                        tabsetPanel(
                          tabPanel(title = "Dataset Metadata",
                                    shiny::dataTableOutput("dataset_metadata")),
                          tabPanel(title = "Block Metatdata",
                                    shiny::dataTableOutput("block_metadata"))
                        ) ##  end tabsetPanel
                      ))
                      ) # end fluidRow
             ), # end Tab DATA
             
             #################################
             ## TAB 2: TRANSFORMATION
             #################################
             
             tabPanel("Transform data",
                      
                      fluidRow(
                        
                        column(4, wellPanel(
                          h3("Transformations"),
                          radioButtons("execute_normalisation", 
                                             "Normalise to:", 
                                             choices = c("None" = "none",
                                                         "Max" = "max", 
                                                         "Last" = "last",
                                                         "First" = "first"), 
                                             selected = NULL,
                                             inline = TRUE),
                          checkboxInput("execute_inverse", "Inverse", FALSE),
                          checkboxInput("execute_logx", "log x", FALSE),
                          checkboxInput("execute_logy", "log y", FALSE),
                          checkboxInput("execute_wl2energy", " Wavelength \U02192 Energy", FALSE),
                          checkboxInput("execute_energy2wl", "Energy \U02192 Wavelength", FALSE),
                          checkboxInput("execute_cumsum", "Cumulative sum", FALSE),
                          checkboxInput("execute_zeroy", "Zero negative y", FALSE)
                          

                        )),
                        
                        column(8, wellPanel(
                          h2("Plot"),
                          plotOutput(outputId = "plot_transformation",
                                     dblclick = "plot_transformation_dblclick",
                                     brush = brushOpts(
                                       id = "plot_transformation_brush",
                                       resetOnNew = TRUE)),
                          helpText("Choose zoom area with mouse and double click for zoom. Double klick again for default view.")
                          ))
                        ) ## end fluid row
                      ), ## end tab TRANSFORMATION
             
             #################################
             ## TAB 3: FITTING PANEL
             #################################
             
             tabPanel("Fitting",
                      
                      fluidRow(
                        
                        column(4, wellPanel(
                          h4("Set fitting function"),
                          selectInput(inputId = "set_model_type", 
                                      label = "", 
                                      choices = c("Linear Model" = "linear",
                                                  "Quadratic" = "quadratic",
                                                  "Cubic" = "cubic",
                                                  "Exponential decay" = "exp_dec", 
                                                  "Double exponential decay"  = "double_exp_dec",
                                                  "Gaussian" = "gaussian"
                                        ),
                                      selected = "linear"),
                          uiOutput(outputId = "model_formula"),
                          br(),
                          h4("Set starting parameters"),
                          uiOutput(outputId = "coef_guess_ui"),
                          h4("Start fitting"),
                          br(),
                          actionButton(inputId = "start_fit", 
                                       label = "Start fit"),
                          actionButton(inputId = "remove_fit",
                                       label = "Remove fit"),
                          fluidRow(
                            column(4, checkboxInput(inputId = "see_guess", 
                                                    label = "See guess", 
                                                    value = FALSE)),
                            column(8, numericInput(inputId = "set_digits_fit",
                                                   label = "Digits for fit", 
                                                   value = 4, 
                                                   min = 1,
                                                   max = 12,
                                                   step = 1))
                          ), # end fluidRow

                          br(),
                          h4("Download"),
                          br(),
                          h5("Download fitted values"),
                          downloadButton(outputId = "download_Fit_table", 
                                         label = "Download table"),
                          br(),
                          h5("Download figure"),
                          fluidRow(
                            column(5,
                          selectInput(inputId = "set_output_format", 
                                      label = "Select output format",
                                      choices = c("pdf", "eps", "jpeg", "tiff", "png", "bmp", "svg"),
                                      selected = "pdf"))),
                          fluidRow(
                            column(5,
                          downloadButton(outputId = "download_Fit_plot", 
                                         label = "Download fitting plot")))
                          )), ## end column(4, ...)
                            
                      column(8, wellPanel(
                        h2("Plot"),
                      plotOutput(
                        outputId = "plot_fitting",
                        dblclick = "plot_fitting_dblclick",
                        brush = brushOpts(
                          id = "plot_fitting_brush",
                          resetOnNew = TRUE)
                      ),
                      htmlOutput(outputId = "fit_print")
                      )) #end column(8, ...),
                      ) # end fluidRow
                      
             ), # end Tab FITTING
             
             #################################
             ## TAB 3: ABOUT
             #################################
             
             tabPanel("About",
                      h5("Authors"),
                      p("Johannes Friedrich, University of Bayreuth (Germany)"),
                      h5("Contact"),
                      a("Johannes.Friedrich@uni-bayreuth.de"),
                      br(),
                      a(href = "https://johannesfriedrich.github.io/rxylibShiny", "Homepage"),
                      br(),
                      a(href = "https://github.com/JohannesFriedrich/rxylibShiny", "GitHub"),
                      br(),
                      h5("License"),
                      p("This program is free software: you can redistribute it and/or
             modify it under the terms of the GNU General Public License as
             published by the Free Software Foundation, either version 3 of
             the License, or any later version."),
                      p("This program is distributed in the hope that it will be useful
             , but WITHOUT ANY WARRANTY; without even the implied warranty
             of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the",
                        a("GNU General Public License"), "for more details.")
             )
             
             
  ) # end navbarPage
)# end shinyUI
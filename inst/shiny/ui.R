shinyUI(
  navbarPage("rxylib Shiny",
             
             #################################
             ## TAB 1: INPUT & PLOT
             #################################
             
             tabPanel("Data",
                      
                      fluidRow(
                      
                      column(4, wellPanel(
                        fileInput('file', 'Choose File'),
                        tags$hr(),
                        checkboxInput('dataset_meta_button', 'Show dataset meta data', TRUE),
                        checkboxInput('block_meta_button', 'Show block meta data', TRUE),
                        checkboxInput('show_grid_input', 'Show grid', FALSE),
                        uiOutput("block_ui"),
                        uiOutput("column_ui"),
                        downloadButton("downloadData", "Download")
                      )),
                      column(8, wellPanel(
                        h2("Plot"),
                        plotOutput(
                          "plot",
                          dblclick = "plot_dblclick",
                          brush = brushOpts(
                            id = "plot_brush",
                            resetOnNew = TRUE)
                        ),
                        helpText("Choose zoom area with mouse and double click for zoom. Double klick again for default view."),
                        tabsetPanel(
                          tabPanel("Dataset Metadata",
                                    shiny::dataTableOutput("dataset_metadata")),
                          tabPanel("Block Metatdata",
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
                          h2("Transformations"),
                          checkboxInput('execute_normalisation_max', 'Normalise to maximum', FALSE),
                          checkboxInput('execute_normalisation_first', 'Normalise to first data point', FALSE),
                          checkboxInput('execute_normalisation_last', 'Normalise to last data point', FALSE),
                          checkboxInput('execute_inverse', 'Inverse', FALSE),
                          checkboxInput('execute_logx', 'log x', FALSE),
                          checkboxInput('execute_logy', 'log y', FALSE),
                          checkboxInput('execute_wl2energy', ' Wavelength \U02192 Energy', FALSE),
                          checkboxInput('execute_energy2wl', 'Energy \U02192 Wavelength', FALSE),
                          checkboxInput('execute_cumsum', 'Cumulative sum', FALSE),
                          checkboxInput('execute_zeroy', 'Zero negative y', FALSE),
                          
                          checkboxInput('show_grid_transform', 'Show grid', FALSE)
                          
                        )),
                        
                        column(8, wellPanel(
                          h2("Plot"),
                          plotOutput("plot_transformation",
                                     dblclick = "plot_transformation_dblclick",
                                     brush = brushOpts(
                                       id = "plot_transformation_brush",
                                       resetOnNew = TRUE))
                          
                        ))
                        ) ## end fluid row
                      ), ## end tab TRANSFORMATION
             
             #################################
             ## TAB 3: FITTING PANEL
             #################################
             
             tabPanel("Fitting",
                      
                      fluidRow(
                        
                        column(4, wellPanel(
                          selectInput("model_type", "Select a model", 
                                      c("Exponential decay" = "exp_dec", 
                                        "Linear Model" = "linear")),
                          uiOutput("model_formula"),
                          br(),
                          uiOutput("coef_guess_ui"),
                          actionButton("fitButton", "Fit")
                      )),
                      column(8, wellPanel(
                      plotOutput(
                        "plot_fitting",
                        dblclick = "plot_fitting_dblclick",
                        brush = brushOpts(
                          id = "plot_fitting_brush",
                          resetOnNew = TRUE)
                      )
                      ))
                      ) # end fluidRow
                      
             ), # end Tab FITTING
             
             #################################
             ## TAB 3: ABOUT
             #################################
             
             tabPanel("About",
                      h5("Authors"),
                      p("Johannes Friedrich, University of Bayreuth (Germany)"),
                      h5("Contact"),
                      p("johannes.friedrich@uni-bayreuth.de"),
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
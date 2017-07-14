shinyUI(
  navbarPage("rxylib Shiny",
             tabPanel("Data",
                      
                      # fluidRow(
                      
                      column(4, wellPanel(
                        fileInput('file', 'Choose File'),
                        tags$hr(),
                        checkboxInput('dataset_meta_button', 'Show dataset meta data', TRUE),
                        checkboxInput('block_meta_button', 'Show block meta data', TRUE),
                        uiOutput("block_ui"),
                        uiOutput("column_ui")
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
                      # ) # end fluidRow
             ), # end Tab DATA
             
             tabPanel("Fitting",
                      
                      fluidRow(
                        
                        column(4, wellPanel(
                          selectInput("model_type", "Select a model", 
                                      c("Exponential decay" = "exp_dec", 
                                        "Linear Model" = "linear")),
                          uiOutput("model_formula"),
                          br(),
                          uiOutput("coef_guess_ui")
                      ))
                      # column(8, wellPanel(
                      # plotOutput(
                      #   # "plot_fitting",
                      #   # dblclick = "plot_fitting_dblclick",
                      #   # brush = brushOpts(
                      #   #   id = "plot_brush",
                      #   #   resetOnNew = TRUE)
                      # )
                      # ))
                      ) # end fluidRow
                      
             ), # end Tab FITTING
             
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
library(shiny)
library(ggplot2)  # for the diamonds dataset
require(rCharts)
options(shiny.maxRequestSize = 9*1024^2)
shinyUI(navbarPage("Diamonds Product Report", inverse = FALSE, collapsable = FALSE,
                   tabPanel("Summary",
                            fluidRow(
                                sidebarPanel(
                                    helpText("Tips:"),
                                    helpText("This is a summary re all diamonds information in our diamonds dataset. It has been visualized by a point plot on the right side. You are able to customize your own summary by selecting different variables you want to explore on x or y axis. You can also choose the categories of diamonds to reflect them on the diagram. Please make the adjustments below."),
                                    selectInput(inputId = "x",
                                                label = "Choose X",
                                                choices = c('clarity', 'depth', 'price', 'carat'),
                                                selected = "price"),
                                    selectInput(inputId = "y",
                                                label = "Choose Y",
                                                choices = c('clarity', 'depth', 'price', 'carat'),
                                                selected = "caret"),
                                    selectInput(inputId= "z",
                                                label = "Choose Category",
                                                choices = c('cut','color','clarity'),
                                                selected = 'cut')
                                ),
                                mainPanel(
                                    showOutput("myChart1", "polycharts")
                                )
                            ),
                            fluidRow(
                                p(em("Documentation:",a("Diamonds Product Report",href="Documentation.html"))),
                                p(em("Github Documentation:",a("Data-Science-Data-Products",href="https://github.com/ivanliu1989/Data-Science-Data-Products/blob/master/resume/documentation.md")))
#                                 sidebarPanel(
#                                     
#                                 ),
#                                 mainPanel(
# #                                    showOutput("myChart2", "Leaflet")
#                                 )
                            )
                   ),
                   tabPanel("Detailed Report",
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("Tips:"),
                                    helpText("This page allow you to view the details of diamonds based on different cut types. You can select the variables you want to include in the table. You can sort table by different variables and search record based on your preferences. You can also determine how many records you want to show on one page."),
                                    checkboxGroupInput('show_vars_tb1','Columns in Dongdamen to show:',
                                                        names(diamonds), selected = names(diamonds))       
                                ),
                                mainPanel(
                                    tabsetPanel(
                                        id='dataset',
                                        tabPanel('Fair Cut', dataTableOutput('mytable1')),
                                        tabPanel('Good Cut', dataTableOutput('mytable2')),
                                        tabPanel('Very Good Cut', dataTableOutput('mytable3')),
                                        tabPanel('Premium Cut', dataTableOutput('mytable4')),
                                        tabPanel('Ideal Cut', dataTableOutput('mytable5'))
                                    )
                                )
                            )),
                   navbarMenu("More",
                              tabPanel("Download Datasets",
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Tips:"),
                                               helpText("You can download the table for diamonds information here. Please select a type of diamonds that you are interested first and ensure the right table is the one you are willing to download. Then click the download button below. It will be a .csv format file on your drive."),
                                               selectInput("datasetD", "Choose a dataset:", 
                                                           choices = c("Fair Cut", "Good Cut", 
                                                                       "Very Good Cut", "Premium Cut", 
                                                                       "Ideal Cut", "All"), 'All'),
                                               downloadButton('downloadData', 'Download')
                                           ),
                                           mainPanel(
                                               tableOutput('table_download')
                                           )
                                       )),
                              tabPanel("Exchange Rate", 
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Tips:"),
                                               helpText("You can obtain the latest exchange rate regarding your selected currency from the right chart. Please select a currency type first. Information will be collected from yahoo finance."),
                                               textInput("symb", "Symbol", "AUD"),
                                               dateRangeInput("dates", "Date range", 
                                                              start = "2014-01-01", 
                                                              end = as.character(Sys.Date())),
                                               actionButton("get", "Get Exchange Rate"),
                                               br(),br(),
                                               checkboxInput("log", "Plot y axis on log scale", value = FALSE),
                                               checkboxInput("adjust", "Adjust prices for inflation", value = FALSE)
                                           ),
                                           mainPanel(plotOutput("plot"))
                                       )
                              )
                   )
)
)
shinyUI(pageWithSidebar(
      headerPanel('Countries Science and Technology Data Visualization'),
      sidebarPanel(
            selectInput("indicator", "Indicator:", c("Research and development expenditure (% of GDP)" = "Research and development expenditure (% of GDP)",
                                                     "Scientific and technical journal articles" = "Scientific and technical journal articles",
                                                     "Patent applications, nonresidents" = "Patent applications, nonresidents",
                                                     "Patent applications, residents" = "Patent applications, residents",
                                                     "Researchers in R&D (per million people)" = "Researchers in R&D (per million people)",
                                                     "High-technology exports (current US$)" = "High-technology exports (current US$)"),
                        selected = "Scientific and technical journal articles",
            ),
            checkboxGroupInput("country", "Country:", c("United Arab Emirates" = "United Arab Emirates",                                
                                                        "Argentina" = "Argentina",                                                                   
                                                        "Australia" = "Australia",                                           
                                                        "Austria" = "Austria",                                                                                    
                                                        "Bolivia" = "Bolivia",                                             
                                                        "Brazil" = "Brazil",                                              
                                                        "Canada" = "Canada",                      
                                                        "Switzerland" = "Switzerland",                                                                                      
                                                        "China" = "China",                                               
                                                        "Colombia" = "Colombia",                                                                                                                                   
                                                        "Germany" = "Germany",                                                                                     
                                                        "Spain" = "Spain",                                                                                         
                                                        "France" = "France",                                        
                                                        "United Kingdom" = "United Kingdom",                                                     
                                                        "India" = "India",                                                             
                                                        "Italy" = "Italy",                                               
                                                        "Japan" = "Japan",                                      
                                                        "Mexico" = "Mexico",
                                                        "North America" = "North America",               
                                                        "Netherlands" = "Netherlands",        
                                                        "Norway" = "Norway",            
                                                        "Portugal" = "Portugal",                           
                                                        "Russian Federation" = "Russian Federation",                                  
                                                        "Singapore" = "Singapore",                                                                  
                                                        "United States" = "United States",         
                                                        "World" = "World"),
                                                         selected = "Brazil")
            ),
            mainPanel("How is science and technology across different countries? 
                      Using the World Bank dataset (obtained at http://data.worldbank.org/topic/science-and-technology), 
                      we will try to take a look at some indicators related to science and technology over time. 
                      You can choose the countries that will be plotted  and the parameter 
                      you want to take a look as well on the left.",
                  h5(textOutput("ocountry")),
                  h5(textOutput("oindicator")),
                  h5(textOutput("oyear")),
                  showOutput("plot", "morris")
            )
                    
      )
)


# This is the user-interface definition of a Shiny web application.

library(shiny)

shinyUI(fluidPage(
  titlePanel("Tool for Assessing the Impact of OCV Use on Protection and Epidemic Risk"),
    sidebarLayout(
      sidebarPanel(
           h3("Required Input:"),
           wellPanel(
           numericInput("N",
                         label=helpText("Population Size"),
                        value=100000,
                        min=0
                        ),
           #uiOutput("N1in"),     
           numericInput("N1",
                        label=helpText("Number vaccinated with only 1-dose"),
                        value=5000,
                        min=0,
                        step=1),
           numericInput("N2",
                     label=helpText("Number vaccinated with 2-doses"),
                      value=44000,
                      min=0,
                      step=1)),
           #submitButton(text = "Submit"),          
           h3("Optional Input:"),
           wellPanel(                         
             numericInput("VE1",
                        label=helpText("1-dose Vaccine Efficacy [default based on point estimate from meta-analysis]"),
                        value=0.44,
                        min=0,
                        max=1),
           numericInput("VE2",
                        label=helpText("2-dose Vaccine Efficacy [default based on point estimate from meta-analysis]"),
                        value=0.73,
                        min=0,
                        max=1),
           numericInput("R.pes",
                        label=helpText("Reproductive Number [pessimistic]"),
                        value=2,
                        min=1                      
           ),
           numericInput("R.mod",
                        label=helpText("Reproductive Number [moderate]"),
                        value=1.5,
                        min=1),
           numericInput("R.opt",
                        label=helpText("Reproductive Number [optimistic]"),
                        value=1.1,
                        min=1),
           sliderInput("pct.uncer",
                        label=helpText("Margin of Error [+/- percent of estimate] \n This dictates the width of the colored regions and is a bit adhoc for now"),
                        value=0.3,
                        min=0,
                        max=1)          
           ),
           "Source code can be found ",
           a("here", href="https://github.com/HopkinsIDD/propvac"),
           br(),  
           "Brought to you by the",
           a(" Infectious Disease Dynamics Group",href="http://iddynamics.jhsph.edu/"),
           "at Johns Hopkins Bloomberg School of Public Health"
      ),
mainPanel(
  tabsetPanel(
    tabPanel("Direct and Indirect Protection",
             HTML("<h3>WARNING: This application is under development. Please consult the <a href=mailto:azman@jhu.edu?Subject=Cholera%20App>developer</a> before using this for decision making.</h3>"),
             "This tool estimates the proportion of the population directly protected from vaccine, the number of cases prevented (direct + indirect effects), and the final number expected to be infected for a given population size and vaccine coverage (required inputs). We have provided 3 scenarios (estimates of the transmission efficiency of cholera) that we believe could characterize the observed epidemic in Juba (in early June-2014). Feel free to change the scenario reproductive numbers to reflect your beliefs. The circles on the right are guides to help illustrate the likelihood of transmission should cholera be introduced to this population with red representing situations where onward transmission would be very likely and green representing situations where onward transmission is less likely.",
             plotOutput('ind.dir.plot'),
             h3("Considerations:"),
             HTML("<ul>
                <li>The estimates here are shown as fixed points but in reality there is considerable uncertainty. We are working hard to illustrate uncertainty in future versions but in the meantime be conservative when using these results and consult the developers if any questions arise. </li>
                <li>This represents a model of person to person transmission. If transmission is through the environment (e.g. transmission predominantly through water system), there will be little to no indirect protection.</li>
                <li>The final size represents the number infected (i.e., those who gain temporary immunity) and we do not expect these to represent clinical cases that would show up to a clinic. The attack rates should be scaled down by a factor representing your best guess for the ratio of severe disease to infections.</li>
                <li>By default we are assuming 44% vaccine effectiveness for a single dose, an average derived from only two studies who measured this as a secondary outcome and in both cases the confidence interval spanned null. To be conservative you may consider setting the 1-dose effectiveness (in optional input) to 0.</li>
                </ul>"),
             h3("Definitions:"),
             HTML("<ul>
                <li>Cases Prevented: This is the difference in attack rates between epidemics with and without vaccine</li>
                <li>Directly Protected: This is the vaccine efficacy weighted proportion of the population protected by vaccine</li>
                <li>Uninfected (Susceptible): This is the proportion of people who are unvaccinated who will ultimately not get infected in an epidemic</li>
                <li>Infected: This is the proportion of people who are unvaccinated who will ultimately get infected in an epidemic</li>
                </ul>")             
             ),
    tabPanel("Experimental",
             HTML("<h3>WARNING: This application is under development. Please consult the <a href=mailto:azman@jhu.edu?Subject=Cholera%20App>developer</a> before using this for decision making.</h3>"),
             plotOutput('my.plot'),
             "The interface between yellow and red in each corresponds to the estimate of 1-1/R:",
             tabPanel('Proportion Needed to Protect (point estimates)', tableOutput("table")),
             h5("Warning: The above output is based on basic epidemic theory and should only be used as a rough guide. These are based on the assumption that transmission is roughly person to person so if transmission occurs through contamination of water there will be no indirect protection and everyone should be vaccinated to be protected. \n")
  )
           )))))
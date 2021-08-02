library(shiny)
library(shinyBS)
library(shinycssloaders)

renderInputs <- function(prefix) {
  (
    wellPanel(
    tabsetPanel( 
      tabPanel("Disposition",
               sliderInput("n_dispersed", "The number of mobile missiles deployed for forward sites.", 0, 24, value = 4),
               bsTooltip(id = "n_dispersed", title = 'A standard mobile missile brigade has 12 launchers (pg. 9) and in 2010 China had two brigades (pg. 17). In day-to-day alert, 1 battalion per brigade (2 launchers) is dispered to a forward site (pg. 11). "In fully alerted status, all the mobile missiles are dispered to the forward sites" (pg. 12).', 
                         trigger = "hover"),
               sliderInput("n_dealerted", "The number of mobile missiles kept de-alerted in technical bases", 0, 24, value = 20),
               sliderInput("n_silo", "The number of silo-based missiles", 0, 40, value = 20)),
      tabPanel("Targeting",
               
               sliderInput("p_launch_pad_detected", "The probability a launch pad is located", 0, 1, value = c(0.59, 0.61)),
               
               sliderInput("p_forward_site_detected", "The probability a forward site is located", 0, 1, value = c(0.69, 0.71)),
               bsTooltip(id = "p_forward_site_detected", title = "Table A.1", 
                         trigger = "hover"),
               sliderInput("p_forward_site_killed", "The probability a forward site is destroyed if fired on", 0, 1, value = c(0.79, 0.81)),

               sliderInput("p_technical_site_detected", "The probability a technical site is located", 0, 1, value = c(0.79, 0.81)),
               sliderInput("p_technical_site_killed", "The  probability a technical site is destroyed if fired on", 0, 1, value = c(0.79, 0.81)),
               
               sliderInput("p_silo_detected", "The  probability a silo is located", 0.9, 1, value = c(0.998, 1), step = 0.01),
               sliderInput("p_silo_killed", "The  probability a silo is destroyed if fired on", 0.9, 1, value = c(0.994, 0.996)),    
               bsTooltip(id = "p_silo_killed", title = '"The W88 warheads of the Trident II D5 had the same counterforce capability against the DF-5 silos as shown in table 4" (pg. 21)', 
                         trigger = "hover"),
               sliderInput("p_base_detected", "The probability a warhead base is located", 0.9, 1, value = c(0.998, 0.999)),
               bsTooltip(id = "p_base_detected", title = "When de-alerted, warheads for ICBMs and mobile missiles are kept in a warhead base. Table A.1 gives a detection probability of 99.9%.", 
                         trigger = "hover"),
               sliderInput("p_base_killed", "The probability a warhead base is destroyed if fired on.", 0, 1, value = c(0.79, 0.81)),     
               bsTooltip(id = "p_base_killed", title = "When de-alerted, warheads for ICBMs and mobile missiles are kept in a warhead base, which is an undergrond facility. Table A.2 gives a kill probability of 80% for underground facilities in 2010.", 
                         trigger = "hover"),
               
               sliderInput("p_missile_survives_movement", "The probability a missile survives during movement", 0, 1, value = c(0.49, 0.51)),
               
               sliderInput("p_prep_detected", "The probability a missile is located during launch preparation", 0, 1, value = c(0.19, 0.21)),
               sliderInput("p_missile_reliable", "The reliability of Chinese missiles", 0, 1, value = c(0.79, 0.81))
      ),
      tabPanel("BMD",
               sliderInput("p_bmd_discrim", "The probability of target discrimination of BMD system", 0, 1, value = c(0.09,  0.11)),
               bsTooltip(id = "p_bmd_discrim", title = 'pg. 22', 
                         trigger = "hover"),
               sliderInput("p_bmd_reliability", "The reliability of a BMD vehicle", 0, 1, value = c(0.49, 0.51)),
               bsTooltip(id = "p_bmd_reliability", title = 'pg. 22', 
                         trigger = "hover")
    ),
    tabPanel("Advanced",
             checkboxInput("use_correlation", "Should detection probabilities be identical for all target types?",
                           value = FALSE),
             bsTooltip(id = "use_correlation", title = "If TRUE, detection probability will be drawn once per simulation and used for all targets", 
                       trigger = "hover"),
             sliderInput("correlated_detection", "The probability range for detecting all targets", 0, 1, value = c(0.50, 0.80))
    ))))}

fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          
          tags$h2("Replication of Wu Riqiang's (forthcoming) analysis of a US counterforce attack on China in 2010."),
          p(""),
          hr(),
          fluidRow(
            renderInputs("a")
          ),
          fluidRow(
            column(12,
                  withSpinner(plotOutput("distrib_prob", height="300px"), 8)
                   #withSpinner(textOutput("distrib_prob"), 8)
            )
          )
          
)

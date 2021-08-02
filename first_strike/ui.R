library(shiny)
library(shinyBS)
library(shinycssloaders)


renderInputs <- function(prefix) {
  wellPanel(
    #fluidRow(
    tabsetPanel( 
      #column(4,
      tabPanel("Instructions",
          br(),
          p("This calculator replicates and extends Lieber and Press' 2006 article in International Security that models a US nuclear first strike on Russia. It accompanies a working paper by [AUTHORS REMOVED FOR REVIEW]."),
          br(),
          p("The calculator uses the model described in Lieber and Press' article and begins with the default values they provide in the paper. It allows researchers to modify those values or assumptions in order to see how results vary with changes in inputs. Each tab includes parameters that can be modified. Two sets of each are provided and separate outputs are plotted below. This makes it easier to compare the effect of varying assumptions on outcomes."),
          br(),
          p("The calculator can also be used to study a US nuclear first strike on Chinese forces by using different values, specifically those given by Wu (2020). In 2010, the weapons that comprised the Chinese nuclear force were silo-based ICBMs (DF-5s) and road-mobile ICBMs (DF-31A). Wu assumes 100% reliability for US missiles because other missiles can be allocated if the first salvo fails (this is equivalent to Press's boost-phase failure compensation)."),
          br(),
          p("Numbers: 20 DF-5s, 24 DF-31As"),
          br(),
          p("Chinese silos (DF-5) have an overpressure strength of 2,000 PSI. Road missiles (DF-31A) are assumed to be soft targets with an LR of a 455kt warhead of 5,765m. Doing the lethal radius math, this works out to 4 x 10^-11 psi, which seems quite low."),
          br(),
          p("Chinese detection probabilities (all in 2010): 60% for launch sites, 70% for forward sites, 80% for technical sites, 50% for moving missiles, 20% for launch preperation. The Lieber and Press model, in contrast to Wu's, assumes that all warheads are mated with their launch vehicles, so the detection probability for \"warhead bases\" are omitted."),
          p("Assuming 3 W88s per Chinese target equals 132 warheads.")
      ),
      # 1 - (0.2 * 0.3 * 0.4) = 0.976 probability of locating at least one component
      # 
      tabPanel("Weapon Reliability",
               column(6,
             sliderInput("ohio_w88_reliability", "Reliability of the Ohio W88 vehicle", 
                         0.5, 1, value = 0.8, step = 0.01),
             sliderInput("ohio_w76_reliability", "Reliability of the Ohio W76 vehicle", 
                         0.5, 1, value = 0.8, step = 0.01),
             sliderInput("b2_b83_reliability", "Reliability of the B2 B83 vehicle", 
                         0.5, 1, value = 0.8, step = 0.01),
             sliderInput("b52_agm_reliability", "Reliability of the B52 AGM vehicle", 
                         0.5, 1, value = 0.8, step = 0.01),
             sliderInput("minuteman_w78_reliability", "Reliability of the Minuteman W78 vehicle", 
                         0.5, 1, value = 0.8, step = 0.01),
             sliderInput("minuteman_w62_reliability", "Reliability of the Minuteman W62 vehicle", 
                         0.5, 1, value = 0.8, step = 0.01)
             ),
             column(6,
                    sliderInput("ohio_w88_reliability2", "Reliability of the Ohio W88 vehicle", 
                                0.5, 1, value = 0.8, step = 0.01),
                    sliderInput("ohio_w76_reliability2", "Reliability of the Ohio W76 vehicle", 
                                0.5, 1, value = 0.8, step = 0.01),
                    sliderInput("b2_b83_reliability2", "Reliability of the B2 B83 vehicle", 
                                0.5, 1, value = 0.8, step = 0.01),
                    sliderInput("b52_agm_reliability2", "Reliability of the B52 AGM vehicle", 
                                0.5, 1, value = 0.8, step = 0.01),
                    sliderInput("minuteman_w78_reliability2", "Reliability of the Minuteman W78 vehicle", 
                                0.5, 1, value = 0.8, step = 0.01),
                    sliderInput("minuteman_w62_reliability2", "Reliability of the Minuteman W62 vehicle", 
                                0.5, 1, value = 0.8, step = 0.01)
             )),
      tabPanel("Weapon Accuracy",
               column(6,
      sliderInput("ohio_w88_cep", "Accuracy of the Ohio W88 warhead (CEP, meters)", 
                  20, 200, value = 90, step = 1),
      sliderInput("ohio_w76_cep", "Accuracy of the Ohio W76 warhead (CEP, meters)", 
                  20, 200, value = 90, step = 1),
      sliderInput("b2_b83_cep", "Accuracy of the B2 B83 warhead (CEP, meters)", 
                  100, 200, value = 150, step = 1),
      sliderInput("b52_agm_cep", "Accuracy of the B52 AGM warhead (CEP, meters)", 
                  5, 100, value = 30, step = 1),
      sliderInput("minuteman_w78_cep", "Accuracy of the Minuteman W78 warhead (CEP, meters)", 
                  5, 300, value = 120, step = 1),
      sliderInput("minuteman_w62_cep", "Accuracy of the Minuteman W62 warhead (CEP, meters)", 
                  5, 300, value = 180, step = 1)
    ),
    column(6,
    sliderInput("ohio_w88_cep2", "Accuracy of the Ohio W88 warhead (CEP, meters)", 
                20, 200, value = 90, step = 1),
    sliderInput("ohio_w76_cep2", "Accuracy of the Ohio W76 warhead (CEP, meters)", 
                20, 200, value = 90, step = 1),
    sliderInput("b2_b83_cep2", "Accuracy of the B2 B83 warhead (CEP, meters)", 
                100, 200, value = 150, step = 1),
    sliderInput("b52_agm_cep2", "Accuracy of the B52 AGM warhead (CEP, meters)", 
                5, 100, value = 30, step = 1),
    sliderInput("minuteman_w78_cep2", "Accuracy of the Minuteman W78 warhead (CEP, meters)", 
                5, 300, value = 120, step = 1),
    sliderInput("minuteman_w62_cep2", "Accuracy of the Minuteman W62 warhead (CEP, meters)", 
                5, 300, value = 180, step = 1)
    )),
    
    tabPanel("Attack Parameters",
             column(6,
      
      sliderInput("ohio_w88_num", "Number of SLBM W88s warheads used", 
                  0, 384, value = 384), 
      sliderInput("ohio_w76_num", "Number of SLBM W76s warheads used", 
                  0, 768, value = 768),
      sliderInput("b2_b83_num", "Number of B-2 B83s warheads used", 
                  0, 256, value = 256),
      sliderInput("b52_agm_num", "Number of air launched cruise missiles used", 
                  0, 840, value = 840),
      sliderInput("minuteman_w78_num", "Number of Minuteman W78s used", 
                  0, 713, value = 713),
      sliderInput("minuteman_w62_num", "Number of Minuteman W62s used", 
                  0, 300, value = 285)
             ),
      column(6,
             
             sliderInput("ohio_w88_num2", "Number of SLBM W88s warheads used", 
                         0, 384, value = 384), 
             sliderInput("ohio_w76_num2", "Number of SLBM W76s warheads used", 
                         0, 768, value = 768),
             sliderInput("b2_b83_num2", "Number of B-2 B83s warheads used", 
                         0, 256, value = 256),
             sliderInput("b52_agm_num2", "Number of air launched cruise missiles used", 
                         0, 840, value = 840),
             sliderInput("minuteman_w78_num2", "Number of Minuteman W78s used", 
                         0, 713, value = 713),
             sliderInput("minuteman_w62_num2", "Number of Minuteman W62s used", 
                         0, 300, value = 285)
      )
    ),
    tabPanel("Target Parameters",
             column(6,
                    sliderInput("silo_18_psi", "Overpressure SS-18 silos (85/258) can withstand", 
                                2000, 5000, value = 3000, step = 50),
                    sliderInput("silo_other_psi", "Overpressure SS-19 and SS-27 silos (173/258) can withstand", 
                                2000, 7000, value = 5000, step = 50),
                    sliderInput("mobile_missile_psi", "Overpressure a deployed mobile missle can withstand", 
                                0, 5, value = 1, step = 0.1),
                    sliderInput("silo_detection", "Probability that the attacker can locate a silo-based ICBM", 
                                0.9, 1, value = 1, step = 0.001),
                    sliderInput("mobile_base_detection", "Probability that the attacker can locate a road-mobile ICBM base", 
                                0, 1, value = 1, step = 0.01),
                    sliderInput("mobile_deployed_detection", "Probability that the attacker can locate a deployed road-mobile ICBM", 
                                0, 1, value = 0.70, step = 0.01)
             ),
             column(6,
                    sliderInput("silo_18_psi2", "Overpressure SS-18 silos (85/258) can withstand", 
                                2000, 5000, value = 3000, step = 50),
                    sliderInput("silo_other_psi2", "Overpressure SS-19 and SS-27 silos (173/258) can withstand", 
                                2000, 7000, value = 5000, step = 50),
                    sliderInput("mobile_missile_psi2", "Overpressure a deployed mobile missle can withstand", 
                                0, 5, value = 1, step = 0.1),
                    sliderInput("silo_detection2", "Probability that the attacker can locate a silo-based ICBM", 
                                0.9, 1, value = 1, step = 0.001),
                    sliderInput("mobile_base_detection2", "Probability that the attacker can locate a road-mobile ICBM base", 
                                0, 1, value = 1, step = 0.01),
                    sliderInput("mobile_deployed_detection2", "Probability that the attacker can locate a deployed road-mobile ICBM", 
                                0, 1, value = 0.70, step = 0.01)
             )),
    tabPanel("Target Numbers",
      p("Change numbers and types of targets (useful for calculating attack on non-Russia 2006 targets"),
      column(6,
      sliderInput("ss_18_num", "Number of SS-18 silos", 0, 200, value = 85),
      sliderInput("ss_other_num", "Number of other silos", 0, 200, value = 173),
      sliderInput("missile_mobile_num", "Number of mobile ICBM (garrisoned) aimpoints", 0, 100, value = 40),
      bsTooltip(id = "missile_mobile_num", title = "Press and Lieber assume mobile ICBMs are stationary in their garrisions. 291 mobile ICBMs thus present 40 aimpoints.", trigger = "hover"), 
      sliderInput("missile_mobile_deployed_num", "Number of mobile ICBM (deployed) aimpoints", 0, 300, value = 0),
      bsTooltip(id = "missile_mobile_deployed_num", title = "Press and Lieber assume mobile ICBMs are stationary in their garrisions. 291 mobile ICBMs thus present 40 aimpoints.", trigger = "hover"), 
      sliderInput("airfield_primary_num", "Number of primary airfields", 0, 50, value = 27),
      sliderInput("airfield_secondary_num", "Number of secondary airfields", 0, 100, value = 54),
      sliderInput("naval_primary_num", "Number of primary naval sites", 0, 50, value = 30),
      sliderInput("naval_secondary_num", "Number of secondary naval sites", 0, 200, value = 107),
      sliderInput("storage_num", "Number of storage/assembly sites", 0, 400, value = 283)
    ),
    column(6,
    sliderInput("ss_18_num2", "Number of SS-18 silos", 0, 200, value = 85),
    sliderInput("ss_other_num2", "Number of other silos", 0, 200, value = 173),
    sliderInput("missile_mobile_num2", "Number of mobile ICBM (garrisoned) aimpoints", 0, 100, value = 40),
    bsTooltip(id = "missile_mobile_num2", title = "Press and Lieber assume mobile ICBMs are stationary in their garrisions. 291 mobile ICBMs thus present 40 aimpoints.", trigger = "hover"), 
    sliderInput("missile_mobile_deployed_num2", "Number of mobile ICBM (deployed) aimpoints", 0, 300, value = 0),
    bsTooltip(id = "missile_mobile_deployed_num2", title = "Press and Lieber assume mobile ICBMs are stationary in their garrisions. 291 mobile ICBMs thus present 40 aimpoints.", trigger = "hover"), 
    sliderInput("airfield_primary_num2", "Number of primary airfields", 0, 50, value = 27),
    sliderInput("airfield_secondary_num2", "Number of secondary airfields", 0, 100, value = 54),
    sliderInput("naval_primary_num2", "Number of primary naval sites", 0, 50, value = 30),
    sliderInput("naval_secondary_num2", "Number of secondary naval sites", 0, 200, value = 107),
    sliderInput("storage_num2", "Number of storage/assembly sites", 0, 400, value = 283)
    ))
  ))
}

fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),

          tags$h2("Replication of Lieber and Press, \"The End of MAD?\" (2006)"),
          p(""),
          hr(),
          fluidRow(
            renderInputs("a")
          ),
          fluidRow(
            column(6,
              withSpinner(plotOutput("attack_plot1", height="1000px"), 8)
              ),
            column(6,
                   withSpinner(plotOutput("attack_plot2", height="1000px"), 8)
            )
          )

)
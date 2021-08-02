library(shiny)
library(shinyBS)
library(shinycssloaders)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
      sliderInput("initial_front_length", "Length of the front line that NATO must defend (km)", 700, 800, value = 750),
      bsTooltip(id = "initial_front_length", title = "Defined as point estimate of 750 in Appendix 3, pg. 240. This factor affects how many NATO divisions must be tied up in defending the non-breakthrough sectors. A longer front means more defensive divisions are required.", 
           trigger = "hover"), 
 
      sliderInput("breakthrough_width", "Width of each attempted breakthrough", 40, 60, value = 50),
      bsTooltip(id = "breakthrough_width", title = "Defined as point estimate of 50 km in Appendix 3, pg. 240.", 
           trigger = "hover"), 
                
      sliderInput("breakthru_num", "Number of Pact breakthrough efforts", 1, 5, value = 3),
      bsTooltip(id = "breakthru_num", title = "Defined as point estimate of 50 km in Appendix 3, pg. 240. Posen observes (pg. 70) that more than 3 breakthrough efforts is not advantageous to the Soviet side because of the forces required.", 
           trigger = "hover"),  
 
      sliderInput("offensive_ade_sector", "Width of the front occupied by a Pact ADE at breakthrough effort", 
                  10, 60, value = c(10, 25), step= 1),
    bsTooltip(id = "offensive_ade_sector", title = "The higher the number, the fewer the ADEs in the breakthrough. Posen gives a value of 12.5km (pg. 116)", 
              trigger = "hover"), 
      sliderInput("offensive_non_breakthru", "Width of flank-defending Pact ADEs (km)", 10, 60, value = c(22, 33)),
    bsTooltip(id = "offensive_non_breakthru", title = 'This represents Pact forces defending non-breakthrough areas against NATO counterattack. "[B]etween 0.75 and 1.1 ADEs per 25 km was deemd an acceptible defensive force-to-space ratio by the Soviets" (pg. 108). Those values are set as the default here. Page 116, however, indicates a point estimate of 25km/ADE.', 
           trigger = "hover"), 
      sliderInput("defense_ade_sector", "Width occupied by a NATO ADE across the border", 10, 60, value = c(25, 50), 
                  step = 1),
      bsTooltip(id = "defense_ade_sector", title = 'There is great uncertainty about this value. Estimates range from 15-60 km per ADE, but 25-50 is a credible range. See 107-109. This value is fixed across the front, in both breakthrough and non-breakthrough areas. Page 116 indicates that a point estimate of 25km/ADE is used, but 108-109 suggests a NATO-favorable assumption is 0.66 ADES per 25 km, thus 37.8 km/ADE.', 
                trigger = "hover"), 
      sliderInput("pact_attrition_rate", "Rate of attrition per day by Pact", 0, 10, value = c(1.2, 7.5),
                  step = 0.1),
      bsTooltip(id = "pact_attrition_rate", title = "Table 3.6 (pg. 118) reports various values for this. The tool encompasses the complete range. A NATO-favorable case is 1.2-1.7% losses for the Pact daily, along with daily NATO losses of 0.5-0.6%. A Pact-favorable case sees higher losses for each side: 3.3% losses for the Pact, and 2.7% for NATO.", 
           trigger = "hover"), 
      # losses by pact or caused by pact?
      sliderInput("pact_advance_rate", "Daily rate of Pact advance (km)", 1, 50, value = c(2, 5)),
      bsTooltip(id = "pact_advance_rate", title = "While Soviet doctrine called for advance rates of 100km/day, in the past, 15-20 km/day have only been possible against disorganized, retreating enemies (pg. 119). The Soviet-favorable case assumes 5km/day (pg. 121) and the NATO-favorable case assumes 2km/day (123).", 
           trigger = "hover"), 
      sliderInput("nato_multiplier", "NATO C3 multiplier", 1, 2, value = c(1, 1.5), step = 0.1),
 bsTooltip(id = "nato_multiplier", title = "NATO units have much larger command and logistics components (an additional 30%-100% personnel) than their Pact counterparts. Posen argues (pg. 95f) that this should make NATO units more effective, and suggests a multiplier of 1.5. Unfavorable scenarios assume no extra credit (multiplier=1).", 
           trigger = "hover"),
      sliderInput("exchange_rate", "Pact to NATO exchange rate", 1, 4, value = c(1.5, 2), step = 0.1),
      bsTooltip(id = "exchange_rate", title = "The Soviet-favorable case assumes 1.5 lost Soviet divisions for each destroyed NATO division. A NATO-favorable case assumes 2:1 (pg. 119). Table 3.7 (pg. 120) finds historical values ranging from 2:1 through 4:1 or 6:1", 
           trigger = "hover")
      ),
 column(6,
        sliderInput("pact_d", "Day for the Pact to start if forces available", 0, 30, value = 14),
        bsTooltip(id = "pact_d", title = "Longer delays more favorable to NATO.", 
                  trigger = "hover"),
        sliderInput("pact_ac", "Number of Pact CAS aircraft", 1000, 1750, value = c(1200, 1250)),
        bsTooltip(id = "pact_ac", title = 'Point estimate of 1225 aircraft: 420 fixed wing, 805 helicopter (Table 3.3, pg. 104). An additional 525 are estimated to be available as "reserve" forces, but are excluded from the analysis (fn 68, page 104)', trigger = "hover"),
        sliderInput("pact_air_attr_rate", "Pact attrition suffered per sortie", 0, 1, value = c(0.04, 0.06), step = 0.01), 
        bsTooltip(id = "pact_air_attr_rate", title = '"The 5 percent attrition per sortie assumed here is high by historical standards for wester air forces." (pg. 104)', trigger = "hover"),
        sliderInput("pact_k", "Pact kills per sortie", 0, 2, value = c(0.20, 0.50), step = 0.05), # pg 105
        bsTooltip(id = "pact_k", title = 'Historical variance with same mean as Posen: "One-quarter to one-half a vehicle killed per sortie is in the historical range of performance. The west is assigned a higher kill rate primarily on the grounds of superior weaponry." (pg. 104-105)',
                  trigger = "hover"),
        sliderInput("pact_sr", "Pact CAS sorties rate", 0, 4, value = 1, step = 0.05),
        bsTooltip(id = "pact_sr", title = "Sorties per aircraft per day.", 
                  trigger = "hover"),
        sliderInput("nato_ac", "NATO helicopters and aircraft", 1500, 2460, value = c(1600, 1660)),
        bsTooltip(id = "nato_ac", title = 'Point estimate of 1630 aircraft: 660 fixed wing, 970 helicopter (Table 3.3, pg. 104). An additional 830 are estimated to be available as "reserve" forces, but are excluded from the analysis (fn 68, page 104)', trigger = "hover"),
        sliderInput("nato_air_attr_rate", "NATO attrition per sortie", 0, 1, value = c(0.04, 0.06), step = 0.01), # pg 105
        bsTooltip(id = "nato_air_attr_rate", title = "Assumed a point estimate of 0.05, same as Pact forces. See pg. 105 for a justification of this assumption.", trigger = "hover"),
        
        sliderInput("nato_k", "NATO kills per sortie", 0, 2, value = c(0.35, 0.65), step = 0.01),
        bsTooltip(id = "nato_k", title = "Historical variance with same mean as Posen: \"One-quarter to one-half a vehicle killed per sortie is in the historical range of performance. The west is assigned a higher kill rate primarily on the grounds of superior weaponry.\" (pg. 104-105). The table on pg. 105 assigns a point estimate of 0.5.", trigger = "hover"),
        sliderInput("nato_sr", "NATO sortie rate per day", 0, 3, value = 2, step = 0.05),
        sliderInput("n_sims", "How many simulations to run?", 1, 100, value = 50, step = 1)
      )
 )
# p(actionButton(paste0(prefix, "_", "recalc"),
#                "Re-run simulation", icon("random")
#                )
#    )
  )
}

fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),

          tags$h2("The European Conventional Balance in the 1980s"),
          p("A replication and extension of Posen's (1991) campaign analysis"),
          hr(),
          
          #fluidRow(
          #  column(6, tags$h3("Scenario A")),
          #  column(6, tags$h3("Scenario B"))
          #),
          fluidRow(
            renderInputs("a")
            #column(6, renderInputs("a")),
            #column(6, renderInputs("b"))
          ),
          fluidRow(
            withSpinner(plotOutput("force_plot",height="1000px"), 8)
          )
          
)
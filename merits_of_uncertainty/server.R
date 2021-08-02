library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  reps <- 2000

  surviving <- reactive({
  
  ### Sample all inputs
    if (input$use_correlation == FALSE){
  p_launch_pad_detected <- runif(reps, min = input$p_launch_pad_detected[1], max = input$p_launch_pad_detected[2])
  p_forward_site_detected <- runif(reps, min = input$p_forward_site_detected[1], max = input$p_forward_site_detected[2])
  p_technical_site_detected <- runif(reps, min = input$p_technical_site_detected[1], max = input$p_technical_site_detected[2])
  p_silo_detected <- runif(reps, min = input$p_silo_detected[1], max = input$p_silo_detected[2])
  p_base_detected <- runif(reps, min = input$p_base_detected[1], max = input$p_base_detected[2])
  p_prep_detected <- runif(reps, min = input$p_prep_detected[1], max = input$p_prep_detected[2])
    } else if(input$use_correlation == TRUE){
       detection <-  runif(reps, min = input$correlated_detection[1], max = input$correlated_detection[2])
       print("detection_prob:")
       print(summary(detection))
       p_launch_pad_detected <- detection
       p_forward_site_detected <- detection
       p_technical_site_detected <- detection
       p_prep_detected <- detection
       # keep silos and bases at their original values
       p_silo_detected <- runif(reps, min = input$p_silo_detected[1], max = input$p_silo_detected[2])
       p_base_detected <- runif(reps, min = input$p_base_detected[1], max = input$p_base_detected[2])
    }
  
  
  # if a pad is detected, it is destroyed. Three pads per launcher.
  p_one_pad_survives <- 1 - p_launch_pad_detected^3 
    
  p_forward_site_killed <- runif(reps, min = input$p_forward_site_detected[1], 
                                       max = input$p_forward_site_detected[2])
  
  p_technical_site_killed <- runif(reps, min = input$p_technical_site_killed[1], 
                                         max = input$p_technical_site_killed[2])
  
  p_silo_killed <- runif(reps, min = input$p_silo_killed[1], 
                               max = input$p_silo_killed[2])
  
  p_base_killed <- runif(reps, min = input$p_base_killed[1], 
                               max = input$p_base_killed[2])
  
  p_missile_survives_movement <- runif(reps, min = input$p_missile_survives_movement[1], 
                                             max = input$p_missile_survives_movement[2])
  p_missile_reliable <- runif(reps, min = input$p_missile_reliable[1], max = input$p_missile_reliable[2])
  
  p_bmd_discrim <- runif(reps, min = input$p_bmd_discrim[1], max = input$p_bmd_discrim[2])
  p_bmd_reliability <- runif(reps, min = input$p_bmd_reliability[1], max = input$p_bmd_reliability[2])
  
  # How many technical sites are there?  Figure 1 shows a missile brigade having one technical site per bridage,
  # and in 2010 there were two mobile missile brigades
  # (but pg. 9 says PLARF is organized into "six missile bases.")
  n_tech_sites <- 2
  ###### END INPUTS ######
  
  # intercept probabilities
  # The 4th power is because 4 intercepters are launched at each warhead
  p_survives_intercept <- (1 - p_bmd_discrim*p_bmd_reliability)^4
  
  ## probability of retaliation from forward sites
  p_forward_site_survives <- 1 - p_forward_site_detected * p_forward_site_killed
  p_missile_survives_prep <- (1 - p_prep_detected) # assume it's destroyed if detected
  
  #  "p_tech_site_retaliation" is the same as "p_disperse" in Appendix A
  p_tech_site_retaliation <-  p_forward_site_survives * p_one_pad_survives * p_missile_survives_movement * 
    p_missile_survives_prep * p_missile_reliable * p_survives_intercept
  
  ## probability of a missile surviving in a technical base 
  # (no idea where the 1.2 comes from)
  p_dealert <- p_one_pad_survives * 1.2*p_missile_survives_movement * p_missile_survives_prep * p_missile_reliable * 
    p_survives_intercept
  #print(head(p_dealert))
  
  ## probability of successful retaliation if a technical base survives
  p_tech_base_survives <- (1 - p_technical_site_detected*p_technical_site_killed)
  p_tech <- p_tech_base_survives * (1 - (1 - p_dealert)^(input$n_dealerted/n_tech_sites)) # de-altered missiles per tech site
  #print(paste0("p_tech ", head(p_tech)))
  
  ## Silo missile survival
  p_silo_survives <- (1 - p_silo_detected*p_silo_killed)
  p_silo_missile <- p_silo_survives * p_missile_reliable * p_survives_intercept
  #print(paste0("p_silo_missile ", head(p_silo_missile)))
  
  
  ## The probability a warhead base survives
  p_base_survives <- (1 - p_base_detected * p_base_killed)
  ## The probability of retaliation from silos AND technical sites
  p_warhead <- p_base_survives * (1 - (1 - p_silo_missile)^input$n_silo * (1 - p_tech)^n_tech_sites)
  # WEIRD: this doesn't seem to allow warheads to be moved into alerted silos: silo retaliation always
  # depends on warhead base survival. It ends up being a moot point because the silos have very low
  # survivability, but it's strange...
  
  
  # overall probability of retaliation
  p_retaliation <- 1 - (1 - p_warhead) * (1 - p_tech_site_retaliation)^input$n_dispersed
  #print(head(p_retaliation))
  #print(summary(p_retaliation))
  #print(paste0("p_retaliation ", head(p_retaliation)))
  
  
  if (input$use_correlation == FALSE){
    st <- "Using independent draws for each target type"
  } else if (input$use_correlation == TRUE){
   st <- "Using perfectly correlated detection for each simulation" 
  }
  print(mean(p_retaliation))
  retal_df <- data.frame(p = p_retaliation)
  print(mean(retal_df$p))
  #save(retal_df, file = "wu_probabilities.RData")
  p <- ggplot(retal_df, aes(x = p)) +
    geom_histogram(bins = 250) +
    scale_x_continuous(limits = c(-0.03, 1.03), minor_breaks = seq(0, 1, 0.1)) +
    labs(x = "Probability of at least one warhead surviving",
         y = "Density",
         subtitle = st)
  #ggsave(p, file = "wu_calculator_output.pdf", width = 5, height = 3)
  
  ##### Plot inputs ####
  input_df <- data.frame(lower = c(input$p_launch_pad_detected[1],     
                                   input$p_forward_site_detected[1],   
                                   input$p_technical_site_detected[1], 
                                   input$p_silo_detected[1],           
                                   input$p_base_detected[1],           
                                   input$p_prep_detected[1]),      
                          upper = c(input$p_launch_pad_detected[2],    
                                   input$p_forward_site_detected[2],  
                                   input$p_technical_site_detected[2], 
                                   input$p_silo_detected[2],
                                   input$p_base_detected[2],
                                   input$p_prep_detected[2]),
                         launch_type = c("Launch Pad", 
                                         "Forward Site",
                                         "Technical Site",
                                         "Silo", 
                                         "Warhead Base", 
                                         "Preperation"))
  
  input_plot <- ggplot(input_df, aes(xmin = lower, xmax = upper, y = launch_type)) +
    geom_errorbarh(aes(height=0))
  ggsave(input_plot, file = "input_plot.pdf")
  
  ### Table of outputs ###
  probs <- sort(p_retaliation)
  mean_retal <- mean(probs)
  lower_95 <- probs[round((length(probs)*0.025), digits = 0)]
  upper_95 <- probs[round((length(probs)*0.975), digits = 0)]
  print(c(lower_95, mean_retal, upper_95))
  
  return(p)
  })
  
 # 
  
  
  output$distrib_prob <- renderPlot({  
   surviving()
  })
  
  
}) # close shinyServer function

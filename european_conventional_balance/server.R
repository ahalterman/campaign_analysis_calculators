library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(reshape2)


shinyServer(function(input, output) {
   

library(RColorBrewer)
library(scales)
library(grid)
theme_andy <- function() {
    
    # Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Greys", n=9)
    color.background = "white" #palette[2]
    color.grid.major = palette[3]
    color.axis.text = palette[6]
    color.axis.title = palette[7]
    color.title = palette[9]
    
    # Begin construction of chart
    theme_bw(base_size=9) +
      
      # Set the entire chart region to a light gray color
      theme(panel.background=element_rect(fill=color.background, color=color.background)) +
      theme(plot.background=element_rect(fill=color.background, color=color.background)) +
      theme(panel.border=element_rect(color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
      theme(panel.grid.minor=element_line(color=color.grid.major,size=.15)) +
      theme(panel.grid.minor.y=element_blank()) +
      theme(axis.ticks=element_blank()) +
      
      # Format the legend, but hide by default
      #theme(legend.position="none") +
      theme(legend.background = element_rect(fill=color.background)) +
      #theme(legend.text = element_text(size=7,color=color.axis.title)) +
      
      # Set title and axis labels, and format these and tick marks
      #theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
      #theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
      #theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
      #theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
      #theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }
  
theme_set(theme_andy())
 


sim <- function(initial_front_length, # see the UI for descriptions of each of these
                breakthrough_width,
                breakthru_num,
                offensive_ade_sector, 
                pact_d,
                pact_advance_rate,
                offensive_non_breakthru,
                nato_multiplier,
                pact_attrition_rate,
                pact_air_attr_rate,
                nato_air_attr_rate,
                pact_ac,
                pact_k,
                pact_sr,
                nato_ac,
                nato_k,
                nato_sr,
                defense_ade_sector,
                exchange_rate,
                print_diag = FALSE,
                n_steps = 100){
  # Question: how to handle ranges? Sample at each time step, or sample once per simulation?
  # Answer: once per simulation
  
  # draw values from range
  # this requires the simulation to be re-run many times
  #breakthrough_width <- runif(1, breakthrough_width[1], breakthrough_width[2])
  offensive_ade_sector <- runif(1, offensive_ade_sector[1], offensive_ade_sector[2])
  offensive_non_breakthru <- runif(1, offensive_non_breakthru[1], offensive_non_breakthru[2])
  pact_attrition_rate <- runif(1, pact_attrition_rate[1], pact_attrition_rate[2])
  pact_advance_rate <- runif(1, pact_advance_rate[1], pact_advance_rate[2])
  pact_attrition_rate <- pact_attrition_rate / 100
  exchange_rate <- runif(1, exchange_rate[1], exchange_rate[2])
  nato_multiplier <- runif(1, nato_multiplier[1], nato_multiplier[2])
  pact_ac <- round(runif(1, pact_ac[1], pact_ac[2]))
  nato_ac <- round(runif(1, nato_ac[1], nato_ac[2]))
  pact_air_attr_rate <- runif(1, pact_air_attr_rate[1], pact_air_attr_rate[2])
  nato_air_attr_rate <- runif(1, nato_air_attr_rate[1], nato_air_attr_rate[2])
  defense_ade_sector <- runif(1, defense_ade_sector[1], defense_ade_sector[2])
  pact_k <- runif(1, pact_k[1], pact_k[2])
  nato_k <- runif(1, nato_k[1], nato_k[2])
  
  # set up forces.
  # FOR FAST PACT MOBILIZATION: Pact ADEs arrive linearly from day 1 to day 29. Then constant after that.
  # (83 - 30) / 29  = 1.827586
  #pact_ades <- 30 + 1:29 * 1.827586
  #pact_ades <- c(pact_ades, rep(83, n_steps - length(pact_ades)))
  # FOR REGULAR PACT MOBILIZATION (fig. 3.4)
  pact_ades <- c(28:37, # first 10 days
                 seq(38, 40, length.out = 10), # from days 10 to 20, ADEs go from 38 to 40)
                 rep(40, 10), # from 20-30, constant at 40
                 seq(40, 50, length.out = 5),  # from day 30 to 35, it goes from 40 to 50
                 rep(50, (57 - 35)),  # constant at 50 from day 35 to 57)
                 seq(50, 55, length.out = (64-57)), # from day 58 to 64, it goes from 50 to 55
                 seq(55, 70, length.out = (70-64)),  # from day 65 to 70, it goes up to 70 ADEs
                 seq(70, 82, length.out = (78-70)), # from day 70 to 78, it goes from 70 ADEs to 82
                 rep(82, (90 - 78))) # from day 78 onward, constant at 82
  # manually copied from figure 3.6, page 110. The figure indicates that fractions of ADEs are added, but that's rounded to integers in 
  # the NATO case
  nato_ades <- c(22, 22, 22, 22, 22, 22, 22, 23, 24, 25, 26, 27, 28, 30, 32, 33, 33, 34, 34, 35, 36, 36, 37, 37, 38, 38, 39, 39, 40, 40, 41, 41, 41, 42, 42, 42, 42, 43, 43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 45, 45, 45, 45, 46, 46, 46, 46, 47, 47, 47, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 49, 49, 49, 50, 50)
  # if the steps are longer than 90, just add the last value repeatedly.
  pact_ades <- c(pact_ades, rep(pact_ades[length(pact_ades)], n_steps - length(pact_ades)))
  
  nato_ades <- c(nato_ades, rep(nato_ades[length(nato_ades)], n_steps - length(nato_ades)))
  nato_ades <- nato_multiplier * nato_ades
  
  daily_flank <- rep(NA, n_steps)
  pact_force_needs <- rep(NA, n_steps)
  pact_attrition_total <- rep(NA, n_steps)
  pact_flank_force <- rep(NA, n_steps)
  normal_daily_loss <- rep(NA, n_steps)
  attack_history <- rep(NA, n_steps)
  blu_cas_kills <- rep(NA, n_steps) 
  blue_cas_ac <- rep(NA, n_steps)
  cum_cas_kill <- rep(NA, n_steps)
  nato_daily_flank <- rep(NA, n_steps)
  nato_flank_force <- rep(NA, n_steps)
  nato_def_force <- rep(NA, n_steps)
  nato_force_needs <- rep(NA, n_steps)
  nato_daily_loss <- rep(NA, n_steps)
  nato_ground_loss <- rep(NA, n_steps)
  nato_air_loss <- rep(NA, n_steps)
  ground_cbt <- rep(NA, n_steps)
  daily_loss_red_air <- rep(NA, n_steps)
  red_cas_ac <- rep(NA, n_steps)
  nato_total_losses <- rep(NA, n_steps)
  
  # initialize a bunch to zero.
  pact_flank_force[1] <- 0
  pact_force_needs[1] <- 0
  pact_force_needs[2] <- 0
  normal_daily_loss[1] <- 0
  pact_attrition_total[1] <- 0
  cum_cas_kill[1] <- 0
  nato_daily_flank[1] <- 0
  nato_flank_force[1] <- 0
  ground_cbt[1] <- 0 # Soviet losses from NATO ground forces
  nato_total_losses[1] <- 0
  daily_loss_red_air[1] <- 0
  
  # initialize other starting values
  red_cas_ac[1] <- pact_ac # how many Sov planes
  blue_cas_ac[1] <- nato_ac # how many NATO planes
  
  # calculate pact cover forces needed (fixed over time)
  pact_cover_force <- (initial_front_length - (breakthrough_width * breakthru_num)) / 
                        offensive_non_breakthru
  
  # calculate pactassaultforce (what the pact needs at the front for its breakthrough)
  # doesn't vary by day because these are fixed. (The flanks, however, do vary)
  pact_assault_force <- (breakthrough_width * breakthru_num) / offensive_ade_sector
  
  # NATO's defense force is also fixed.
  # this is the number of ADEs away from the attack
  nato_def_force <- initial_front_length / defense_ade_sector
  
  
  # the big loop
  for (today in 2:n_steps){
    yesterday <- today - 1
    if (print_diag == TRUE){
      print(paste0("today = ", today))
      print(paste0("yesterday = ", yesterday))
    }
    
    #####################################
    ######   Soviet/Warsaw Pact   #######
    #####################################
    
    # calculate dailyflank
    if (print_diag == TRUE){
      print(paste0("pact_ades[today] ", pact_ades[today] ))
      print(paste0("pact_force_needs[yesterday] ", pact_force_needs[yesterday]))
    }
    
    if (pact_ades[today] > pact_force_needs[yesterday] & # confused
        today >= pact_d){ # it also has to be after the Pact start date
      daily_flank[today] <- (breakthru_num * pact_advance_rate * 2) / offensive_non_breakthru
      if (print_diag == TRUE){
        print(paste0("updated daily_flank: ", daily_flank[today]))
      }
    }
    else{
      daily_flank[today] <- 0
    }
    
    # pact_flank_force update
    pact_flank_force[today] <- pact_flank_force[yesterday] + daily_flank[today]
    
    # normal daily loss (pg 245)
    if (pact_ades[today] > pact_force_needs[yesterday] &
        today >= pact_d){
      if (print_diag == TRUE){
        print("attack!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      }
      normal_daily_loss[today] <- pact_assault_force * pact_attrition_rate
      attack_history[today] <- 1
    } else{
      if (print_diag == TRUE){
        print("no attack today...")
      }
      normal_daily_loss[today] <- 0
      attack_history[today] <- 0
    }
    
    # calculate Soviet losses from NATO/blue airpower
    # we've already calculated if an attack happens today, so just reuse
    if (attack_history[today] == 1){
      # blue CAS kills (in ADEs)
      #blu_cas_kills[today] <- blue_cas_ac[yesterday] * nato_k * 
      #  ((1 - (1 - nato_air_attr_rate)^(nato_sr + 1) / nato_air_attr_rate) - 1) * (1 / 1200)
      
      blu_cas_kills[today] <- blue_cas_ac[yesterday] * # available planes
                              nato_k * # vehicles killed per plane per sortie
                              # big junk
                        (((1 - (1 - nato_air_attr_rate)^(nato_sr + 1)) / nato_air_attr_rate) - 1) *
                              (1 / 1200) # vehicles per division
      
      
      # blue_cas_ac = how many NATO planes survive
      blue_cas_ac[today] <- blue_cas_ac[yesterday] * (1 - nato_air_attr_rate)^nato_sr 
    } else {
      blu_cas_kills[today] <- 0
      blue_cas_ac[today] <- blue_cas_ac[yesterday] # if no attack, no sorties, so no losses
    }
    cum_cas_kill[today] <- cum_cas_kill[yesterday] + blu_cas_kills[today]
    
    
    # calculate the Pact's attrition
    # note: don't confuse pact_attrition_rate (user parameter) and pact_attrition_total
    if (blu_cas_kills[today] > normal_daily_loss[today]){
      pact_attrition_total[today] <- pact_attrition_total[yesterday] + blu_cas_kills[today]
      ground_cbt[today] <- 0
    } else{
      pact_attrition_total[today] <- pact_attrition_total[yesterday] + normal_daily_loss[today]
      # Calculate groundcbt, the percentage of Pact losses caused by NATO ground forces.
      #  (this is needed in order to calculate NATO ground losses
      ground_cbt[today] <- normal_daily_loss[today] - blu_cas_kills[today]
    }
    
    if (print_diag == TRUE){
      print(paste0("pact_cover_force: ", pact_cover_force))
      print(paste0("pact_assault_force: ", pact_assault_force))
      print(paste0("pact_flank_force[today]: ", pact_flank_force[today]))
      print(paste0("pact_attrition_total[today]: ", pact_attrition_total[today]))
    }
    pact_force_needs[today] <- pact_cover_force + pact_assault_force + 
                                pact_flank_force[today] + pact_attrition_total[today]
    
    ################################################
    ####              NATO                     ####
    ################################################
    
    # calculate today's flank force requirements
    if (attack_history[today] == 1){
      nato_daily_flank[today] <- breakthru_num * pact_advance_rate * 2 / defense_ade_sector
    } else {
      nato_daily_flank[today] <- 0
    }
    
    # calculate:
    # 1. total NATO flank force requirements
    # 2. NATO's ground losses
    if (attack_history[today] == 1){
      nato_flank_force[today] <- nato_flank_force[yesterday] + nato_daily_flank[today]
      nato_ground_loss[today] <- ground_cbt[today] / exchange_rate
    } else {
      nato_flank_force[today] <- nato_flank_force[yesterday]
      nato_ground_loss[today] <- 0
    }
    

    # Calculate red/Soviet air losses
    if (attack_history[today] == 1){
      red_cas_ac[today] <- red_cas_ac[yesterday] * (1 - pact_air_attr_rate)^pact_sr
    } else{
      red_cas_ac[today] <- red_cas_ac[yesterday]
    }
    
    # calculate NATO's losses from Pact air forces
    # "dailylossredair"
    if (attack_history[today] == 1){
      daily_loss_red_air[today] <- red_cas_ac[yesterday] * # available planes
        pact_k * # vehicles killed per plane per sortie
        (((1 - (1 - pact_air_attr_rate)^(pact_sr + 1)) / pact_air_attr_rate) - 1) *
        (1 / 1200) # vehicles per division
      
    } else {
      daily_loss_red_air[today] <- 0
    }
    
    # calculate NATO's daily losses
    nato_daily_loss[today] <- nato_ground_loss[today] + daily_loss_red_air[today]
    if (print_diag == TRUE){
      print(paste0("nato_ground_loss[today]", nato_ground_loss[today]))
      print(paste0("daily_loss_red_air[today]", daily_loss_red_air[today]))
    }
     
    # calculate NATO's total losses
    nato_total_losses[today] <- nato_total_losses[yesterday] + nato_daily_loss[today]
    if (print_diag == TRUE){
      print(paste0("nato_total_losses[today]", nato_total_losses[today]))
      print(paste0("nato_def_force ",  nato_def_force))
      print(paste0("nato_flank_force[today] ", nato_flank_force[today]))
    }
    # NATO's total force needs
    nato_force_needs[today] <- nato_def_force + nato_flank_force[today] + nato_total_losses[today]
  }
  df <- data.frame(days = 1:n_steps,
                   pact_ades = pact_ades,
                   pact_force_needs = pact_force_needs,
                   nato_ades = nato_ades,
                   nato_force_needs = nato_force_needs,
                   nato_flank_force = nato_flank_force, # added
                   nato_total_losses = nato_total_losses, #added
                   nato_def_force = nato_def_force, #added
                   attack_history = attack_history,
                   red_cas_ac = red_cas_ac,
                   blue_cas_ac = blue_cas_ac,
                   nato_total_losses = nato_total_losses,
                   pact_attrition_total = pact_attrition_total,
                   daily_loss_red_air = daily_loss_red_air, # nato losses to "red" air
                   blu_cas_kills = blu_cas_kills) # pact losses to nato air.
  return(df)
}

## Test the code

#ggplot(dfs, aes(x = days, y = red_cas_ac,
#                id = as.factor(run))) + 
#  geom_line() #+ geom_point(aes(color = as.factor(attack_history)))



#force_ratio <- reactive({
#  reps <- 1000
#  offensive_ade_sector_draws <- runif(reps, 
#                                      min = input$offensive_ade_sector[1], 
#                                      max = input$offensive_ade_sector[2])
#  defensive_ade_sector_draws <- runif(reps, 
#                                      min = input$defense_ade_sector[1], 
#                                      max = input$defense_ade_sector[2])
#  ratio <- defensive_ade_sector_draws / offensive_ade_sector_draws
#  return(paste0("Mean force ratio of ", mean(ratio)))
#})


force_plot <- reactive({
  all_dfs <- list()
  initial_front_length <- input$initial_front_length
  breakthrough_width <- input$breakthrough_width
  breakthru_num <- input$breakthru_num
  offensive_ade_sector <- input$offensive_ade_sector
  pact_d <- input$pact_d
  pact_advance_rate <- input$pact_advance_rate
  offensive_non_breakthru <- input$offensive_non_breakthru
  nato_multiplier <- input$nato_multiplier
  pact_attrition_rate <- input$pact_attrition_rate
  pact_air_attr_rate <- input$pact_air_attr_rate
  nato_air_attr_rate <- input$nato_air_attr_rate
  nato_sr <- input$nato_sr
  pact_k <- input$pact_k
  pact_sr <- input$pact_sr
  pact_ac <- input$pact_ac
  nato_ac <- input$nato_ac
  nato_k <- input$nato_k
  defense_ade_sector <- input$defense_ade_sector
  exchange_rate <- input$exchange_rate
  
  for (i in 1:input$n_sims){
    df <- sim(initial_front_length = initial_front_length,
              breakthrough_width = breakthrough_width,
              breakthru_num = breakthru_num,
              offensive_ade_sector = offensive_ade_sector,
              pact_d = pact_d,
              pact_advance_rate = pact_advance_rate,
              offensive_non_breakthru = offensive_non_breakthru,
              nato_multiplier = nato_multiplier,
              pact_attrition_rate = pact_attrition_rate,
              pact_air_attr_rate = pact_air_attr_rate,
              nato_air_attr_rate = nato_air_attr_rate,
              pact_ac = pact_ac,
              pact_k = pact_k,
              pact_sr = pact_sr,
              nato_ac = nato_ac,
              nato_k = nato_k,
              nato_sr = nato_sr,
              defense_ade_sector = defense_ade_sector,
              exchange_rate = exchange_rate,
              print_diag = FALSE)
    df$days <- df$days - 1 # start day is 0
    df$run <- i
    all_dfs[[i]] <- df
  }
  dfs <- dplyr::bind_rows(all_dfs)
  
  melted <- melt(dfs, id.vars = c("run", "days", "attack_history"))
  sub <- melted %>% 
    group_by(days) %>% 
    summarize(prob = mean(attack_history))
  pact_assault <- ggplot(sub, aes(x = days, y = prob)) + 
    geom_line() +
    labs(title = "Probability of the Pact having enough forces to continue the attack",
         y = NULL,
         x = "Days since Pact mobilization")
  
  # plot NATO supply vs. NATO demand
  nato_needs <- ggplot(dfs, aes(x = days, y = nato_force_needs, group = as.factor(run))) + 
    geom_line(size = 3 / input$n_sims) +
    geom_line(aes(y = nato_ades), color = "red", size = 3 / input$n_sims) +
    labs(title = "NATO forces supply (red) and demand (black)",
         y = NULL,
         x = "Days since Pact mobilization")
    
  
  # plot Pact supply vs. Pact demand
  pact_needs <- ggplot(dfs, aes(x = days, y = pact_force_needs, group = as.factor(run))) + 
    geom_line(size = 3 / input$n_sims) +
    geom_line(aes(y = pact_ades), color = "red", size = 3 / input$n_sims) +
    labs(title = "Pact forces supply (red) and demand (black)",
         y = NULL,
         x = "Days since Pact mobilization")
  
  # plot probability that NATO has a shortfall
  shortfall <- dfs %>% 
    group_by(days) %>% 
    summarize(prob = mean(nato_force_needs > nato_ades))
    
  shortfall_plot <- ggplot(shortfall, aes(x = days, y = prob)) + 
    geom_line(size = 0.5) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(title = "Probability of NATO force shortfall",
         y = NULL,
         x = "Days since Pact mobilization")
  
  red_air_surviving <- ggplot(dfs, aes(x = days, y = red_cas_ac, id = as.factor(run))) + 
    geom_line(size = 0.1, alpha = 0.5) +
    #scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(title = "Number of surviving Russian CAS aircraft",
         y=NULL,
         x = "Days since Pact mobilization")
  
  blue_air_surviving <- ggplot(dfs, aes(x = days, y = blue_cas_ac, id = as.factor(run))) + 
    geom_line(size = 0.1, alpha = 0.5) +
    #scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(title = "Number of surviving NATO CAS aircraft",
         y = NULL,
         x = "Days since Pact mobilization")
  
  pact_total_losses <- ggplot(dfs, aes(x = days, y = pact_attrition_total, id = as.factor(run))) + 
    geom_line(size = 0.1, alpha = 0.5) +
    labs(title = "Cumulative Pact losses",
         y = NULL,
         x = "Days since Pact mobilization")
  
  nato_total_losses <- ggplot(dfs, aes(x = days, y = nato_total_losses, id = as.factor(run))) + 
    geom_line(size = 0.1, alpha = 0.5) +
    labs(title = "Cumulative NATO losses",
         x = "Days since Pact mobilization",
         y = NULL)
  
  p <- dfs[,c("days", "run", "daily_loss_red_air", "blu_cas_kills")]
  p <- melt(p, id.vars = c("days", "run"))
  air_losses <- ggplot(p, aes(x = as.integer(days), y = value, color = as.factor(variable), id = as.factor(run))) + 
    geom_line(size = 0.1, alpha = 0.5) +
    guides(color=FALSE) +
    labs(title = "Kills by close air support (blue=kills by NATO, red=Pact)",
         y = NULL,
         x = "Days since Pact mobilization")

  
  nn <- dfs %>% 
    select(days, nato_flank_force, run, nato_def_force, nato_total_losses) %>% 
    reshape2::melt(id.vars = c("days", "run"))
  
  nn$variable <- as.factor(nn$variable) 
  nn$variable <- recode(nn$variable, 
                     nato_flank_force = "Flank Force",
                     nato_def_force = "Line Defense",
                     nato_total_losses = "Losses")
  #save(nato_needs, file = "nato_needs.RData")
  needs_breakdown <- ggplot(nn, aes(x = as.integer(days), y = value, color = as.factor(variable), id = as.factor(run))) + 
    geom_line(size = 0.3, alpha = 0.5) +
    labs(#title = "Breakdown of NATO force requirements",
         y = NULL,
         x = "Days since Pact mobilization",
         color = "NATO Force Requirement") +
    theme(legend.position = "top", legend.justification = "right")
    
  #load("~/MIT/Calculator/Barry_Calculator/barry_output.RData")
  dfs$nato_shortfall <- (dfs$nato_force_needs > dfs$nato_ades) & dfs$days >= pact_d
  dfs <- dfs %>% 
    group_by(run) %>% 
    mutate(shortfall_days = cumsum(nato_shortfall))
  dfs$ever_shortfall <- dfs$shortfall_days > 0
  
  ever_shortfall <- dfs %>% 
    group_by(days) %>% 
    summarize(shortfall_mean = mean(ever_shortfall)) %>% 
    ggplot(aes(x = days, y = shortfall_mean)) +
    geom_line() +
    ylim(0, 1) +
    labs(title = "Cumulative probability of NATO shortfall",
         y = NULL,
         x = "Days since Pact mobilization")
  
  #save(dfs, file = "barry_output.RData")
  plots <- list(shortfall_plot, pact_assault, ever_shortfall, needs_breakdown, nato_needs, pact_needs, 
                pact_total_losses, nato_total_losses, red_air_surviving, blue_air_surviving, air_losses)
  p <- grid.arrange(grobs=plots, ncol=2)
  #ggsave(file = "posen_plot.pdf", p)
  return(p)
})

output$force_plot <- renderPlot({
  force_plot()
})

#output$force_ratio <- renderText({
#    force_ratio()
#})

}) # close shinyServer function




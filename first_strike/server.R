library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(gridExtra)
library(plotly)


shinyServer(function(input, output) {
   

library(RColorBrewer)
library(scales)
library(grid)
theme_andy <- function() {
    
    # Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Greys", n=9)
    color.background = "white" #"#f5f5f5" # this is what the Shiny slider background is    #palette[2]
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
      
      # Format the legend,
      theme(legend.background = element_rect(fill=color.background)) +
      #theme(legend.text = element_text(size=7,color=color.axis.title)) +
      
      # Set title and axis labels, and format these and tick marks
      theme(plot.title=element_text(color=color.title, size=14, vjust=1.25)) +
      theme(axis.text.x=element_text(size=12,color=color.title)) +
      theme(axis.text.y=element_text(size=12,color=color.title)) +
      theme(axis.title.x=element_text(size=12,color=color.title, vjust=0)) +
      theme(axis.title.y=element_text(size=12,color=color.title, vjust=1.25)) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }
  
theme_set(theme_andy())
 
## Taken from pg. 14-15 and Table 1
#russian_silo_icbms <- 258 # pg. 14
#russian_silo_targets <- 258 # each silo is separate target
#
#russian_mobile_icbms <- 291 # pg. 14
#russian_mobile_targets <- 40 # each garrison is one target
#
#russian_bombers <- 78 # pg. 14-15
#russian_air_bases <- 2 + 7 + 54 # primary plus backup (pg. 15) plus dispersal
#russian_subs <- 9 # pg. 15
#russian_subs_targets <- 3
#
#russian_storage <- 127 # pg. 15

#########################################################
## Russian Aimpoints, taken from Table 1 (pg. 16)   #####
#########################################################

# number of silo aimpoints
ss_18_silo_aps <- 85
ss_19_silo_aps <- 129
ss_27_silo_aps <- 44

# silo overpressue tolerance 
ss_18_psi <- 3000
ss_19_psi <- 5000
ss_27_psi <- 5000

# mobile ICBMs
mobile_icbm_aps <- 40
mobile_icbm_psi <- NA

# subs
sub_aps <- 3
sub_psi <- NA

# bombers
bomber_primary_aps <- 9
bomber_secondary_aps <- 54

storage_aps <- 127



#########################################################
########## US nuclear warheads and accuracy  ############
##########   (taken from Table 2, pg. 18)    ############
#########################################################

# subs
average_warheads_per_missile <- 6
w88_num <-  64 * average_warheads_per_missile # 64 is number of W88-outfitted missiles
w76_num <- 1152 - w88_num  # total warhead number minus W88 number (see note to Tbl 2)

#########################################################
########## Initialize US weapon parameters   ############
#########################################################

init_ohio_w88 <- function(num = 381, yield = 455, cep = 90,
                       reliability = 0.8){
  ohio_w88 <- list()
  ohio_w88[["num"]] <- num
  ohio_w88[["yield"]] <- yield
  ohio_w88[["cep"]] <- cep
  ohio_w88[["reliability"]] <- reliability
  return(ohio_w88)
}


init_ohio_w76 <- function(num = w76_num, yield = 100, cep = 90,
                          reliability = 0.8){
  ohio_w76 <- list()
  ohio_w76[["num"]] <- num
  ohio_w76[["yield"]] <- yield
  ohio_w76[["cep"]] <- cep
  ohio_w76[["reliability"]] <- reliability
  return(ohio_w76)
}

# icbms
init_minuteman_w78 <- function(num = 713, yield = 335, cep = 120,
                               reliability = 0.8){
  minuteman_w78 <- list()
  minuteman_w78[["num"]] <- num
  minuteman_w78[["yield"]] <- yield
  minuteman_w78[["cep"]] <- cep
  minuteman_w78[["reliability"]] <- reliability
  return(minuteman_w78)
}

init_minuteman_w62 <- function(num = 285, yield = 170, cep = 180,
                               reliability = 0.8){
  minuteman_w62 <- list()
  minuteman_w62[["num"]] <- num
  minuteman_w62[["yield"]] <- yield
  minuteman_w62[["cep"]] <- cep
  minuteman_w62[["reliability"]] <- reliability
  return(minuteman_w62)
}

# b2 bombers
init_b2_b83 <- function(num = 256, yield = 1200, cep = 150,
                        reliability = 0.8){
  b2_b83 <- list()
  b2_b83[["num"]] <- num
  b2_b83[["yield"]] <- yield
  b2_b83[["cep"]] <- cep
  b2_b83[["reliability"]] <- reliability
  return(b2_b83)
}

# air-launched cruise missiles (from b52)
init_b52_agm <- function(num = 840, yield = 150, cep = 30,
                        reliability = 0.8,
                        detection_prob = 1){
  b52_agm <- list()
  b52_agm[["num"]] <- num
  b52_agm[["yield"]] <- yield
  b52_agm[["cep"]] <- cep
  b52_agm[["reliability"]] <- reliability
  b52_agm[["detection_prob"]] <- detection_prob
  return(b52_agm)
}

ohio_w88 <- init_ohio_w88()
ohio_w76 <- init_ohio_w76()
minuteman_w78 <- init_minuteman_w78()
minuteman_w62 <- init_minuteman_w62()
b2_b83 <- init_b2_b83()
b52_agm <- init_b52_agm()

lethal_radius <- function(yield, psi){
  # this equation uses yield in megatons (see pg 41) so correct for that
  lr <- 2.62 * (yield / 1000) ^(1/3) / psi^(1/3)
  # lethal radius is returned in nautical miles
  return(lr)
}

sspk <- function(lr, cep){
  # single shot probability of kill
  # lr: lethal radius in nautical miles
  # cep: circular error probable (that is, median miss distance in meters)
  # first, convert nautical miles to meters (?????????)
  lr_meters <- lr * 1852
  pk <- 1 - 0.5^(lr_meters/cep)^2
  return(pk)
}


single_strike <- function(target_psi, warhead){
  # returns tkp (terminal kill probability), the single shot probability of
  #  a kill, excluding the reliability of the weapon)
  lr <- lethal_radius(warhead$yield, target_psi)
  pk <- sspk(lr, warhead$cep)
  return(pk)
}


single_target_attack <- function(ohio_w88, ohio_w88_num,
                                 ohio_w76, ohio_w76_num, 
                                 b2_b83, b2_b83_num, 
                                 b52_agm, b52_agm_num, 
                                 minuteman_w78, minuteman_w78_num,
                                 minuteman_w62, minuteman_w62_num,
                                 target_psi,
                                 prob_cloud = 0.75){
  # Returns: probability of a single target being destroyed
  
  # for each target (e.g. silo), allocate the same number of each warhead
  prob_surv <- 1
  prob_clear_skies <- 1 # probability that no fratricide occurs
  
  # Use the most accurate two for the initial strike?
  
  # make a vector or list of warheads (rep using the numbers of each)
  #  then loop through each, 2 by 2, calculating fratricide risk at each.
  #  order the list by the most to least accurate??
  warheads <- c(rep(list(ohio_w88), ohio_w88_num), 
                rep(list(ohio_w76), ohio_w76_num),
                rep(list(b2_b83), b2_b83_num),
                rep(list(b52_agm), b52_agm_num),
                rep(list(minuteman_w78), minuteman_w78_num),
                rep(list(minuteman_w62), minuteman_w62_num))
  if (length(warheads) == 0){
    return(0)
  }
  for (i in 1:length(warheads)){
    warhead <- warheads[[i]]
    # assuming the warhead makes it through, probability of target destruction
    tk <- single_strike(target_psi, warhead)
    
    # calculate the probability there's no fraticide. 
    #  Conditions: (1) the missile worked: warhead$reliability
    #              (2) but missed: (1 - tk)
    #              (3) and made a cloud in the way (prob_cloud)
    prob_clear_skies <- prob_clear_skies * (1 - prob_cloud * (1 - tk) * warhead$reliability)
    if (i <= 2){
      # Get two "freebies": guarenteed hits without worrying about fratricide
      # overall probability of survival: p(survive) = p(survive | hit) p(hit)
      p_kill_with_this_warhead <-  tk * warhead$reliability
      prob_surv <- prob_surv *  (1 - p_kill_with_this_warhead)
    }
    if (i > 2){
      # now need to account for fratricide risk
      prob_surv <- prob_surv * prob_clear_skies * (1 - tk * warhead$reliability)
    }
  }
  p_kill <- 1 - prob_surv
  return(p_kill)
}

target_to_aimpoints <- function(target) {
  # Make a dataframe that has t rows where is targets and w columns were w is the number
  # of US warhead types.
  # Parameters: 
  #   Target: list
  #     Russian target (e.g. silo, with PSI info)
  df <- data.frame(id = 1:target[["num"]])
  df$psi <- target[["psi"]]
  df$name <- target[["name"]]
  df$death_prob <- 0
  # construct columns that indicate which warheads can hit this target
  wh <- c("ohio_w88", "ohio_w76", "b2_b83", "b52_agm", "minuteman_w78", "minuteman_w62")
  for (i in 1:length(wh)){
    warhead <- wh[i]
    df[,warhead] <- warhead %in% target[["eligible_warheads"]]
  }
  return(df)
}

make_aimpoints <- function(targets){
  # Make a big matrix/dataframe to store weapon assignment
  aimpoints_list <- list()
  for (t in 1:length(targets)){
    if(targets[[t]]$num > 0){
      aimpoints_list[[t]] <- target_to_aimpoints(targets[[t]])
    }
  }
  aimpoints <- bind_rows(aimpoints_list)
  return(aimpoints)
}

assign_warheads <- function(targets, # a list of targets (e.g. ss_18, ss_other, missile_mobile, airfield_primary... 
                            ohio_w88,
                            ohio_w76,
                            b2_b83,
                            b52_agm,
                            minuteman_w78,
                            minuteman_w62){
  # Each target is a named list that says how many and what their max PSI is.
  ## allocate warheads to targets in a semi-efficient way.
  aimpoints <- make_aimpoints(targets)
  set.seed(123)
  #aimpoints <- aimpoints[sample(1:nrow(aimpoints)),]  # DON'T SHUFFLE!!!!! I used to and it didn't work
  
  warheads <- c(rep("ohio_w88", ohio_w88$num),            # fast
                rep("ohio_w76", ohio_w76$num),            # fast
                rep("b2_b83", b2_b83$num),                # slow
                rep("b52_agm", b52_agm$num),              # fast
                rep("minuteman_w78", minuteman_w78$num),  # slow
                rep("minuteman_w62", minuteman_w62$num))  # slow
  set.seed(123)
  # shuffle the warheads to avoid some (but not all!) pathological assignments
  # TODO: allocate the fast movers first to make sure everyone gets one.
  warheads <- warheads[sample(1:length(warheads))]
  # make this a vector instead and just iterate through
  
  # include dummy columns for each weapon that can be checked...
  warhead_types <- c("ohio_w88", "ohio_w76", "b2_b83", "b52_agm", "minuteman_w78", "minuteman_w62")
  assignments <- matrix(0, nrow = nrow(aimpoints), ncol = length(warhead_types))
  colnames(assignments) <- warhead_types
  
  
    for (w in 1:length(warheads)){
      warhead_type = warheads[w]
      # find the rows that can be hit by this warhead
      eligible_rows <- aimpoints[,warhead_type] == 1
      if (sum(eligible_rows) == 0){
        next
      }
      # of those, find the one with the lowest kill probability
      row_to_change <- which(aimpoints$death_prob == min(aimpoints[eligible_rows, "death_prob"]) & eligible_rows)[1]
      # if this results in no target, skip
      # increment the warhead count on this target
      assignments[row_to_change, warhead_type] <- 1 + assignments[row_to_change, warhead_type]
      # update the kill probability for this target
      death_prob <- single_target_attack(ohio_w88, assignments[row_to_change, 1],
                                         ohio_w76, assignments[row_to_change, 2], 
                                         b2_b83, assignments[row_to_change, 3], 
                                         b52_agm, assignments[row_to_change, 4], 
                                         minuteman_w78, assignments[row_to_change, 5],
                                         minuteman_w62, assignments[row_to_change, 6],
                                         aimpoints[row_to_change, "psi"],
                                         prob_cloud = 0.75)
      aimpoints[row_to_change, "death_prob"] <- death_prob
  }
  df <- cbind(aimpoints, assignments)
  #save(df, file = "/Users/ahalterman/nukes_tmp.RData")
  return(df)
}

#single_target_attack(ohio_w88, 1,
#                     ohio_w76, 1, 
#                     b2_b83, 0, 
#                     b52_agm, 0, 
#                     target_psi = 3000,
#                     prob_cloud = 0.75)

## BIG QUESTION TIME:
# - do we assume US launches with Press and Lieber amounts and see what survies?
# - or do we calculate the number of US launches needed to reach e.g. 99% chance
#   of 0 remaining Russian ICBMs?


sim <- function(surv_prob, detection_prob = 1){
  # Given a vector of survival probabilities, simulate how many units will survive
  detection <- rbinom(length(surv_prob), 
                      size = 1, 
                      prob = detection_prob)
  surviving_units <- length(surv_prob) - sum(detection * rbinom(n = length(surv_prob), size = 1, surv_prob))
  return(surviving_units)
}

targets_and_assignments <- function(targets, assignments, NSIMS){
  # As a hack above to make assignemnt easier, the "assignments" df has duplicate column names.
  assignment_df <- assignments[, c(1:4, 11:16)]
  #save(assignment_df, file = "~/Desktop/assignment_df.RData")
  icbm_assignments <- assignment_df[assignment_df$name %in% c("ss18_silo", "other_silo"),]
  surv_num <- replicate(NSIMS, sim(icbm_assignments$death, targets[[1]]$detection_prob),
                        simplify = TRUE)
  surv_icbm <- data.frame(table(surv_num),
                          stringsAsFactors = FALSE)
  surv_icbm$surv_platform_num <- as.numeric(as.character(surv_icbm$surv_num))
  #surv_icbm$warhead_num <- 
  surv_icbm$platform <- "ICBM"
  
  #### mobile missiles ###
  mobile_assignments <- assignment_df[assignment_df$name %in% c("mobile_missile"),]
  #print(paste0("nrow mobile garrison ", nrow(mobile_assignments)))
  #print(mobile_assignments)
  surv_num <- replicate(NSIMS, sim(mobile_assignments$death, 
                                   targets[[3]]$detection_prob),
                        simplify = TRUE)
  surv_mob <- data.frame(table(surv_num),
                         stringsAsFactors = FALSE)
  #print("surviving mobile garrisons:")
  #print(surv_mob)
  surv_mob$surv_platform_num <- as.numeric(as.character(surv_mob$surv_num))
  surv_mob$platform = "Mobile Missile (Garrison)"
  # How many warheads survive?
  # 1 warhead per missile
  # 291 missiles in 40 garrisons
  # make the (unrealistic) assumption that all the missiles are destroyed or none.
  #surv_mob$surv_warhead_num <- round(surv_mob$surv_platform_num * (291/40), 0)
  
  #### mobile missiles DEPLOYED ###
  mobile_deployed_assignments <- assignment_df[assignment_df$name %in% c("mobile_missile_deployed"),]
  #print(paste0("nrow: ", nrow(mobile_deployed_assignments)))
  #print(mobile_deployed_assignments)
  
    surv_num <- replicate(NSIMS, sim(mobile_deployed_assignments$death, 
                                     targets[[4]]$detection_prob),
                          simplify = TRUE)
    surv_mob_dep <- data.frame(table(surv_num),
                           stringsAsFactors = FALSE)
  #print(surv_mob_dep)
    surv_mob_dep$surv_platform_num <- as.numeric(as.character(surv_mob_dep$surv_num))
    surv_mob_dep$platform = "Mobile Missile (Deployed)"
  
  # air fields
  airfield_assignments <- assignment_df[assignment_df$name %in% c("airfield_primary", "airfield_secondary"),]
  surv_num <- replicate(NSIMS, sim(airfield_assignments$death),
                        simplify = TRUE)
  surv_airfield <- data.frame(table(surv_num),
                              stringsAsFactors = FALSE)
  surv_airfield$surv_platform_num <- as.numeric(as.character(surv_airfield$surv_num))
  surv_airfield$platform <- "Airfield"
  
  df <- rbind(surv_icbm, surv_mob, 
              surv_mob_dep, 
              surv_airfield)
  
  surviving_plot <- ggplot(data = df, aes(x = surv_platform_num, y = Freq/NSIMS, fill = platform)) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(labels=scales::percent) + 
    facet_wrap(~platform, nrow=4) +
    theme(legend.position = "none") +
    labs(y = "Probability",
         x = "Number of surviving platforms",
         title = "Number of surviving platform types")
  #ggsave(surviving_plot, file = "plot_surviving_warheads.pdf", width = 8, height = 6)
  
  ##### PLOT: survival probability histograms by target type
  survival_probs_plot <- assignment_df %>% 
    ggplot(., aes(x = death_prob, fill = name)) +
    geom_histogram(bins=50) +
    facet_wrap(~name, nrow=3)
  
  
  ##### PLOT: number of warheads per target type
  assignment_plot <- assignment_df %>% 
    select(-psi, -death_prob, -id) %>% 
    reshape2::melt(., id.vars = c("name")) %>% 
    group_by(name, variable) %>% 
    summarize(total = sum(value)) %>% 
    #reshape2::dcast(name ~ variable) %>% 
    ggplot(., aes(name, variable, fill = total)) + 
    geom_tile(colour = "white") + 
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = "US Warhead Types",
         x = "Russian Targets",
         title = "Assignment of US Warheads to Russian Targets")
  #ggsave(assignment_plot, file = "plot_warhead_allocation.pdf", width = 8, height = 6)
  
  
  
  #### Bundle the plots together
  plots <- list(surviving_plot, 
                #survival_probs_plot, 
                assignment_plot)
  plot_grid <- grid.arrange(grobs=plots, ncol=1)
  return(plot_grid)
}


attack_plot1 <- reactive({
  NSIMS <- 500
  ohio_w88 <- init_ohio_w88(reliability = input$ohio_w88_reliability,
                            num = input$ohio_w88_num,
                            cep = input$ohio_w88_cep)
  ohio_w76 <- init_ohio_w76(reliability = input$ohio_w76_reliability,
                            num = input$ohio_w76_num,
                            cep = input$ohio_w76_cep)
  b2_b83 <- init_b2_b83(reliability = input$b2_b83_reliability,
                        num = input$b2_b83_num,
                            cep = input$b2_b83_cep)
  b52_agm <- init_b52_agm(reliability = input$b52_agm_reliability,
                          num = input$b52_agm_num,
                            cep = input$b52_agm_cep)
  minuteman_w78 <- init_minuteman_w78(reliability = input$minuteman_w78_reliability,
                                      num = input$minuteman_w78_num,
                                      cep = input$minuteman_w78_cep)
  minuteman_w62 <- init_minuteman_w62(reliability = input$minuteman_w62_reliability,
                                      num = input$minuteman_w62_num,
                                      cep = input$minuteman_w62_cep)
  
  ss_18 <- list("num" = input$ss_18_num,
                "psi" = input$silo_18_psi,
                "name" = "ss18_silo",
                "eligible_warheads" = c("ohio_w88", "ohio_w76",  "b2_b83", "b52_agm", "minuteman_w78", 
                                        "minuteman_w62"),
                "detection_prob" = input$silo_detection)
  
  ss_other <- list("num" = input$ss_other_num,
                   "psi" = input$silo_other_psi,
                   "name" = "other_silo",
                   "eligible_warheads" = c("ohio_w88", "ohio_w76", "b2_b83", "b52_agm", "minuteman_w78", 
                                           "minuteman_w62"),
                   "detection_prob" = input$silo_detection)
  
  missile_mobile <- list("num" = input$missile_mobile_num,
                         "psi" = 30,
                         "name" = "mobile_missile",
                         "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                 "minuteman_w78", 
                                                 "minuteman_w62"),
                         "detection_prob" = input$mobile_base_detection)
  
  #print(input$missile_mobile_deployed_num) 
  missile_mobile_deployed <- list("num" = input$missile_mobile_deployed_num,
                         "psi" = input$mobile_missile_psi,
                         "name" = "mobile_missile_deployed",
                         "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                 "minuteman_w78", 
                                                 "minuteman_w62"),
                         "detection_prob" = input$mobile_deployed_detection)
  
  airfield_primary <- list("num" = input$airfield_primary_num,
                           "psi" = 30,
                           "name" = "airfield_primary",
                           "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                   "b52_agm",
                                                   "minuteman_w78", 
                                                   "minuteman_w62"),
                           "detection_prob" = 1)
  
  airfield_secondary <- list("num" = input$airfield_secondary_num,
                             "psi" = 30,
                             "name" = "airfield_secondary",
                             "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                     "minuteman_w78", 
                                                     "minuteman_w62"),
                             "detection_prob" = 1)
  
  naval_primary <- list("num" = input$naval_primary_num,
                        "psi" = 315,
                        "name" = "naval_primary",
                        "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                "b52_agm", "minuteman_w78", 
                                                "minuteman_w62"),
                        "detection_prob" = 1)
  
  naval_secondary <- list("num" = input$naval_secondary_num,
                          "psi" = 315,
                          "name" = "naval_secondary",
                          "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                  "b2_b83", "b52_agm", "minuteman_w78", 
                                                  "minuteman_w62"),
                          "detection_prob" = 1)
  
  targets <- list(ss_18, ss_other, missile_mobile, missile_mobile_deployed, airfield_primary, 
                  airfield_secondary, naval_primary,naval_secondary)
  #save(targets, file = "~/targets.RData")
  assignments <- assign_warheads(targets, ohio_w88, ohio_w76, b2_b83, b52_agm,
                                 minuteman_w78, minuteman_w62)
  
  plots <- targets_and_assignments(targets, assignments, NSIMS)
})


attack_plot2 <- reactive({
  NSIMS <- 500
  ohio_w88 <- init_ohio_w88(reliability = input$ohio_w88_reliability2,
                            num = input$ohio_w88_num2,
                            cep = input$ohio_w88_cep2)
  ohio_w76 <- init_ohio_w76(reliability = input$ohio_w76_reliability2,
                            num = input$ohio_w76_num2,
                            cep = input$ohio_w76_cep2)
  b2_b83 <- init_b2_b83(reliability = input$b2_b83_reliability2,
                        num = input$b2_b83_num2,
                        cep = input$b2_b83_cep2)
  b52_agm <- init_b52_agm(reliability = input$b52_agm_reliability2,
                          num = input$b52_agm_num2,
                          cep = input$b52_agm_cep2)
  minuteman_w78 <- init_minuteman_w78(reliability = input$minuteman_w78_reliability2,
                                      num = input$minuteman_w78_num2,
                                      cep = input$minuteman_w78_cep2)
  minuteman_w62 <- init_minuteman_w62(reliability = input$minuteman_w62_reliability2,
                                      num = input$minuteman_w62_num2,
                                      cep = input$minuteman_w62_cep2)
  
  ss_18 <- list("num" = input$ss_18_num2,
                "psi" = input$silo_18_psi2,
                "name" = "ss18_silo",
                "eligible_warheads" = c("ohio_w88", "ohio_w76",  "b2_b83", "b52_agm", "minuteman_w78", 
                                        "minuteman_w62"),
                "detection_prob" = input$silo_detection2)
  
  ss_other <- list("num" = input$ss_other_num2,
                   "psi" = input$silo_other_psi2,
                   "name" = "other_silo",
                   "eligible_warheads" = c("ohio_w88", "ohio_w76", "b2_b83", "b52_agm", "minuteman_w78", 
                                           "minuteman_w62"),
                   "detection_prob" = input$silo_detection2)
  
  missile_mobile <- list("num" = input$missile_mobile_num2,
                         "psi" = 30,
                         "name" = "mobile_missile",
                         "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                 "minuteman_w78", 
                                                 "minuteman_w62"),
                         "detection_prob" = input$mobile_base_detection2)
  
  missile_mobile_deployed <- list("num" = input$missile_mobile_deployed_num2,
                                  "psi" = input$mobile_missile_psi2,
                                  "name" = "mobile_missile_deployed",
                                  "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                          "minuteman_w78", 
                                                          "minuteman_w62"),
                                  "detection_prob" = input$mobile_deployed_detection2)
  
  airfield_primary <- list("num" = input$airfield_primary_num2,
                           "psi" = 30,
                           "name" = "airfield_primary",
                           "eligible_warheads" = c("ohio_w88", "ohio_w76"),
                           "detection_prob" = 1)
  
  airfield_secondary <- list("num" = input$airfield_secondary_num2,
                             "psi" = 30,
                             "name" = "airfield_secondary",
                             "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                    "minuteman_w78", 
                                                     "minuteman_w62"),
                             "detection_prob" = 1)
  
  naval_primary <- list("num" = input$naval_primary_num2,
                        "psi" = 315,
                        "name" = "naval_primary",
                        "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                "b52_agm", "minuteman_w78", 
                                                "minuteman_w62"),
                        "detection_prob" = 1)
  
  naval_secondary <- list("num" = input$naval_secondary_num2,
                          "psi" = 315,
                          "name" = "naval_secondary",
                          "eligible_warheads" = c("ohio_w88", "ohio_w76",
                                                  "b2_b83", "b52_agm", "minuteman_w78", 
                                                  "minuteman_w62"),
                          "detection_prob" = 1)
  
  targets <- list(ss_18, ss_other, missile_mobile, missile_mobile_deployed, 
                  airfield_primary, airfield_secondary, naval_primary,
                  naval_secondary)
  assignments <- assign_warheads(targets, ohio_w88, ohio_w76, b2_b83, b52_agm,
                                 minuteman_w78, minuteman_w62)
  plots <- targets_and_assignments(targets, assignments, NSIMS)
})

force_plot <- reactive({
  # calculate how much of the force is used
  df <- data.frame(ohio_w88 = (ohio_w88$num - input$ohio_w88_num * total_silo_aps) / ohio_w88$num,
                   ohio_w79 = (ohio_w76$num - input$ohio_w76_num * total_silo_aps) / ohio_w76$num,
                   b2_b83 = (b2_b83$num - input$b2_b83_num * total_silo_aps) /  b2_b83$num,
                   b52_agm = (b52_agm$num - input$b52_agm_num * total_silo_aps) / b52_agm$num)
  
  df <- reshape2::melt(df)
  g <- ggplot(df, aes(x = variable, y = value)) + geom_bar(stat = "identity") +
    scale_y_continuous(labels = percent)
  return(g)
})


output$attack_plot1 <- renderPlot({
  attack_plot1()
})

output$attack_plot2 <- renderPlot({
  attack_plot2()
})


}) # close shinyServer function





#a <- assign_warheads(aimpoints = 258, 
#                ohio_w88_num,
#                ohio_w76_num, 
#                b2_b83_num, 
#                b52_agm_num,
#                minuteman_w78_num,
#                minuteman_w62_num)
#



#
#ss_18 <- list("num" = 151,
#              "psi" = 3000,
#              "name" = "ss18_silo",
#              "eligible_warheads" = c("ohio_w88", "ohio_w76",  "b2_b83", "b52_agm", "minuteman_w78", 
#                                      "minuteman_w62"))
#
#ss_other <- list("num" = 81,
#                 "psi" = 5000,
#                 "name" = "other_silo",
#                 "eligible_warheads" = c("ohio_w88", "ohio_w76", "b2_b83", "b52_agm", "minuteman_w78", 
#                                         "minuteman_w62"))
#
#missile_mobile <- list("num" = 40,
#                       "psi" = 30,
#                       "name" = "mobile_missile",
#                       "eligible_warheads" = c("ohio_w88", "ohio_w76"))
#
#airfield_primary <- list("num" = 9,
#                         "psi" = 30,
#                         "name" = "airfield_primary",
#                         "eligible_warheads" = c("ohio_w88", "ohio_w76"))
#
#airfield_secondary <- list("num" = 111,
#                           "psi" = 30,
#                           "name" = "airfield_secondary",
#                           "eligible_warheads" = c("ohio_w88", "ohio_w76"))
#
#naval_primary <- list("num" = 135,
#                      "psi" = 315,
#                      "name" = "naval_primary",
#                      "eligible_warheads" = c("ohio_w88", "ohio_w76"))
#
#naval_secondary <- list("num" = 444,
#                        "psi" = 315,
#                        "name" = "naval_seconary",
#                        "eligible_warheads" = c("ohio_w88", "ohio_w76"))
#
#targets <- list(ss_18, ss_other, missile_mobile, airfield_primary, airfield_secondary, naval_primary,
#                naval_secondary)
#tmp <- make_aimpoints(targets)
#
#assignments <- assign_warheads(targets, ohio_w88, ohio_w76, b2_b83, b52_agm,
#                               minuteman_w78, minuteman_w62)
#  
#
#single_target_attack(ohio_w88, 0,
#                     ohio_w76,2, 
#                     b2_b83, 1, 
#                     b52_agm, 2, 
#                     minuteman_w78, 1,
#                     minuteman_w62,0,
#                     5000,
#                     prob_cloud = 0.75)
#

#load("/Users/ahalterman/tmp.RData")
#load("/Users/ahalterman/surv.RData")
#
#tmp <- df[,c("name", "death_prob")]
##tmp <- reshape2::melt(tmp, id.vars = c("name", "death_prob"))
#tmp %>% group_by(name) %>% summarize(prob = mean(death_prob)) %>% 
#  ggplot(aes(x = name, y = prob)) + geom_bar(stat = "identity")

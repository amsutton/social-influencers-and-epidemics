#Build diagnostic plots of individual models: baselines (without influencers)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,here,qdapRegex,data.table,ggblend,
               wesanderson,ggpubr,ggrepel)

here::i_am("code/r_code/build_baseline_viz_simulations.R")

#function for viz
process_visualize_simresults = function(dat,string_one,string_two) {
  
  require(tidyverse, data.table, wesanderson)
  
  #how many new infections each step by group (for each sim)
  cdf_newinfections_bygroup =
    dat %>%
    select(id,rep_idx,step,group,n_new_infections) %>%
    distinct %>%
    group_by(id,rep_idx,group) %>%
    mutate(cdf_newinfections = cumsum(n_new_infections),
           group = factor(ifelse(group == 1, 
                                 "early-infected group", 
                                 "late-infected group"))) %>%
    distinct %>%
    mutate(maxcdf = max(cdf_newinfections,na.rm=TRUE)) %>%
    ungroup
  
  cdf_newbehaviors_bygroup = 
    dat %>%
    filter(behavior == 1) %>%
    select(id,rep_idx,step,group,n_behavior) %>%
    distinct() %>%
    group_by(id,rep_idx,group) %>%
    mutate(n_new_behaviors = n_behavior-lag(n_behavior),
           n_new_behaviors = ifelse(is.na(n_new_behaviors),
                                    0,
                                    n_new_behaviors), #fine because the NAs are for the first entry at step 1, which would have zero difference.
           cdf_newbehaviors = cumsum(n_new_behaviors),
           group = factor(ifelse(group == 1, 
                                 "early-infected group", 
                                 "late-infected group"))) %>%
    distinct %>%
    group_by(group) %>%
    mutate(maxcdf_behavior = max(cdf_newbehaviors,na.rm=TRUE)) %>%
    ungroup
  
  #convenience labels
  g1_influencer_message = g1_influencer_message_phrase = "none"
  g2_influencer_message = g2_influencer_message_phrase = "none"
  
  #for calculating the infection rates to peak later
  dat_for_peak = dat %>%
    select(id,rep_idx,step,group,status,n_new_infections) %>%
    filter(status == "I") %>%
    mutate(behavior = "do behavior", #engage in behaviour, i.e., 1 not 0
           status = ifelse(status ==  "I",
                           "infected",
                           "susceptible"),
           group = factor(ifelse(group == 1,
                                 paste0("early-infected group: no influence"),
                                 paste0("late-infected group: no influence")))) %>%
    ungroup() %>%
    select(-status,-behavior)

  dat = dat %>%
    select(id, rep_idx, step:n_behavior, w1:g2_influencer_message,f,ninfluencers) %>%
    filter(
      behavior == 1, #we only want to visualize the behaviour data when behaviour is TRUE
      status == "I") %>%
    group_by(w1,w2,aversion,
             g1_influencer_message,g2_influencer_message,
             f,ninfluencers) %>%
    mutate(behavior = "do behavior",
           status = ifelse(status ==  "I",
                           "infected",
                           "susceptible"),
           group = factor(ifelse(group == 1,
                                 paste0("early-infected group: no influence"),
                                 paste0("late-infected group: no influence")))) %>%
    ungroup()
  
  w1 = unique(dat$w1)
  w2 = unique(dat$w2)
  aversion = unique(dat$aversion)
  f = unique(dat$f)
 
  #calculate the peak estimates
  peak = dat %>%
    group_by(id,rep_idx,group) %>%
    summarize(max_status = max(n_status)) %>%
    group_by(group) %>%
    mutate(peak_n_infections_max = max_status,
           peak_n_infections_median = median(max_status),
           peak_n_infections_lower = quantile(max_status,probs=0.025),
           peak_n_infections_upper = quantile(max_status,probs=0.975))
  
  
  peak_behavior = dat %>%
    group_by(id,rep_idx,group) %>%
    summarize(max_behavior = max(n_behavior)) %>%
    group_by(group) %>%
    mutate(peak_n_behavior_max = max_behavior,
           peak_n_behavior_median=median(max_behavior),
           peak_n_behavior_lower = quantile(max_behavior,probs=0.025),
           peak_n_behavior_upper = quantile(max_behavior,probs=0.975)) 
  
  
  #save these for visualization
  
  fwrite(peak,here(paste0("output/abm_results/summary_results/baselines/",string_two,"_",string_one,"_total_peak_infection_stats.csv")))
  fwrite(peak_behavior,here(paste0("output/abm_results/summary_results/baselines/",string_two,"_",string_one,"_total_peak_behavior_stats.csv")))
  

  #then compare with the no-influence runs, and based on their proportions 
  #we can discuss the effect of the influence
  
  dat_distribution =
    dat %>%
    group_by(step,group) %>%
    distinct %>%
    mutate(
      min_status = as.double(min(n_status)),
      med_status = as.double(median(n_status)),
      max_status = as.double(max(n_status)),
      mean_status = as.double(mean(n_status)),
      q0.025_status  = as.double(quantile(n_status, probs = 0.025)),
     # q0.1_status  = as.double(quantile(n_status, probs = 0.1)),
      q0.5_status  = as.double(quantile(n_status, probs = 0.5)), #same as med_status
      q0.975_status  = as.double(quantile(n_status, probs = 0.975))) %>%
    filter(status == "infected") %>%
    mutate(
      min_behavior = as.double(min(n_behavior)),
      med_behavior = as.double(median(n_behavior)),
      max_behavior = as.double(max(n_behavior)),
      mean_behavior = as.double(mean(n_behavior)),
      q0.025_behavior  = as.double(quantile(n_behavior, probs = 0.025)),
     # q0.1_behavior  = as.double(quantile(n_behavior, probs = 0.1)),
      q0.25_behavior  = as.double(quantile(n_behavior, probs = 0.25)),
      q0.975_behavior  = as.double(quantile(n_behavior, probs = 0.975)),
     # q0.9_behavior  = as.double(quantile(n_behavior, probs = 0.9)),
      q0.75_behavior  = as.double(quantile(n_behavior, probs = 0.75))) %>%
    ungroup() %>%
    select(step,group,min_status:q0.75_behavior) %>%
    distinct()
  
  #labels for homophily and aversion in plots
  homophily_label = case_when(w1 == 0.5 ~ "homophily low (0.5)",
                              w1 == 0.7 ~ "homophily medium (0.7)",
                              w1 == 0.9 ~ "homophily high (0.9)")
  
  aversion_label = case_when(aversion == 0.3 ~ "out-group aversion very low (0.3)",
                             aversion == 0.5 ~ "out-group aversion high (0.9)",
                             aversion == 0.7 ~ "out-group aversion medium (0.7)",
                             aversion == 0.9 ~ "out-group aversion high (0.9)")

  #### VIZ INFECTIONS WITH MEDIAN TREND ####

  #select curve colours directly
  colours = wes_palette(name="Zissou1",
                        type="discrete")
  colours = c(colours[3],colours[1])


  dat %>%
    na.omit() %>%
    filter(status == "infected") %>%
    select(id,rep_idx,step,group,n_status,f) %>% #f used as a convenience for labelling
    mutate(group = as.factor(group)) %>%
    distinct %>%
    ggplot(aes(color=group,
               x = step)) +
    geom_line(aes(group = interaction(id,rep_idx,group),
                  y = (n_status/500)*100),
              alpha = 0.004) +
    geom_line(data= dat_distribution,
              aes(y = (med_status/500)*100,
                  group = as.factor(group)),
              linewidth = 2.25) |> blend("multiply") +
    scale_color_manual(values= colours,
                       aesthetics = c("color")) +
    scale_y_continuous(limits = c(0,40),
                       breaks=seq(0,40,by=20)) +
    scale_x_continuous(limits = c(0,225),
                       breaks=seq(0,225,by=100)) +
    theme_pubclean() +
    theme(legend.position = "none",
          strip.background = element_rect(fill="white",
                                          colour = "black",
                                          linewidth = 0.5),
          text =  element_blank(),
          axis.text = element_blank(),
          # text =  element_text(colour = "black",
          #                      size = 25),
          # axis.text = element_text(colour = "black",
          #                          size = 25),
          axis.ticks = element_line(colour="black",linewidth = 2),
          axis.ticks.length = unit(0.5,"cm"),
          axis.title =  element_blank())

  ggsave(here(paste0("output/abm_results/viz/individual_model_viz/paper_results/infection_curves/baselines/",string_two,"_",string_one,"_results.png")), width = 3, height = 2.5, units = "in", bg="white",dpi = 350)

  #Visualize Behavioral Learning Simulations with Median
  dat %>%
    na.omit() %>%
    select(id,rep_idx,step,group,n_behavior,f) %>%
    group_by(id,rep_idx,step,group) %>%
    mutate(group = as.factor(group),
           f = as.character(f)) %>%
    ggplot(aes(x=step,color=group)) +
    geom_line(aes(group = interaction(id,rep_idx,group),
                  y = (n_behavior/500)*100),
              alpha = 0.004) +
    geom_line(data= dat_distribution,
              aes(y = (med_behavior/500)*100,
                  group = group),
              linewidth = 2.25) |> blend("multiply") +
    scale_color_manual(values= colours,
                       aesthetics = c("color")) +
    scale_y_continuous(limits = c(0,60),
                       breaks = seq(0,60,by=20)) +
    scale_x_continuous(limits = c(0,225),
                       breaks=seq(0,225,by=100)) +
    theme_pubclean() +
    theme(legend.position = "none",
          strip.background = element_rect(fill="white",
                                          colour = "black",
                                          linewidth = 0.5),
          text =  element_blank(),
          axis.text = element_blank(),
          # text =  element_text(colour = "black",
          #                      size = 25),
          # axis.text = element_text(colour = "black",
          #                          size = 25),
          #axis.ticks = element_line(colour="black"),
          axis.ticks = element_line(colour="black",linewidth = 2),
          axis.ticks.length = unit(0.5,"cm"),
          axis.title = element_blank())

  ggsave(here(paste0("output/abm_results/viz/individual_model_viz/paper_results/behaviour_curves/baselines/",string_two,"_",string_one,"_g1_influencer_message=0_g2_influencer_message=0_results.png")), width = 3, height = 2.5, units = "in", bg="white",dpi = 350)


  #get the total infection burden stats: Figure 4b

  #take the infection cdf data by step, and calculate the distribution of that point
  group1_endpoint_cdf = cdf_newinfections_bygroup %>%
    group_by(id,rep_idx) %>%
    filter(str_detect(group, "early-infected"),
           n_new_infections>=4) %>%
    filter(step == max(step))

  group2_endpoint_cdf = cdf_newinfections_bygroup %>%
    group_by(id,rep_idx) %>%
    filter(str_detect(group, "late-infected"),
           n_new_infections>=4) %>%
    filter(step == max(step))

  endpoint_cdf = rbind(group1_endpoint_cdf,group2_endpoint_cdf)
  rm(group1_endpoint_cdf,group2_endpoint_cdf)

  #calculate the percent infected
  endpoint_cdf$pct_group_infected_end_of_curve = endpoint_cdf$cdf_newinfections/500*100

  # Do the same for behavior

  #we will use the same end step as with infections for coherence
  last_step = endpoint_cdf %>%
    select(id,rep_idx,group,step) %>%
    rename(laststep = step)

  endpoint_cdf_behavior =
    left_join(cdf_newbehaviors_bygroup,last_step, by = c("id","rep_idx","group")) %>%
      group_by(id,rep_idx) %>%
      filter(step == laststep)

  #calculate the percent with the behaviour
  endpoint_cdf_behavior$pct_group_behavior_end_of_curve = endpoint_cdf_behavior$cdf_newbehaviors/500*100

 endpoint_cdf =
 left_join(endpoint_cdf,endpoint_cdf_behavior, by =c("id","rep_idx","group","step"))

  fwrite(endpoint_cdf,here(paste0("output/abm_results/summary_results/baselines/",string_two,"_",string_one,"_total_burden_stats.csv")))

  rm(endpoint_cdf,endpoint_cdf_behavior)


  #this is actually a representation of the overall peak distribution,
  #by averaging out over the interval values when there are multiple
  #of the same high median peaks:

  #calculate the rate of infection before the peak

  #get the step at which each run peaks for each group
  step_at_the_peak =
    dat %>%
    filter(status=="infected") %>%
    select(id,rep_idx,group,n_status,step) %>%
    group_by(id,rep_idx,group) %>%
    filter(n_status == max(n_status)) %>%
    filter(step == min(step)) %>% #if there are multiples of the same max # infected, take the earliest peak step
    distinct() %>%
    rename(step_at_the_peak = step)

  if (nrow(step_at_the_peak) != 2000){
    #handle cases where the epidemic doesn't take off at all and there is a
    #group without any infections (the zeroes matter) --
    #this is reflected by there being too few rows!
    #(should be 1000 for each group, i.e. 2000)

    #there should be two observation for
    #each simulation run (i.e. crosstab of id and rep_idx).
    #If there aren't, find them:
    missing_values =
      step_at_the_peak %>%
      ungroup() %>%
      count(id,rep_idx) %>%
      filter(n<2)

    #figure out which ones are missing
    missing_values = left_join(missing_values,step_at_the_peak,by=c("id","rep_idx"))

    #dummy a row for the missing ones
    temp =
      missing_values

    #create the zero intuitively for whichever group is appropriate
    temp =
      temp %>%
      mutate(n_status = 0,
             step_at_the_peak = NA,
             group = case_when(group == 'early-infected group: no influence' ~ 'late-infected group: no influence',
                               group == 'early-infected group: no influence' ~ 'late-infected group: no influence',
                               group == 'late-infected group: no influence' ~ 'early-infected group: no influence',
                               group == 'late-infected group: no influence' ~ 'early-infected group: no influence'))

    #join to step_at_the_peak
    missing_values = rbind(missing_values,temp)

    missing_values =
      missing_values %>%
      mutate(cross_tab = interaction(id,rep_idx))  %>%
      select(-n)

    `%nin%` = Negate(`%in%`)

    step_at_the_peak =
      step_at_the_peak %>%
      ungroup() %>%
      mutate(cross_tab = interaction(id,rep_idx)) %>%
      subset(cross_tab %nin% missing_values$cross_tab)


    step_at_the_peak = rbind(step_at_the_peak,missing_values)

    step_at_the_peak =
      step_at_the_peak %>%
      select(-cross_tab)

  }

  #join step_at_the_peak to cdf_newinfections_bygroup
  step_at_the_peak =
    step_at_the_peak %>%
    rename(group_concordance = group) %>%
    mutate(group = str_extract(group_concordance,"early-infected group|late-infected group"))

  cdf_newinfections_bygroup = left_join(cdf_newinfections_bygroup,step_at_the_peak,by=c('id','rep_idx','group'),relationship = "many-to-many")

  cdf_newinfections_bygroup =
    cdf_newinfections_bygroup %>%
    group_by(id,rep_idx,group) %>%
    filter(step <= step_at_the_peak) %>%
    ungroup()

  #join behavioural cdf data too
  cdf_newbehaviors_bygroup = left_join(cdf_newbehaviors_bygroup,step_at_the_peak,by=c('id','rep_idx','group'),relationship = "many-to-many")
  cdf_newbehaviors_bygroup =
    cdf_newbehaviors_bygroup %>%
    group_by(id,rep_idx,group) %>%
    filter(step <= step_at_the_peak) %>%
    ungroup()


  cdfs_by_group = left_join(cdf_newinfections_bygroup,cdf_newbehaviors_bygroup,by=c('id','step','rep_idx','group','group_concordance','n_status','step_at_the_peak'),relationship = "many-to-many")

  cdfs_by_group =
    cdfs_by_group %>%
    select(id,rep_idx,step,group,group_concordance,
           cdf_newinfections,cdf_newbehaviors,
           maxcdf,maxcdf_behavior,
           step_at_the_peak)

  write.csv(cdfs_by_group, file = here(paste0("output/abm_results/summary_results/baselines/",string_two,"_",string_one,"_peak_general_data.csv")))


  #### Figure 5 stats (log transformation done in figure code) ####

  #NB: the behaviour growth rates aren't the same as the infection growth rates, because
  #we don't have the incident number of new behavioural adoption at each time step in
  #these data (a later consideration, after the model was built and run)

  growth_rate =
    cdfs_by_group %>%
    group_by(id,rep_idx,group) %>%
    #mutate(test = lag(cdf_newinfections)) %>%
    select(group,cdf_newinfections,cdf_newbehaviors) %>%
    mutate(infection_growth_rate = cdf_newinfections - lag(cdf_newinfections),
           behavior_growth_rate = cdf_newbehaviors - lag(cdf_newbehaviors)) %>%
    group_by(group) %>%
    mutate(infection_mean_growth_rate = mean(infection_growth_rate,na.rm=TRUE),
           infection_median_growth_rate = median(infection_growth_rate,na.rm=TRUE),
           infection_lower_ci50_growth_rate = quantile(infection_growth_rate,probs=0.25,na.rm=TRUE),
           infection_upper_ci50_growth_rate = quantile(infection_growth_rate,probs=0.75,na.rm=TRUE),
           infection_lower_ci95_growth_rate = quantile(infection_growth_rate,probs=0.025,na.rm=TRUE),
           infection_upper_ci95_growth_rate = quantile(infection_growth_rate,probs=0.975,na.rm=TRUE),
           behavior_mean_growth_rate = mean(behavior_growth_rate,na.rm=TRUE),
           behavior_median_growth_rate = median(behavior_growth_rate,na.rm=TRUE),
           behavior_lower_ci95_growth_rate = quantile(behavior_growth_rate,probs=0.025,na.rm=TRUE),
           behavior_upper_ci95_growth_rate = quantile(behavior_growth_rate,probs=0.975,na.rm=TRUE),
           behavior_lower_ci50_growth_rate = quantile(behavior_growth_rate,probs=0.25,na.rm=TRUE),
           behavior_upper_ci50_growth_rate = quantile(behavior_growth_rate,probs=0.75,na.rm=TRUE)) %>%
    select(group,infection_mean_growth_rate:behavior_upper_ci50_growth_rate) %>%
    distinct() %>%
    ungroup()

  write.csv(growth_rate, file = here(paste0("output/abm_results/summary_results/baselines/",string_two,"_",string_one,"_growth_rate.csv")))

}


#load model id list
modellist = readRDS(file=here("data/baseline_id_list.RDS"))
modellist = modellist %>% distinct()
modellist = modellist %>%
  ungroup() %>%
  filter(str_detect(string_two,"w1=0.5_w2=0.5") |
           str_detect(string_two,"w1=0.7_w2=0.7") |
           str_detect(string_two,"w1=0.9_w2=0.9"))

#we do not include all aversion = 0.3 scenarios in the paper, 
#so we omit homophily 0.7 and 0.9 for this aversion value,
#while keeping the low homophily, very low aversion described in the results:
modellist = rbind(modellist[1,],modellist[4:12,])


for (i in 1:nrow(modellist)){
  model_filenames <- 
    intersect(
      list.files(
        path = here("data/raw_abm_results/baselines"),
        pattern = paste(modellist$string_one[i]),
        full.names = TRUE),
      list.files(
        path = here("data/raw_abm_results/baselines"), 
        pattern = paste(modellist$string_two[i]), 
        full.names = TRUE) )  
  
  dat = map_dfr(model_filenames, fread, .id = 'id') #id is the csv file (10 per model)
  
  string_one = modellist$string_one[i]
  string_one = substring(string_one,1,12)
  string_two = modellist$string_two[i]
  
  process_visualize_simresults(dat,string_one,string_two)
  rm(dat)
  gc()
}




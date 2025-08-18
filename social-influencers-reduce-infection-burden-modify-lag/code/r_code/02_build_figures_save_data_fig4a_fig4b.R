#### Build Figures 4 and 5 ####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,here,data.table,wesanderson,ggpubr)

here::i_am("code/r_code/01b_analyze_and_visualize_influencer_models.R")


#### Build Figure 4a + save data as CSV ####
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/experiment"),
  pattern = "basic_behavior_healthstat_statistics", 
  full.names = TRUE)  

model_filenames = subset(model_filenames,subset = str_detect(model_filenames,"basic_behavior"))

#we do not want either of these in these figures
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.3"))
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.1"))

#trim according to your directory needs
string_one = substring(model_filenames,107,118) # should look like: e.g., "aversion=0.7"
string_two = substring(model_filenames,93,105) # should look like: e.g., "w1=0.5_w2=0.5"

dat = map_dfr(model_filenames, fread, .id = 'id')

dat = dat %>%
  mutate(homophily = rep(string_two,each=2),
         aversion = rep(string_one,each=2)) %>%
  mutate(aversion = str_replace_all(aversion,"aversion","a"),
         homophily = substring(homophily,10,13),
         homophily = paste0("h",homophily),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())

#read in baselines for comparison
model_filenames2  <- list.files(
  path = here("output/abm_results/summary_results/baselines"),
  pattern = "basic_behavior", 
  full.names = TRUE)  

model_filenames2 = subset(model_filenames2,subset = !str_detect(model_filenames2,"aversion=0.3"))

string_one = substring(model_filenames2,106,117)
string_two = substring(model_filenames2,92,104)

dat2 = map_dfr(model_filenames2, fread, .id = 'id')

dat2 =
  dat2 %>%
  mutate(homophily = rep(string_two,each=2),
         aversion = rep(string_one,each=2)) %>%
  mutate(aversion = str_replace_all(aversion,"aversion","a"),
         homophily = paste0("h",substring(homophily,10,13)),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())

dat2$label = "Scenario 1" #no influence
dat2$group = str_remove_all(dat2$group, ": no influence")

#a figure describing the time to peak by group and specs
#x: step, y: 

temp = dat %>%
  group_by(id) %>%
  mutate(label = case_when(str_detect(group,"late-infected group: health-protective") ~ "Scenario 3",
                           str_detect(group,"late-infected group: anti-protective") ~ "Scenario 2",
                           str_detect(group,"early-infected group: health-protective") ~ "Scenario 2",
                           str_detect(group,"early-infected group: anti-protective") ~ "Scenario 3")) %>%
  ungroup()

temp$group = str_remove_all(temp$group, ": health-protective|: anti-protective")

dat2 = dat2 %>%
  mutate(aversion = str_replace_all(aversion,"a=","out-group\naversion\n"),
         homophily = str_replace_all(homophily,"h=",""))
temp$aversion = str_replace_all(dat$aversion,"a=","out-group\naversion\n")

#difference in peak infection timing by h and a, 
#and by which group is influenced to do what

value_label = c("0.5" = "low (0.5)",
                "0.7" = "medium (0.7)",
                "0.9" = "high (0.9)")

colours3 = c(wes_palette(name="Rushmore1",
                         type="discrete")[4],
             wes_palette(name="Zissou1",
                         type="discrete")[1],
             wes_palette(name="Darjeeling1",
                         type="discrete")[4]
)


#Comparison of the median number of infections at the peak of the infection 
#curve by group, and then broken down by homophily and aversion, 
#according to which group is told to mask vs not mask. (95% and 90% whiskers)

dat2 = as_tibble(dat2)

dat2 = dat2 %>%
  mutate(label = factor(label, levels=c('Scenario 1',
                                        'Scenario 2',
                                        'Scenario 3')))

temp = temp %>%
  mutate(label = factor(label, levels=c('Scenario 1',
                                        'Scenario 2',
                                        'Scenario 3')))

dat2 = dat2 %>%  #"no influence" data
  group_by(specs,label) %>%
  mutate(peak_n_infections_median = sum(peak_n_infections_median)/1000*100,
         peak_n_infections_lower = sum(peak_n_infections_lower)/1000*100,
         peak_n_infections_upper = sum(peak_n_infections_upper)/1000*100,
         aversion = str_replace_all(aversion,"out-group\naversion\n","")) %>%
  ungroup() %>%
  select(homophily,aversion,label,peak_n_infections_median:peak_n_infections_upper) %>%
  distinct()

temp2 = 
  temp %>%
  #  group_by(homophily,aversion,label) %>% 
  select(id,group,homophily,aversion,peak_n_infections_median:peak_n_infections_upper,label) %>%
  mutate(
    aversion = str_replace_all(aversion,"out-group\naversion\n",""),
    homophily = str_replace_all(homophily,"h=","")) %>%
  group_by(homophily,aversion,label) %>%
  mutate(sum = sum(peak_n_infections_median),
         peak_n_infections_median = sum(peak_n_infections_median)/1000*100,
         peak_n_infections_lower = sum(peak_n_infections_lower)/1000*100,
         peak_n_infections_upper = sum(peak_n_infections_upper)/1000*100) %>%
  select(-group,-id,-sum) %>% 
  distinct()

temp2 = temp2 %>% ungroup() %>% select(colnames(dat2))

temp2 = rbind(temp2,dat2)

fig4_csv_data = 
  temp2 %>% 
  arrange(label) %>%
  mutate(estimate = as.character(paste0(round(peak_n_infections_median,1)," (",round(peak_n_infections_upper,1),", ",round(peak_n_infections_lower,1),")"))) %>%
  select(homophily:label,estimate) %>%
  group_by(homophily,aversion) %>%
  pivot_wider(names_from = "label",values_from = "estimate")

write.csv(fig4_csv_data, file=here("output/abm_results/summary_results/paper_tables/figure4_data_with_95ci_intervals.csv"))


temp2 %>%
  ungroup() %>%
  #  group_by(homophily,aversion,label) %>% 
  #filter(label == "Scenario 2") %>%
  ggplot(aes(x=homophily, 
             y=peak_n_infections_median, 
             colour = label, 
             group = aversion,
             linetype = aversion)) +
  #  geom_line(data = dat2 %>% ungroup()) +
  geom_line(linewidth=1) +
  # geom_line(data= temp2 %>%
  #            filter(label == "Scenario 3")) +
  labs(y="percent of total population",
       # title = "Population Infected at Epidemic Curve Peak",
       x = "homophily",
       linetype = "out-group aversion: ",
       color = "") +
  scale_color_manual(values= c(wes_palette(name="Rushmore1",
                                           type="discrete")[4],
                               wes_palette(name="Darjeeling1",
                                           type="discrete")[4],
                               wes_palette(name="Zissou1",
                                           type="discrete")[1]),
                     aesthetics = c("color"),
                     labels = scales::label_wrap(40)) +
  scale_y_continuous(limits = c(15,40), breaks=seq(15,40,by=5)) +
  scale_x_discrete(labels = value_label) +
  scale_linetype_discrete(labels = value_label) +
  scale_shape_manual(values = c(5,1,8)) +
  theme_pubclean() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "center",
        panel.spacing.y = unit(2, "lines"),
        panel.spacing.x = unit(2, "lines"),
        legend.location = "plot",
        legend.text = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "grey60",
                                          linewidth = 0.15),
        strip.background = element_rect(fill = "white"),
        strip.text.y = element_text(angle = 0),
        legend.box="vertical",
        legend.margin=margin()) +
  guides(colour = "none") +
  facet_wrap(~label)#,labeller=labeller(label=label_wrap_gen(35)))



ggsave(here(paste0("output/abm_results/viz/paper_figures/fig4a.png")), width = 9, height =4, units = "in", bg="white",dpi = 500)


#### Build Figure 4b + save as CSV ####
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/experiment"), #/f5_results
  #pattern = paste(modellist$string_one), 
  full.names = TRUE)  

model_filenames = subset(model_filenames,subset = str_detect(model_filenames,"__total_infection"))
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.3"))
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.1"))

dat = map_dfr(model_filenames, fread, .id = 'id')

model_filenames = substring(model_filenames,93,170)

dat = dat %>%
  mutate(homophily = rep(substring(model_filenames,4,6),each=10),
         aversion =  rep(substring(model_filenames,24,26),each=10),
         influence = rep(substring(model_filenames,32,78),each=10)) %>%
  mutate(aversion = paste0("a=",aversion),
         homophily = paste0("h=",homophily),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())

#read in baselines for comparison
model_filenames2  <- list.files(
  path = here("output/abm_results/summary_results/baselines"), #/f5_results
  #pattern = "*.csv", 
  full.names = TRUE)  

model_filenames2 = subset(model_filenames2,subset = str_detect(model_filenames2,"infection"))
model_filenames2 = subset(model_filenames2,subset = !str_detect(model_filenames2,"aversion=0.3"))

dat2 = map_dfr(model_filenames2, fread, .id = 'id')

model_filenames2 = substring(model_filenames2,92,117)

dat2 = dat2 %>%
  mutate(homophily = rep(substring(model_filenames2,4,6),each=10),
         aversion =  rep(substring(model_filenames2,24,26),each=10),
         influence = "no influence") %>%
  mutate(aversion = paste0("a=",aversion),
         homophily = paste0("h=",homophily),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,-influence,everything())


#a figure describing the time to peak by group and specs x: step, y: 
dat = dat %>%
  mutate(label = case_when(str_detect(influence,"g1_influencer_message=0_g2_influencer_message=1") ~ "Scenario 3",
                           str_detect(influence,"g1_influencer_message=1_g2_influencer_message=0") ~ "Scenario 2",
                           str_detect(influence,"g1_influencer_message=0_g2_influencer_message=1") ~ "Scenario 2",
                           str_detect(influence,"g1_influencer_message=1_g2_influencer_message=0") ~ "Scenario 3"))

dat2$label = dat2$influence

dat = rbind(dat,dat2)

temp = dat %>%
  ungroup() %>%
  select(-count,-measure,-pct_infected_end_of_curve,-pct_difference) %>% #
  group_by(group,step_end_of_curve,aversion,homophily,label) %>%
  distinct() %>%
  pivot_wider(names_from = group,values_from = step_end_of_curve) %>%
  mutate(step_difference= (`late-infected group`/`early-infected group`*100)-100) %>%
  select(-`late-infected group`,-`early-infected group`)

temp2 = dat %>%
  ungroup() %>%
  select(group,specs,aversion,homophily,label,count) %>% #
  group_by(group,specs,aversion,homophily,label) %>%
  distinct() %>%
  reframe(q0.025_status = as.double(quantile(probs = 0.025,count)),
          q0.1_status = as.double(quantile(probs = 0.1,count)),
          med_status = as.double(quantile(probs = 0.5,count)),
          q0.975_status = as.double(quantile(probs = 0.975,count)),
          q0.9_status = as.double(quantile(probs = 0.9,count)))

temp2 = dat %>%
  ungroup() %>%
  select(group,specs,aversion,homophily,label,count) %>% #
  group_by(group,specs,aversion,homophily,label) %>%
  distinct() %>%
  reframe(q0.025_status = as.double(quantile(probs = 0.025,count)),
          q0.1_status = as.double(quantile(probs = 0.1,count)),
          med_status = as.double(quantile(probs = 0.5,count)),
          q0.975_status = as.double(quantile(probs = 0.975,count)),
          q0.9_status = as.double(quantile(probs = 0.9,count))) %>%
  group_by(specs,aversion,homophily,label) %>%
  reframe(q0.025_status = sum(q0.025_status)/1000*100,
          q0.1_status = sum(q0.1_status)/1000*100,
          med_status = sum(med_status)/1000*100,
          q0.975_status = sum(q0.975_status)/1000*100,
          q0.9_status = sum(q0.9_status)/1000*100) %>%
  ungroup()

fig4b_csv_data = temp2 %>%
  mutate(label = ifelse(label == "no influence","Scenario 1", label),
         homophily = str_remove_all(homophily,"h="),
         aversion = str_remove_all(aversion,"a="),
         estimate = as.character(paste0(round(med_status,1)," (",round(q0.025_status,1),", ",round(q0.975_status,1),")"))) %>%
  arrange(label) %>%
  select(homophily,aversion,label,estimate) %>% 
  pivot_wider(names_from = "label",values_from="estimate")


write.csv(fig4b_csv_data,file = here('/Users/Aja/Julia Directories/social_influence/output/abm_results/summary_results/paper_tables/figure4b_data_with_95ci_intervals.csv'))


temp2 = temp2 %>%
  distinct()

temp2 %>%
  group_by(homophily,aversion,label) %>%
  mutate(label = ifelse(label == "no influence","Scenario 1",label)) %>%
  #filter(label == "Scenario 2") %>%
  select(homophily,aversion,med_status,q0.025_status:q0.9_status,label) %>%
  mutate(
    aversion = str_replace_all(aversion,"a=",""),
    homophily = str_replace_all(homophily,"h=",""),
    label = str_replace_all(label,"influence message: \n",""),
    label = factor(label, levels=c('Scenario 1',
                                   'Scenario 2',
                                   'Scenario 3'))) %>%
  group_by(homophily,aversion,label) %>%
  #mutate(q0.5_status = med_status) %>%
  distinct() %>%
  ggplot(aes(x=homophily,
             y=med_status,
             colour = label,
             linetype=aversion,
             group = aversion)) +
  #mutate(q0.5_status = med_status)) +
  geom_line(data= temp2 %>%
              group_by(homophily,aversion,label) %>%
              filter(label == "Scenario 3") %>%
              mutate(
                aversion = str_replace_all(aversion,"a=",""),
                homophily = str_replace_all(homophily,"h=",""),
                label = str_replace_all(label,"influence message: \n",""),
                label = factor(label, levels=c('Scenario 1',
                                               'Scenario 2',
                                               'Scenario 3'))) %>%
              group_by(homophily,aversion,label),
            linewidth = 1) +
  geom_line(linewidth = 1) +
  # geom_pointrange(aes(ymin=q0.025_status,
  #                     ymax=q0.975_status),
  #                 size =-3) +
  labs(y="percent of total population",
       # title = "Population Infected at\nEpidemic Curve Peak",
       x = "homophily",
       color = "",
       linetype = "out-group aversion: ") +
  scale_color_manual(values= c("#3B9AB2","#35274A","#F98400"),
                     aesthetics = c("color"),
                     labels = scales::label_wrap(40)) +
  scale_y_continuous(limits = c(75,100), breaks=seq(75,100,by=5)) +
  scale_x_discrete(labels = value_label) +
  scale_linetype_discrete(labels = value_label) +
  theme_pubclean() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "center",
        panel.spacing.y = unit(2, "lines"),
        panel.spacing.x = unit(2, "lines"),
        legend.location = "plot",
        panel.grid.major.y = element_line(color = "grey60",
                                          linewidth = 0.15),
        strip.background = element_rect(fill = "white"),
        strip.text.y = element_text(angle = 0),
        legend.text = element_text(hjust = 0.5),
        legend.box="vertical",
        legend.margin=margin()) +
  guides(linetype = guide_legend(order=1),
         color = "none") +
  facet_grid(~label,labeller=labeller(label=label_wrap_gen(35)))


ggsave(here(paste0("output/abm_results/viz/paper_figures/fig4b.png")), width = 9, height =4, units = "in", bg="white",dpi = 500)






#### Build Figure 5: Log-mean Growth Rates Comparison ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,here,data.table,wesanderson,stringr,ggpubr,ggrepel)

here::i_am("code/r_code/03_build_figure_save_data_fig5.R")


#Combine growth rates for Scenarios 1, 2 and 3

#Scenarios 2 and 3
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/experiment/"), 
  pattern = paste0("growth_rate.csv"),
  full.names = TRUE)

dat =  map_dfr(model_filenames, fread, .id = 'id')

strings = str_sub(model_filenames, start=-94)
dat$file = rep(strings,each =2)
dat$homophily = str_sub(dat$file,start=4,end=6) #format: "0.7"
dat$aversion = str_sub(dat$file,start=24,end=26)#format: "0.7"
dat$influencers = TRUE
rm(strings)

#log-transform data
dat$log_mean_growth_rate = log(dat$infection_mean_growth_rate)
dat$log_lower_growth_rate = log(dat$infection_lower_ci50_growth_rate)
dat$log_upper_growth_rate = log(dat$infection_upper_ci50_growth_rate)

dat = dat %>%
  mutate(message = ifelse(str_detect(file,"g1_influencer_message=0") == TRUE,
                          "Scenario 3",
                          "Scenario 2")) %>%
  select(homophily,aversion,message,group,log_mean_growth_rate,log_upper_growth_rate,log_lower_growth_rate)

with_influencers = dat

### Scenario 1 ###
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/baselines/"), 
  pattern = paste0("growth_rate.csv"),
  full.names = TRUE)

dat =  map_dfr(model_filenames, fread, .id = 'id')

strings = str_sub(model_filenames, start=-42)
dat$file = rep(strings,each =2)
dat$homophily = str_sub(dat$file,start=4,end=6) #format: "0.7"
dat$aversion = str_sub(dat$file,start=24,end=26)#format: "0.7"
rm(strings)

dat$influencers = FALSE
dat$log_mean_growth_rate = log(dat$infection_mean_growth_rate)
dat$log_lower_growth_rate = log(dat$infection_lower_ci50_growth_rate)
dat$log_upper_growth_rate = log(dat$infection_upper_ci50_growth_rate)

dat = dat %>%
  mutate(message = "Scenario 1") %>%
  select(homophily,aversion,message,group,log_mean_growth_rate,log_upper_growth_rate,log_lower_growth_rate)

dat = rbind(dat,with_influencers)

dat2 = dat %>%
  mutate(logmean = paste0(signif(log_mean_growth_rate,3),
                          " (",
                          signif(log_upper_growth_rate,3),
                          ", ",
                          signif(log_lower_growth_rate,3),
                          ")")) %>%
  select(-log_mean_growth_rate,-log_lower_growth_rate,-log_upper_growth_rate) %>%
  pivot_wider(.,id_cols = c("homophily","aversion","group"),names_from = "message",values_from = "logmean")
  
write.csv(dat2,file=here('output/abm_results/summary_results/paper_tables/supp_table1.csv'))

#tidy up, remove: aversion = 0.3
dat = dat %>%
  filter(aversion != "0.3")


#### Build figure 5 ####

homophily_label = c(homophily == 0.5 ~ "homophily low (0.5)",
                    homophily == 0.7 ~ "homophily medium (0.7)",
                    homophily == 0.9 ~ "homophily high (0.9)")

aversion_label = c(aversion == 0.3 ~ "out-group aversion very low (0.3)",
                   aversion == 0.5 ~ "out-group aversion high (0.9)",
                   aversion == 0.7 ~ "out-group aversion medium (0.7)",
                   aversion == 0.9 ~ "out-group aversion high (0.9)")


value_label = c("0.5" = "low\n(0.5)",
                "0.7" = "medium\n(0.7)",
                "0.9" = "high\n(0.9)")
summary(dat$log_upper_growth_rate)
dat %>%
  mutate(aversion =factor(aversion),
         homophily = factor(homophily),
         label = factor(message,levels = c("Scenario 1",
                                           "Scenario 2",
                                           "Scenario 3"))) %>%
  group_by(group,homophily,aversion,label) %>%
  select(homophily,aversion,group,log_mean_growth_rate,log_lower_growth_rate,log_upper_growth_rate,label) %>%
  ungroup() %>% 
  ggplot(aes(x=homophily, 
             y=log_mean_growth_rate, 
             colour = aversion,
             group = aversion)) +
  geom_pointrange(aes(y=log_mean_growth_rate,
                      ymin = log_lower_growth_rate,
                      ymax = log_upper_growth_rate),
                      linewidth = 1,
            position=position_dodge(0.5)) + 
  labs(y="Log-Mean Infection Growth Rate (IQR)",
            x = "homophily (β)",
            color = "out-group aversion (α):") +
  scale_color_manual(values= c(wes_palette(name="Rushmore1",
                                           type="discrete")[4],
                               wes_palette(name="Darjeeling1",
                                           type="discrete")[4],
                               wes_palette(name="Zissou1",
                                           type="discrete")[1]),
                     aesthetics = c("color"),
                     labels = value_label) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,by=0.5)) +
  scale_x_discrete(labels = value_label) +
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
        axis.text = element_text(size = 11, color = "black"),
        strip.text = element_text(size = 11, color = "black"),
        legend.margin=margin()) +
  guides(color = guide_legend(order=1)) +
  facet_grid(group~label,labeller = labeller(label=as_labeller(value_label)))



ggsave(here(paste0("output/abm_results/viz/paper_figures/fig5.png")), width = 11, height =5, units = "in", bg="white",dpi = 500)




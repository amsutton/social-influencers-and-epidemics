

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,here,data.table,wesanderson,ggpubr,ggrepel)

here::i_am("code/r_code/03_build_figure_save_data_fig5.R")


#Combine growth rates for Scenarios 1, 2 and 3

#Scenarios 2 and 3
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/experiment/"), #/f5_results
  pattern = paste0("growth_rate.csv"),
  full.names = TRUE)

dat =  map_dfr(model_filenames, fread, .id = 'id')

dat$file = rep(model_filenames,each =2)
dat$homophily = substring(dat$file,97,99) #format: "0.7"
dat$aversion = substring(dat$file,117,119) #format: "0.7"
dat$influencers = TRUE

#log-transform data
dat$log_mean_growth_rate = log(dat$mean_growth_rate)

dat = dat %>%
  mutate(message = ifelse(str_detect(file,"g1_influencer_message=0") == TRUE,
                          "early-infected: anti-protective, late-infected: health-protective",
                          "early-infected: health-protective, late-infected: anti-protective")) %>%
  select(homophily,aversion,message,group,log_mean_growth_rate,mean_growth_rate)

with_influencers = dat

### Scenario 1 ###

model_filenames <- list.files(
  path = here("output/abm_results/summary_results/baselines/"), #/f5_results
  pattern = paste0("growth_rate.csv"),
  full.names = TRUE)

dat =  map_dfr(model_filenames, fread, .id = 'id')

dat$file = rep(model_filenames,each =2)

dat$homophily = substring(dat$file,96,98)
dat$aversion = substring(dat$file,116,118)
dat$influencers = FALSE
dat$log_mean_growth_rate = log(dat$mean_growth_rate)

dat = dat %>%
  mutate(message = "no influence") %>%
  select(homophily,aversion,message,group,log_mean_growth_rate,mean_growth_rate)

dat = rbind(with_influencers,dat)

dat = 
  dat %>%
  group_by(homophily,aversion,message,group) %>%
  select(-mean_growth_rate) %>%
  pivot_wider(names_from = "message",values_from = "log_mean_growth_rate") %>%
  ungroup() %>%
  mutate(diff_noinfluence_vs_earlyconcordant = `early-infected: health-protective, late-infected: anti-protective`-`no influence`,
         diff_noinfluence_vs_earlydiscordant = `early-infected: anti-protective, late-infected: health-protective`-`no influence`)

write.csv(dat, file = here("output/abm_results/summary_results/paper_tables/log_growth_rates_firsthalf_epidemic.csv"))


#### Build figure 5 ####

colours3_alt = c(wes_palette(name="GrandBudapest1",
                             type="discrete")[1:3])

homophily_label = c(homophily == 0.5 ~ "homophily low (0.5)",
                    homophily == 0.7 ~ "homophily medium (0.7)",
                    homophily == 0.9 ~ "homophily high (0.9)")

aversion_label = c(aversion == 0.3 ~ "out-group aversion very low (0.3)",
                   aversion == 0.5 ~ "out-group aversion high (0.9)",
                   aversion == 0.7 ~ "out-group aversion medium (0.7)",
                   aversion == 0.9 ~ "out-group aversion high (0.9)")

dat = dat %>%
 pivot_longer(cols = `early-infected: anti-protective, late-infected: health-protective`:`no influence`, names_to = "message",values_to = "mean_growth_rate")

dat = dat %>%
  mutate(message = case_when(message == "no influence" ~ "Scenario 1",
                             message == "early-infected: anti-protective, late-infected: health-protective" ~ "Scenario 3",
                             message == "early-infected: health-protective, late-infected: anti-protective" ~ "Scenario 2"))


value_label = c("0.5" = "low (0.5)",
                "0.7" = "medium (0.7)",
                "0.9" = "high (0.9)")


dat %>%
  mutate(aversion =factor(aversion),
         homophily = factor(homophily),
         label = factor(message,levels = c("Scenario 1",#"no influence",
                                           "Scenario 2",#"early-infected: anti-protective, late-infected: health-protective",
                                           "Scenario 3"#"early-infected: health-protective, late-infected: anti-protective"
         ))) %>%
  filter(aversion != 0.3) %>%
  group_by(group,homophily,aversion,label) %>%
  #na.omit() %>%
  select(homophily,aversion,group,mean_growth_rate,label) %>%
  #distinct() %>% 
  #filter(aversion == "medium") %>%
  ungroup() %>%
  ggplot(aes(x=homophily, 
             y=mean_growth_rate, 
             colour = aversion,
             group = aversion)) +
  geom_line(linewidth = 1,
            position=position_dodge(0.2)) + 
  # geom_pointrange(aes(ymin = lower_infection_rate_prepeak,
  #                     ymax= upper_infection_rate_prepeak),
  #           linewidth = 0.5,
  #           position=position_dodge(0.2)) + 
  labs(y="log-mean growth rate",
       x = "homophily",
       color = "out-group aversion: ") +
  scale_color_manual(values= colours3_alt,
                     aesthetics = c("color"),
                     labels = scales::label_wrap(40)) +
  scale_y_continuous(limits=c(1.25,2.05),breaks=seq(1.4,2,by=0.2)) +
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
        legend.margin=margin()) +
  guides(color = guide_legend(order=1)) +
  facet_grid(group~label)



ggsave(here(paste0("output/abm_results/viz/paper_figures/fig5.png")), width = 11, height =5, units = "in", bg="white",dpi = 500)




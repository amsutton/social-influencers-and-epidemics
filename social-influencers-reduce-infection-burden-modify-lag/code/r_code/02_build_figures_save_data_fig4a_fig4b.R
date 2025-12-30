#### Build Figures 4a and 4b####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,here,stringr,data.table,wesanderson,ggpubr,ggbreak)

here::i_am("code/r_code/01b_analyze_and_visualize_influencer_models.R")

#helper:
rep_each <- function(x, times) {
  if (length(times) == 1) {
    rep(x, each = times)
  } else if (length(times) == length(x)) {
    rep(x, times = times)
  } else {
    stop('`times` must be length 1 or the same length as `x`')
  }
}

#### Build Figure 4a + save data as CSV ####
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/experiment"),
  pattern = "total_peak_infection_stats", 
  full.names = TRUE)  

#we do not want either of these aversion simulation specs in these figures
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.3"))
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.1"))

strings = str_sub(model_filenames, start =-110)
string_one = str_sub(strings,start=15, end=26) # should look like: e.g., "aversion=0.7"
string_two = str_sub(strings,end=13) # should look like: e.g., "w1=0.5_w2=0.5"
rm(strings)

dat = map_dfr(model_filenames, fread, .id = 'model_id')

length = dat %>% 
  select(model_id) %>% 
  group_by(model_id) %>% 
  summarise(length = n()) %>% 
  select(length) %>% 
  as.list()


dat = dat %>%
  mutate(homophily = rep_each(string_two,times=c(length$length)),
         aversion = rep_each(string_one,times=c(length$length))) %>% 
  mutate(aversion = str_replace_all(aversion,"aversion","a"),
         homophily = substring(homophily,10,13),
         homophily = paste0("h",homophily),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())

#read in baselines for comparison
model_filenames2  <- list.files(
  path = here("output/abm_results/summary_results/baselines"),
  pattern = "total_peak_infection_stats", 
  full.names = TRUE)  

model_filenames2 = subset(model_filenames2,subset = !str_detect(model_filenames2,"aversion=0.3"))

strings = str_sub(model_filenames2, start =-56)
string_one = str_sub(strings,start=15, end=26) # should look like: e.g., "aversion=0.7"
string_two = str_sub(strings,end=13) # should look like: e.g., "w1=0.5_w2=0.5"
rm(strings)

dat2 = map_dfr(model_filenames2, fread, .id = 'model_id')

length = dat2 %>% 
  select(model_id) %>% 
  group_by(model_id) %>% 
  summarise(length = n()) %>% 
  select(length) %>% 
  as.list()

dat2 =
  dat2 %>%
  mutate(homophily = rep_each(string_two,times=c(length$length)),
         aversion = rep_each(string_one,times=c(length$length))) %>% 
  mutate(aversion = str_replace_all(aversion,"aversion","a"),
         homophily = paste0("h",substring(homophily,10,13)),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())

dat2$label = "Scenario 1" #no influence
dat2$group = str_remove_all(dat2$group, ": no influence")

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


value_label = c("0.5" = "low\n(0.5)",
                "0.7" = "medium\n(0.7)",
                "0.9" = "high\n(0.9)")

colours3 = c(wes_palette(name="Rushmore1",
                         type="discrete")[4],
             wes_palette(name="Zissou1",
                         type="discrete")[1],
             wes_palette(name="Darjeeling1",
                         type="discrete")[4]
)


#Comparison of the median number of infections at the peak of the infection 
#curve by group, and then broken down by homophily and aversion, 
#according to which group is told to mask vs not mask. IQR whiskers.

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
  group_by(model_id,id,rep_idx,specs,label) %>%
  mutate(peak_n_infections_max = sum(peak_n_infections_max)/1000*100,
         aversion = str_replace_all(aversion,"out-group\naversion\n","")) %>%
  ungroup() %>%
  select(homophily,aversion,label,peak_n_infections_max) %>%
  distinct()

temp2 = 
  temp %>%
  #  group_by(homophily,aversion,label) %>% 
  select(model_id,id,rep_idx,group,homophily,aversion,max_status,label) %>%
  mutate(
    aversion = str_replace_all(aversion,"out-group\naversion\n",""),
    homophily = str_replace_all(homophily,"h=","")) %>%
  group_by(model_id,id,rep_idx,homophily,aversion,label) %>%
  mutate(peak_n_infections_max = sum(max_status)/1000*100) %>%
  select(-group) %>% 
  distinct()

temp2 = temp2 %>% ungroup() %>% select(colnames(dat2))

temp2 = rbind(temp2,dat2)

temp2 = temp2 %>%
  mutate(aversion = factor(case_when(aversion == "0.5" ~ "low\n(0.5)",
                                     aversion == "0.7" ~ "medium\n(0.7)",
                                     aversion == "0.9" ~ "high\n(0.9)")))

value_label2 = c("0.5" = "homophily\nlow\n(0.5)",
                 "0.7" = "homophily\nmedium\n(0.7)",
                 "0.9" = "homophily\nhigh\n(0.9)")

iqr = temp2 %>%
  group_by(label,homophily,aversion) %>%
  summarise(median = median(peak_n_infections_max),
            q1 = quantile(peak_n_infections_max,probs=0.25),
            q3 = quantile(peak_n_infections_max,probs=0.75))


temp2 %>%
  mutate(aversion = factor(aversion, levels=c("low\n(0.5)",
                                               "medium\n(0.7)",
                                               "high\n(0.9)"))) %>%
  ungroup() %>%
  ggplot(aes(x=homophily, 
             y=peak_n_infections_max, 
             colour = aversion, 
             group = aversion)) +
  geom_jitter(position = position_dodge(0.6),
              size = 0.6,
              alpha = 0.2,
              shape = 19) +
  geom_errorbar(data = iqr %>%
                  mutate(aversion = factor(aversion, levels=c("low\n(0.5)",
                                                              "medium\n(0.7)",
                                                              "high\n(0.9)"))),
             aes(y = median,
                 ymin = q1,
                 ymax = q3),
             position = position_dodge(0.6),
             width = 0.5) +
  geom_point(data = iqr %>%
                  mutate(aversion = factor(aversion, levels=c("low\n(0.5)",
                                                              "medium\n(0.7)",
                                                              "high\n(0.9)"))),
                aes(y = median),
             size = 2,
                position = position_dodge(0.6)) +
  labs(y="Percent Total Population Infected",
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
  #scale_y_continuous(limits = c(14.5,42), breaks=seq(15,45,by=5)) +
  scale_x_discrete(labels = value_label) +
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
        axis.text = element_text(size = 11, color = "black"),
        strip.text = element_text(size = 11, color = "black"),
        legend.margin=margin()) +
  facet_wrap(~label)

ggsave(here(paste0("output/abm_results/viz/paper_figures/final_edit_figures/fig4a.png")), width = 9, height =4, units = "in", bg="white",dpi = 500)

#save iqr for supplementary materials table 2
iqr=
  iqr %>%
  mutate(iqr = paste0(signif(median,3)," (",signif(q1,3),", ",signif(q3,3),")")) %>%
  select(-median,-q1,-q3) %>%
  pivot_wider(id_cols = c("homophily","aversion"),names_from = "label",values_from = "iqr")

write.csv(iqr,file=here('output/abm_results/summary_results/paper_tables/supp_table2_part1.csv'))


#### Build Figure 4b + save as CSV ####
model_filenames <- list.files(
  path = here("output/abm_results/summary_results/experiment"), 
  pattern = "burden", 
  full.names = TRUE)  

model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.3"))
model_filenames = subset(model_filenames,subset = !str_detect(model_filenames,"aversion=0.1"))

dat = map_dfr(model_filenames, fread, .id = 'model_id')

strings = str_sub(model_filenames, start = -102)
string_one = str_sub(strings,start=15,end=26) # should look like: e.g., "aversion=0.7"
string_two = str_sub(strings,end=13) # should look like: e.g., "w1=0.5_w2=0.5"

model_filenames = substring(model_filenames,93,170)

length = dat %>% 
  select(model_id) %>% 
  group_by(model_id) %>% 
  summarise(length = n()) %>% 
  select(length) %>% 
  as.list()

dat = dat %>%
  mutate(homophily = rep_each(string_two,times=c(length$length)),
         aversion = rep_each(string_one,times=c(length$length)),
         influence = rep_each(substring(model_filenames,32,78),times=c(length$length))) %>% 
  mutate(aversion = str_replace_all(aversion,"aversion","a"),
         homophily = substring(homophily,10,13),
         homophily = paste0("h",homophily),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())


#read in baselines for comparison
model_filenames2  <- list.files(
  path = here("output/abm_results/summary_results/baselines"), 
  pattern = "burden", 
  full.names = TRUE)  

model_filenames2 = subset(model_filenames2,subset = !str_detect(model_filenames2,"aversion=0.3"))

dat2 = map_dfr(model_filenames2, fread, .id = 'model_id')

strings = str_sub(model_filenames2,start=-49)
string_one = str_sub(strings,start=15,end=26) # should look like: e.g., "aversion=0.7"
string_two = str_sub(strings,end=13) # should look like: e.g., "w1=0.5_w2=0.5"


length = dat2 %>% 
  select(model_id) %>% 
  group_by(model_id) %>% 
  summarise(length = n()) %>% 
  select(length) %>% 
  as.list()

dat2 = dat2 %>%
  mutate(homophily = rep_each(string_two,times=c(length$length)),
         aversion = rep_each(string_one,times=c(length$length)),
         influence = "Scenario 1") %>% 
  mutate(aversion = str_replace_all(aversion,"aversion","a"),
         homophily = substring(homophily,10,13),
         homophily = paste0("h",homophily),
         specs = paste0(homophily,", ",aversion)) %>%
  select(specs,everything())

#a figure describing the time to peak by group and specs x: step, y: 
dat = dat %>%
  mutate(label = case_when(str_detect(influence,"g1_influencer_message=0_g2_influencer_message=1") ~ "Scenario 3",
                           str_detect(influence,"g1_influencer_message=1_g2_influencer_message=0") ~ "Scenario 2",
                           str_detect(influence,"g1_influencer_message=0_g2_influencer_message=1") ~ "Scenario 2",
                           str_detect(influence,"g1_influencer_message=1_g2_influencer_message=0") ~ "Scenario 3"))

dat2$label = dat2$influence

dat = rbind(dat,dat2)


dat = dat %>%
  mutate(aversion = str_remove_all(aversion,"a="),
         homophily = str_remove_all(homophily,"h=")) %>%
 mutate(aversion = case_when(aversion == "0.5" ~ "low\n(0.5)",
                                    aversion == "0.7" ~ "medium\n(0.7)",
                                    aversion == "0.9" ~ "high\n(0.9)")) 

#being picky, so separately 
dat = dat %>%
  mutate(aversion = factor(aversion, levels=c("low\n(0.5)",
                                              "medium\n(0.7)",
                                              "high\n(0.9)")))

#get the IQR and median for each label, homophily, aversion value combination

dat = dat %>%
  select(model_id:rep_idx,homophily,aversion,label,maxcdf)



dat = dat %>%
  group_by(label,model_id, id, rep_idx, homophily,aversion) %>%
  mutate(maxcdf = mean(maxcdf)) %>%
  distinct()

iqr = 
  dat %>%
  group_by(label,homophily,aversion) %>%
  summarise(median = median(maxcdf)/500*100,
            q1 = quantile(maxcdf,probs=0.25)/500*100,
            q3 = quantile(maxcdf,probs=0.75)/500*100) 
 
dat %>%
  ungroup() %>%
  ggplot(aes(x=homophily, 
             y=maxcdf/500*100, 
             colour = aversion,
             group = aversion)) +
  geom_jitter(position = position_dodge(0.6),
              size = 0.6,
              alpha = 0.2,
              shape = 19) +
  geom_errorbar(data = iqr %>%
                  mutate(aversion = factor(aversion, levels=c("low\n(0.5)",
                                                              "medium\n(0.7)",
                                                              "high\n(0.9)"))),
                aes(y = median,
                    ymin = q1,
                    ymax = q3),
                position = position_dodge(0.6),
                width = 0.5) +
  geom_point(data = iqr %>%
               mutate(aversion = factor(aversion, levels=c("low\n(0.5)",
                                                           "medium\n(0.7)",
                                                           "high\n(0.9)"))),
             aes(y = median),
             size = 2,
             position = position_dodge(0.6)) +
  labs(y="Percent Total Population Infected",
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
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,by=10)) +
  scale_y_break(c(0,70), space = 0.6, ticklabels = c(seq(0,100,10),100),
                expand = c(0, 0)) +
  scale_x_discrete(labels = value_label) +
  scale_shape_manual(values = c(5,1,8)) +
  theme_pubclean() +
  theme(
    #plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "center",
        panel.spacing.y = unit(2, "lines"),
        panel.spacing.x = unit(2, "lines"),
       # legend.location = "plot",
        legend.text = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "grey60",
                                          linewidth = 0.15),
        strip.background = element_rect(fill = "white"),
        strip.text.y = element_text(angle = 0),
        legend.box="vertical",
       axis.title.y = element_text(hjust=0.75),
        axis.text = element_text(size = 11, color = "black"),
        strip.text = element_text(size = 11, color = "black"),
        legend.margin=margin()) +
  facet_wrap(~label)

ggsave(here(paste0("output/abm_results/viz/paper_figures/final_edit_figures/fig4b.png")), width = 9, height =4, units = "in", bg="white",dpi = 500)

iqr=
  iqr %>%
  mutate(iqr = paste0(signif(median,3)," (",signif(q1,3),", ",signif(q3,3),")")) %>%
  select(-median,-q1,-q3) %>%
  pivot_wider(id_cols = c("homophily","aversion"),names_from = "label",values_from = "iqr")

write.csv(iqr,file=here('output/abm_results/summary_results/paper_tables/supp_table2_part2.csv'))




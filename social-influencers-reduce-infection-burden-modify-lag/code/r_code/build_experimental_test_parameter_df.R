### Build Social Influence Test Parameters ###

pacman::p_load(tidyverse,utils,here)

here::i_am("code/r_code/build_experimental_test_parameter_df.R")

#$1 --aversion=$2 --g1_influencer_message=$3 --g2_influencer_message=$4 
#--w1=$5 --w2=$6 --ninfluencers=$7 --f=$8 --nreplicates=$9 --nagents=1000 
test_parameters = expand.grid(aversion = c(0.3,0.5,0.7,0.9),
                              g1_influencer_message = c(0,1),
                              g2_influencer_message = c(0,1),
                              w1 = c(0.5,0.7,0.9), #homophily of group 1
                              w2 = c(0.5,0.7,0.9), #homophily of group 2
                              ninfluencers = 20,
                              f = 3,
                              nreplicates = 100,#run 10 times over in Julia = 1000
                              nagents = 1000)

#build a reference for unique model ids and unique parameters to simplify analysis
test_parameters = test_parameters %>%
  rownames_to_column(var="modelid")

parameters_diverging_influence = test_parameters %>% filter(g1_influencer_message != g2_influencer_message)

parameters_diverging_influence %>% select(-modelid) %>% print() #row.names = FALSE

parameters_same_influence = test_parameters %>% filter(g1_influencer_message == g2_influencer_message)

#save as rdata object
save(test_parameters, parameters_diverging_influence, parameters_same_influence,
     file = here("data/test_parameters_reference.Rdata"))


#build baselines
test_parameters = expand.grid(aversion = c(0.3,0.5,0.7,0.9),
                              g1_influencer_message = c(0),
                              g2_influencer_message = c(0),
                              w1 = c(0.5,0.7,0.9), #homophily of group 1
                              w2 = c(0.5,0.7,0.9), #homophily of group 2
                              ninfluencers = 20,
                              f = 3,
                              nreplicates = 100,
                              nagents = 1000)

#build a reference for unique model ids and unique parameters to simplify analysis
test_parameters = test_parameters %>%
  rownames_to_column(var="modelid")

parameters_diverging_influence = test_parameters %>% filter(g1_influencer_message != g2_influencer_message)

parameters_diverging_influence %>% select(-modelid) %>% print() #row.names = FALSE

parameters_same_influence = test_parameters %>% filter(g1_influencer_message == g2_influencer_message)

save(test_parameters, parameters_diverging_influence, parameters_same_influence,
     file = here("data/baseline_parameters_reference.Rdata"))

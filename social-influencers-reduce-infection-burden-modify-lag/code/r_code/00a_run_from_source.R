#### Analytical Code: "Social influencers reduce infection burden and 
#### modify epidemic lag in group-structured populations"

#Code Author: Aja Sutton
#Summer 2025


# Manage database referencing
if (!require("here")) install.packages("here")
library(here)

# Point to working directory
here::i_am("code/r_code/00a_run_from_source.R")

# Run analysis from source

# I built the test parameters for the Julia code in R for ease, here:
source("code/r_code/build_experimental_test_parameter_df.R")


# NB: prior to running the R code below, the agent-based models must be run in Julia.

# Process and clean raw simulation data produced in Julia
# This processes simulation data for experiment simulations only; to process
# baseline data, open this script and at the top set baselines = TRUE
  source("code/r_code/00b_process_simulation_data.R")

# Analyze results and build visualizations of infection and behaviour curves 

  # (i) for Scenario 1 models (i.e., without influencers)
  source("code/r_code/01a_analyze_and_visualize_no_influencer_models.R")
  
  # (ii) for Scenario 2 and 3 models (i.e., with influencers)
  source("code/r_code/01b_analyze_and_visualize_influencer_models.R")

  #Figure 2 is composed of individual visualisations built in the above scripts.
  #Figure 3 is composed of these, manually trimmed of axes labels, and built
  #into a collage with guiding arrows for intuition. The visualized data are
  #not manipulated.

  # (iii) we build Figure 4 here:
  source("code/r_code/02_build_figures_save_data_fig4a_fig4b.R")
  
  #(iv) we build Figure 5 here:
  source("code/r_code/03_build_figure_save_data_fig5.R")



#!/bin/bash



# Submit main results jobs, one line for each innovation starting condition,
# specified in the third positional argument to run_helper.sh: 
# testruntrials --aversion=$2 --g1_influencer_message=$3 --g2_influencer_message=$4 
#--w1=$5 --w2=$6 --ninfluencers=$7 --f=$8 --nreplicates=$9 --nagents=$10 


# The first positional argument passed to this script is the results CSV output write directory.


#this first section represents all models where g1_influencer_message != g2_influencer_message
#we can turn this off and run another section with the models that have the same messaging later.




##f = 3 (ie number of teachers for behavior)
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.1 0 1 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.5 0.5 20 3 100 
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.5 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.7 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.9 0.5 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.5 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.7 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.9 0.7 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.5 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.7 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 0 0.9 0.9 20 3 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 0 0.9 0.9 20 3 100

##f = 5 (ie number of teachers for behavior)
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.5 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.5 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.5 0.5 20 5 100 
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.5 0.5 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.5 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.5 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.5 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.5 0.5 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.7 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.7 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.7 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.7 0.5 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.7 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.7 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.7 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.7 0.5 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.9 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.9 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.9 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.9 0.5 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.9 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.9 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.9 0.5 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.9 0.5 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.5 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.5 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.5 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.5 0.7 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.5 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.5 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.5 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.5 0.7 20 5 100
#sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.7 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.7 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.7 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.7 0.7 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.7 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.7 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.7 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.7 0.7 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.9 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.9 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.9 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.9 0.7 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.9 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.9 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.9 0.7 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.9 0.7 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.5 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.5 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.5 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.5 0.9 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.5 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.5 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.5 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.5 0.9 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.7 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.7 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.7 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.7 0.9 20 5 100
# #sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.7 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.7 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.7 0.9 20 5 100
# sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.7 0.9 20 5 100
#sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 1 0 0.9 0.9 20 5 100
sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 1 0 0.9 0.9 20 5 100
sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 1 0 0.9 0.9 20 5 100
sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 1 0 0.9 0.9 20 5 100
#sbatch --array=1-10 src/slurm/run_helper.sh $1 0.3 0 1 0.9 0.9 20 5 100
sbatch --array=1-10 src/slurm/run_helper.sh $1 0.5 0 1 0.9 0.9 20 5 100
sbatch --array=1-10 src/slurm/run_helper.sh $1 0.7 0 1 0.9 0.9 20 5 100
sbatch --array=1-10 src/slurm/run_helper.sh $1 0.9 0 1 0.9 0.9 20 5 100
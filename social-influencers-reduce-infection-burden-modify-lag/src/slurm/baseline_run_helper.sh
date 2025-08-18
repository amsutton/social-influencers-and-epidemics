#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem-per-cpu=2GB
#SBATCH --time=00:35:00 
#SBATCH --output=SocInf.out
#SBATCH --partition=serc,normal
#SBATCH --mail-type=begin
#SBATCH --mail-type=end  

#remember that the sbatch time is calling for each array in sbatch, 
#so it's for each instance of running the model 100 times! 
#job resource scoping should be done on *one* line of sbatch call in main.sh
module --force purge
module load devel
module load julia/1.7.2

#$1 is the directory name space
julia src/baseline_run_trials.jl $1 --aversion=$2 --g1_influencer_message=$3 --g2_influencer_message=$4 --w1=$5 --w2=$6 --ninfluencers=$7 --f=$8 --nreplicates=$9 --nagents=1000 

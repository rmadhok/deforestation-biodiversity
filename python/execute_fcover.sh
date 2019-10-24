#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --account=def-sgulati
#SBATCH --mem=100G
#SBATCH --mail-user=madhok.raahil@gmail.com
#SBATCH --mail-type=END

module load r/3.6.0
module load gcc/7.3.0

cd /home/rmadhok/projects/def-sgulati/rmadhok/def_biodiv/scripts
Rscript process_tree_cover_wg.R
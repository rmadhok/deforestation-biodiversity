#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --account=def-sgulati
#SBATCH --mem=40G
#SBATCH --mail-user=madhok.raahil@gmail.com
#SBATCH --mail-type=END

module load python/2.7
virtualenv --no-download $SLURM_TMPDIR/env
source $SLURM_TMPDIR/env/bin/activate
pip install --upgrade pip

pip install requests
pip install bs4
pip install lxml
pip install pandas
pip install mechanize
pip install func-timeout

cd /home/rmadhok/projects/def-sgulati/rmadhok/def_biodiv/scripts
python fc_scraper_wg.py

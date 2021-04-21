#!/bin/bash
#SBATCH --time=01:30:00
#SBATCH --mem=32Gb
#SBATCH --partition=regular
../EGS parameters.txt

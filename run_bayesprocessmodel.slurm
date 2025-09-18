#!/bin/bash
#SBATCH --job-name=bayes_gpu_job
#SBATCH --output=logs/bayes_%j.out
#SBATCH --error=logs/bayes_%j.err
#SBATCH --partition=gpu         # Using the newer GPU partition
#SBATCH --gres=gpu:0               # Request 1 GPU
#SBATCH --cpus-per-task=32          # CPU cores
#SBATCH --mem=16G                  # Memory
#SBATCH --time=04:00:00            # Runtime

# Load Python module (adjust version to match your env setup)
module load 2023
module load GPU

# Activate your virtual environment
source ~/venvs/bayesenv/bin/activate

# Default values
DRAW_SAMPLES=250
TUNE_SAMPLES=500
CHAINS=32

# Run your Python script
python Py/src/bayesianInference.py \
    --draw-samples=$DRAW_SAMPLES \
    --tune-samples=$TUNE_SAMPLES \
    --chains=$CHAINS \
    --n_cores=32 \
    --mode_model="force" \
    --cv \
    --location_option=1 \
    --split_method='mean_temperature' \
    --n_splits=3 \
    --data_type='moth' \
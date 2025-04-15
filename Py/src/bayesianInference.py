# BayesianInference.py

import os, sys
import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import pymc as pm
import arviz as az
import pytensor
import pytensor.tensor as pt
from sklearn.metrics import r2_score
import argparse
import logging

sys.path.append(os.path.abspath(os.path.join(os.getcwd(), os.pardir, 'src')))
from dataloadermaker import DataLoaderMaker
import vis_utils as vu
import probabilistic_utils as pu

# Configure logger
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

USE_MACOS = False
if USE_MACOS:
    pytensor.config.cxx = ''

def main():
    parser = argparse.ArgumentParser(description='Bayesian Inference with MCMC parameters')
    parser.add_argument('--draw-samples', type=int, default=50,
                        help='Number of MCMC draw samples')
    parser.add_argument('--tune-samples', type=int, default=50,
                        help='Number of MCMC tune samples')
    parser.add_argument('--chains', type=int, default=8,
                        help='Number of MCMC chains')
    
    args = parser.parse_args()
    
    posterior_samples, df_train, df_test = pu.bayesian_inference(
        mcmc_draw_samples=args.draw_samples,
        mcmc_tune_samples=args.tune_samples,
        mcmc_chains=args.chains
    )

    df_use = df_test 

    # Get posterior samples
    posterior = az.extract(posterior_samples)

    method_estimate = 'mean'
    match method_estimate:
        case 'mean':
            t_base_post = float(posterior["t_base"].mean().values)  # or az.map_estimate(posterior_samples)["t_base"]
            start_doy_post = float(posterior["start_date"].mean().values)  # or MAP estimate
        case 'median':
            t_base_post = float(posterior["t_base"].median().values)  # or az.map_estimate(posterior_samples)["t_base"]
            start_doy_post = float(posterior["start_date"].median().values)  # or MAP estimate
        # case 'MAP':
            # t_base_post = az.map_estimate(posterior_samples)["t_base"]
            # start_doy_post = az.map_estimate(posterior_samples)["start_date"]

    # Compute GDD for test data
    temperature_test = df_use["temperature"].values
    t_above_base_test = np.maximum(0, temperature_test - t_base_post)

    gdd_test = np.zeros_like(t_above_base_test)
    for s in df_use["season"].unique():
        inds_s = df_use["season"] == s
        inds_s = inds_s.values

        doy_s = df_use["doy"][inds_s].values
        gdd_s = t_above_base_test[inds_s]
        gdd_s[doy_s < start_doy_post] = 0
        gdd_s = np.cumsum(gdd_s)
        gdd_test[inds_s] = gdd_s

    # α_post = float(posterior["α"].mean().values)  # or MAP estimate
    β_post = float(posterior["β"].mean().values)  # or MAP estimate
    mu_test = 1 / (1 + np.exp(-(β_post * gdd_test)))  # Sigmoid function

    df_use["predicted_bb_cdf"] = mu_test
    r2 = r2_score(df_use["bb_cdf"], df_use["predicted_bb_cdf"])
    logger.info(f"R² on test set: {r2:.3f}")

    # Plot predictions vs actual values
    plt.figure(figsize=(5, 3))
    plt.scatter(df_use["bb_cdf"], df_use["predicted_bb_cdf"], alpha=0.5)
    plt.plot([0, 1], [0, 1], color="red", linestyle="--")
    plt.xlabel("Actual BB CDF") 
    plt.ylabel("Predicted BB CDF")

    # Get the parent directory of the current script
    current_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(os.path.dirname(current_dir))
    save_dir = os.path.join(parent_dir, 'fg')
    
    # Create directory if it doesn't exist
    os.makedirs(save_dir, exist_ok=True)

    # Save the figure
    plt.savefig(os.path.join(save_dir, 'bb_cdf_prediction.png'), 
                dpi=300, 
                bbox_inches='tight')
    plt.close()  # Close the figure to free memory


    seasons = df_use['season'].unique()
    n_seasons = len(seasons)
    fig, ax = plt.subplots(figsize=(12, n_seasons // 3 * 3), nrows=n_seasons // 3, ncols=3, gridspec_kw={"hspace": 0.4, "wspace": 0.8})

    for i_s, s in enumerate(seasons):
        tmp_sel = df_use[df_use['season'] == s]
        curr_ax = np.ravel(ax)[i_s]

        curr_ax.plot(tmp_sel.doy, tmp_sel.temperature, '.', c='k', markersize=2)

        ax2 = curr_ax.twinx()
        ax2.plot(tmp_sel.doy, tmp_sel.predicted_bb_cdf, 'r-', lw=2)
        ax2.plot(tmp_sel.doy, tmp_sel.bb_cdf, 'b-', lw=2)
        curr_ax.annotate(s, xy=(0.05, 0.9), xycoords='axes fraction', ha='left', va='center', weight='bold')
        curr_ax.set_xlabel("DOY")
        curr_ax.set_ylabel("Temperature")
        ax2.set_ylabel("BB CDF")

    # Save the multi-panel figure
    plt.savefig(os.path.join(save_dir, 'seasonal_comparison.png'),
                dpi=300,
                bbox_inches='tight')
    plt.close()  # Close the figure to free memory

    df_use = df_test

    # Extract posterior samples (e.g., 1000 samples)
    n_samples = len(posterior["t_base"])  # Number of posterior samples
    temperature_test = df_use["temperature"].values
    n_test = len(temperature_test)  # Number of test observations

    # Initialize array to store GDD predictions (n_samples x n_test)
    gdd_samples = np.zeros((n_samples, n_test))
    for i in range(n_samples):
        t_base_sample = float(posterior["t_base"][i].values)
        start_doy_sample = float(posterior["start_date"][i].values)

        t_above_base_test = np.maximum(0, temperature_test - t_base_sample)

        gdd_test = np.zeros_like(t_above_base_test)
        for s in df_use["season"].unique():
            inds_s = df_use["season"] == s
            inds_s = inds_s.values

            doy_s = df_use["doy"][inds_s].values
            gdd_s = t_above_base_test[inds_s]
            gdd_s[doy_s < start_doy_sample] = 0  # Set GDD to 0 before start date
            gdd_s = np.cumsum(gdd_s)  # Compute cumulative sum
            gdd_test[inds_s] = gdd_s

        gdd_samples[i, :] = gdd_test  # Store sample-specific GDD

    # Initialize array for predictions (n_samples x n_test)
    bb_cdf_samples = np.zeros((n_samples, n_test))
    for i in range(n_samples):
        # α_sample = float(posterior_samples["α"][i].values)
        β_sample = float(posterior["β"][i].values)
        
        # Logistic function: maps GDD to cumulative fraction
        bb_cdf_samples[i, :] = 1 / (1 + np.exp(-( β_sample * gdd_samples[i, :])))
        # bb_cdf_samples[i, :] = 1 / (1 + np.exp(-(α_sample + β_sample * gdd_samples[i, :])))

    bb_cdf_mean = np.mean(bb_cdf_samples, axis=0)  # Mean prediction
    bb_cdf_lower = np.percentile(bb_cdf_samples, 0.5, axis=0)  # 2.5th percentile (lower CI)
    bb_cdf_upper = np.percentile(bb_cdf_samples, 99.5, axis=0)  # 97.5th percentile (upper CI)

    df_use["predicted_bb_cdf"] = bb_cdf_mean
    df_use["bb_cdf_lower"] = bb_cdf_lower
    df_use["bb_cdf_upper"] = bb_cdf_upper


    # Plot predictions with uncertainty (shaded area)
    fig, ax = plt.subplots(figsize=(12, 6), nrows=2, ncols=3, gridspec_kw={"hspace": 0.4, "wspace": 0.8})

    for i_s, s in enumerate(df_use['season'].unique()):
        tmp_sel = df_use[df_use['season'] == s]
        curr_ax = np.ravel(ax)[i_s]

        curr_ax.plot(tmp_sel.doy, tmp_sel.temperature, '.', c='k', markersize=2)

        ax2 = curr_ax.twinx()
        ax2.plot(tmp_sel.doy, tmp_sel.predicted_bb_cdf, 'r-', lw=2)
        ax2.plot(tmp_sel.doy, tmp_sel.bb_cdf, 'b-', lw=1)
        ## uncertainty:
        ax2.fill_between(x=tmp_sel.doy, y1=tmp_sel.bb_cdf_lower, y2=tmp_sel.bb_cdf_upper, color='red', alpha=0.4)

        curr_ax.annotate(s, xy=(0.05, 0.9), xycoords='axes fraction', ha='left', va='center', weight='bold')
        curr_ax.set_xlabel("DOY")
        curr_ax.set_ylabel("Temperature")
        ax2.set_ylabel("BB CDF")

    fig.suptitle('Evaluation of the model on test data', weight='bold')
    
    # Save the figure with uncertainty bands
    plt.savefig(os.path.join(save_dir, 'seasonal_comparison_with_uncertainty.png'),
                dpi=300,
                bbox_inches='tight')
    plt.close()  # Close the figure to free memory

if __name__ == "__main__":
    main()
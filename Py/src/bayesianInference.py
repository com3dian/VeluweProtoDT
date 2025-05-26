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
from datetime import datetime

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

def main():
    parser = argparse.ArgumentParser(description='Bayesian Inference with MCMC parameters')
    parser.add_argument('--draw-samples', type=int, default=50,
                        help='Number of MCMC draw samples')
    parser.add_argument('--tune-samples', type=int, default=50,
                        help='Number of MCMC tune samples')
    parser.add_argument('--chains', type=int, default=8,
                        help='Number of MCMC chains')
    parser.add_argument('--save_posterior', type=bool, default=False,
                        help='Save posterior samples')
    parser.add_argument('--plot_posterior_fit', type=bool, default=True,
                        help='Plot posterior fit')
    parser.add_argument('--use_macos', type=bool, default=False)
    parser.add_argument('--n_cores', type=int, default=8)
    parser.add_argument('--mode_model', type=str, default='force_chill')
    parser.add_argument('--cv', action='store_true')
    parser.add_argument('--species', type=str, default='Quercus robur L.')
    parser.add_argument('--location_option', type=int, default=0)
    
    args = parser.parse_args()

    assert args.mode_model in ['force', 'force_chill', 'force_chill-zoned'], 'mode_model must be force, chill or force-zoned'
    if args.mode_model == 'force':
        INFER_CHILLING = False
        ZONED_CHILLING = False
    elif args.mode_model == 'force_chill':
        INFER_CHILLING = True
        ZONED_CHILLING = False
    elif args.mode_model == 'force_chill-zoned':
        INFER_CHILLING = True
        ZONED_CHILLING = True
    USE_MACOS = args.use_macos
    USE_CV = args.cv
    if USE_MACOS:
        pytensor.config.cxx = ''

    if USE_CV:
        cv_splits = [0, 1, 2, 3, 4, 5]
    else:
        cv_splits = [5]

    species_sel=args.species
    if args.location_option == 0:
        location_list=None
    elif args.location_option == 1:
        location_list=['Hoge Veluwe']
    elif args.location_option == 2:
        location_list=['Oosterhout']
    if location_list is None:
        descr_location = 'All locations'
    else:
        descr_location = ' + '.join(location_list)
    print('--------\nUsing location list:', location_list)
    print('Using species:', species_sel, '\n--------')

    timestamp = datetime.now().strftime("%Y-%m-%d-%H%M")
    for split in cv_splits:
        logger.info(f"Running split {split} with mode {args.mode_model}")
        print(f"Running split {split} with mode {args.mode_model}")
        posterior_samples, df_train, df_test, model = pu.bayesian_inference(
            mcmc_draw_samples=args.draw_samples,
            mcmc_tune_samples=args.tune_samples,
            mcmc_chains=args.chains,
            infer_chilling=INFER_CHILLING,
            zoned_chilling=ZONED_CHILLING,
            mcmc_cores=args.n_cores,
            split_seasons_traintest=split,
            species_sel=species_sel,
            location_list=location_list
        )

        # Get the parent directory of the current script
        current_dir = os.path.dirname(os.path.abspath(__file__))
        parent_dir = os.path.dirname(os.path.dirname(current_dir))
        save_dir_base = os.path.join(parent_dir, 'fg')
        os.makedirs(save_dir, exist_ok=True)
        folder_name = f'{timestamp}_{args.mode_model.replace('_', '-')}_{species_sel.rstrip('.').replace(' ', '-')}_{descr_location.replace(" ", "-")}'.lower()
        save_dir = os.path.join(save_dir_base, folder_name)
        os.makedirs(save_dir, exist_ok=True)

        if args.save_posterior:
            save_dir_post = os.path.join(parent_dir, 'posterior_samples')
            os.makedirs(save_dir_post, exist_ok=True)
            az.to_netcdf(posterior_samples, os.path.join(save_dir_post, 'posterior_samples.nc'))

        if args.plot_posterior_fit:
            df_use = df_test 
            # Get posterior samples
            posterior = az.extract(posterior_samples)
            hparam_info_str = f'{descr_location}, {species_sel}\n{args.tune_samples} tune, {args.draw_samples} draw, {args.chains} chains, {args.mode_model}, split {split}'
            try:
                if INFER_CHILLING:
                    vars_temp = ['t_base_force', 't_base_chill', 'threshold_cum_chill']
                    if ZONED_CHILLING:
                        vars_temp += ['t_bottom_chill']
                    vars_modelfit = ['alpha', 'beta', 'threshold_cum_chill']
                else:
                    vars_temp = ['start_date', 't_base_force']
                    vars_modelfit = ['alpha', 'beta']

                az.plot_pair(posterior_samples, var_names=vars_temp, kind='kde', marginals=True)
                plt.suptitle(hparam_info_str, weight='bold', fontsize=10)
                plt.savefig(os.path.join(save_dir, f'joint_posterior_temperature_{timestamp}_split-{split}.png'),
                            dpi=300, bbox_inches='tight')
                plt.close()  # Close the figure to free memory

                az.plot_pair(posterior_samples, var_names=vars_modelfit, kind='kde', marginals=True)
                plt.suptitle(hparam_info_str, weight='bold', fontsize=10)
                plt.savefig(os.path.join(save_dir, f'joint_posterior_modelfit_{timestamp}_split-{split}.png'),
                            dpi=300, bbox_inches='tight')
                plt.close()  # Close the figure to free memory
                
            except ValueError as e:
                print(f"Error plotting joint posterior: {e}")
            df_use = df_test

            # Extract posterior samples (e.g., 1000 samples)
            n_samples = len(posterior["t_base_force"])  # Number of posterior samples
            temperature_test = df_use["temperature"].values
            n_test = len(temperature_test)  # Number of test observations

            # Initialize array to store GDD predictions (n_samples x n_test)
            gdd_samples = np.zeros((n_samples, n_test))

            for i in range(n_samples):
                ## Get posterior sample
                t_base_force_sample = float(posterior["t_base_force"][i].values)
                if INFER_CHILLING:
                    t_base_chill_sample = float(posterior["t_base_chill"][i].values)
                    threshold_cum_chill_sample = float(posterior["threshold_cum_chill"][i].values)
                    if ZONED_CHILLING:
                        t_bottom_chill_sample = float(posterior["t_bottom_chill"][i].values)
                else:
                    start_doy_sample = float(posterior["start_date"][i].values)

                ## Compute variables:
                t_above_base_test = np.maximum(0, temperature_test - t_base_force_sample)
                gdd_test = np.zeros_like(t_above_base_test)
                for s in df_use["season"].unique():
                    inds_s = df_use["season"] == s
                    inds_s = inds_s.values
                    doy_s = df_use["doy"][inds_s].values

                    gdd_s = t_above_base_test[inds_s]
                    if INFER_CHILLING:
                        cum_chill_days = np.zeros_like(doy_s)
                        cum_chill_days[temperature_test[inds_s] < t_base_chill_sample] = 1
                        if ZONED_CHILLING:
                            cum_chill_days[temperature_test[inds_s] < t_bottom_chill_sample] = 0
                        cum_chill_days = np.cumsum(cum_chill_days)
                        gdd_s[cum_chill_days < threshold_cum_chill_sample] = 0
                    else:
                        gdd_s[doy_s < start_doy_sample] = 0  # Set GDD to 0 before start date
                    gdd_s = np.cumsum(gdd_s)  # Compute cumulative sum
                    gdd_test[inds_s] = gdd_s
                gdd_samples[i, :] = gdd_test  # Store sample-specific GDD

            ## Predict BB CDF using posterior samples
            bb_cdf_samples = np.zeros((n_samples, n_test))
            for i in range(n_samples):
                alpha_sample = float(posterior["alpha"][i].values)
                beta_sample = float(posterior["beta"][i].values)
                bb_cdf_samples[i, :] = 1 / (1 + np.exp(-(alpha_sample + beta_sample * gdd_samples[i, :])))

            bb_cdf_mean = np.mean(bb_cdf_samples, axis=0)  # Mean prediction
            bb_cdf_lower = np.percentile(bb_cdf_samples, 0.5, axis=0)  # 2.5th percentile (lower CI)
            bb_cdf_upper = np.percentile(bb_cdf_samples, 99.5, axis=0)  # 97.5th percentile (upper CI)

            df_use["predicted_bb_cdf"] = bb_cdf_mean
            df_use["bb_cdf_lower"] = bb_cdf_lower
            df_use["bb_cdf_upper"] = bb_cdf_upper

            ## Plot predictions with uncertainty (shaded area)
            fig, ax = plt.subplots(figsize=(12, 6), nrows=2, ncols=3, gridspec_kw={"hspace": 0.4, "wspace": 0.8})
            ymin, ymax = 10, 10
            for i_s, s in enumerate(df_use['season'].unique()):
                tmp_sel = df_use[df_use['season'] == s]
                curr_ax = np.ravel(ax)[i_s]

                curr_ax.plot(tmp_sel.doy, tmp_sel.temperature, '.', c='k', markersize=2)

                ax2 = curr_ax.twinx()
                ax2.plot(tmp_sel.doy, tmp_sel.predicted_bb_cdf, 'r-', lw=2)
                ax2.plot(tmp_sel.doy, tmp_sel.bb_cdf, 'b-', lw=1)
                ax2.fill_between(x=tmp_sel.doy, y1=tmp_sel.bb_cdf_lower, y2=tmp_sel.bb_cdf_upper, color='red', alpha=0.4)

                curr_ax.annotate(s, xy=(0.05, 0.9), xycoords='axes fraction', ha='left', va='center', weight='bold')
                curr_ax.set_xlabel("DOY")
                curr_ax.set_ylabel("Temperature")
                ax2.set_ylabel("BB CDF")
                ymin = min(ymin, curr_ax.get_ylim()[0])
                ymax = max(ymax, curr_ax.get_ylim()[1])

            for curr_ax in np.ravel(ax):
                curr_ax.set_ylim(ymin, ymax)
                
            fig.suptitle('Evaluation of the model on test data\n' + hparam_info_str, weight='bold', fontsize=10)
            plt.savefig(os.path.join(save_dir, f'posterior_prediction_bbcdf_{timestamp}_split-{split}.png'),
                        dpi=300, bbox_inches='tight')
            plt.close()  # Close the figure to free memory

if __name__ == "__main__":
    main()
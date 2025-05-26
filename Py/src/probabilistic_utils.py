import os, sys
import numpy as np 
import pandas as pd 
from datetime import datetime
import pymc as pm 
import pytensor
import pytensor.tensor as pt

from dataloadermaker import DataLoaderMaker

def prep_data_for_regression(budburst_df=None, temp_df=None, 
                            #  t_base_force=4, gdd_month_day_start=None, 
                             species_sel='Quercus robur L.',
                             location_list=None):
    """
    Prepares the data for regression analysis by merging the budburst and temperature dataframes.

    Parameters:
    - budburst_df: DataFrame containing budburst data.
    - temp_df: DataFrame containing temperature data.
    - t_base_force: Base temperature for growing degree days (default is 4).

    Returns:
    - regression_df: DataFrame containing merged data for regression analysis.
        Use winter seasons as years (Nov - June). Create DFs for each year separately.
        Each row is one DOY
        Each column is one feature (temperature, GDD, bb_frac).
    """
    if budburst_df is None or temp_df is None:
        print('No data provided -- using default data')
        VeluweTreeData = DataLoaderMaker()
        VeluweTreeData.load()

        # Get the budburst and temperature dataframe
        temp_df = VeluweTreeData.get("temp_climwin_input")
        budburst_df = VeluweTreeData.get("interpolated")

    assert species_sel in budburst_df['species'].unique(), f"Species {species_sel} not found in budburst data {budburst_df['species'].unique()}" 
    season_start_doy = 250 
    dict_data_per_year = {}
    years = sorted(budburst_df['year'].unique())
    if location_list is None:
        print("No location list provided, using all locations")
    else:
        print(f"Filtering data for locations: {location_list}")
        budburst_df = budburst_df[budburst_df['verbatimLocality'].isin(location_list)]

    for y in years:
        bb_sel = budburst_df[np.logical_and(budburst_df['year'] == y, 
                                           budburst_df['species'] == species_sel)]
        
        if len(bb_sel) == 0:
            print(f"No data for year {y}")
            continue
        arr_doy = bb_sel.bud_burst_DOY.values
        arr_doy = np.sort(arr_doy)
        bb_cdf = np.arange(len(arr_doy)) / float(len(arr_doy) - 1) ## there can be duplicate doys, so we need the maximum the budburst fraction per doy
        arr_doy = np.round(arr_doy).astype(int)  # Round DOY to nearest integer (For proper GGD calculation and to avoid duplicate DOYs later)
        bb_cdf = pd.DataFrame({'doy': arr_doy, 'bb_cdf': bb_cdf}).groupby('doy').max().reset_index()
        bb_cdf['date'] = pd.to_datetime(bb_cdf['doy'], format='%j').dt.tz_localize('UTC') + pd.offsets.DateOffset(years=y - 1900)  ## 1900 is default start year for pd dt
        temp_sel = temp_df[((temp_df['date'].dt.year == y) & 
                            (temp_df['date'].dt.day_of_year < season_start_doy)) | 
                            ((temp_df['date'].dt.year == y-1) & 
                            (temp_df['date'].dt.day_of_year >= season_start_doy))].copy()

        if len(temp_sel) == 0:
            print(f"No temperature data for year {y}")
            continue

        temp_sel = temp_sel[['date', 'temperature']].groupby('date').mean().reset_index()
        temp_sel = temp_sel.rename(columns={'date': 'date_temp'})
        bb_cdf = bb_cdf.rename(columns={'date': 'date_bb'})
        regression_df = pd.merge(bb_cdf, temp_sel, left_on='date_bb', right_on='date_temp', how='right')
        del regression_df['date_bb']
        del regression_df['doy']
        regression_df = regression_df.rename(columns={'date_temp': 'date'})
        regression_df['bb_cdf'] = regression_df['bb_cdf'].ffill().fillna(0)
        regression_df['date'] = pd.to_datetime(regression_df['date'], format='%Y-%m-%d')

        s = f'{y-1}-{y}'
        regression_df['season'] = s

        dict_data_per_year[s] = regression_df

    regression_df = pd.concat(dict_data_per_year.values(), ignore_index=True)
    return regression_df

def bayesian_inference(
        split_seasons_traintest=None,
        mcmc_draw_samples=100,
        mcmc_tune_samples=200,
        mcmc_chains=32,
        mcmc_cores=8,
        infer_chilling=False,
        zoned_chilling=False,
        species_sel='Quercus robur L.',
        location_list=None
        ):
    df_regression = prep_data_for_regression(species_sel=species_sel, location_list=location_list)
    df_regression['doy'] = df_regression['date'].dt.day_of_year
    df_regression = df_regression[df_regression['date'].dt.month < 7]  # delete Dec effectively, just to make DOY prior easier to deal with 

    seasons = sorted(df_regression["season"].unique())
    assert len(seasons) == 36, f"Expected 36 seasons, got {len(seasons)}"
    assert split_seasons_traintest in [0, 1, 2, 3, 4, 5], f"split_seasons_train must be in [0, 1, 2, 3, 4, 5], got {split_seasons_traintest}"
    ## Create 6 blocks of 6 consecutive seasons, use split_seasons_train to select the test block 
    train_seasons = seasons[:split_seasons_traintest * 6] + seasons[(split_seasons_traintest + 1) * 6:]
    test_seasons = seasons[split_seasons_traintest * 6:(split_seasons_traintest + 1) * 6]
    assert len(train_seasons) == 30, f"Expected 30 training seasons, got {len(train_seasons)}"
    print(f"Training seasons: {train_seasons}, test seasons: {test_seasons}")

    df_train = df_regression[df_regression["season"].isin(train_seasons)]
    df_test = df_regression[df_regression["season"].isin(test_seasons)]

    with pm.Model() as model:
        ## Extract data
        bb_cdf_obs = df_train['bb_cdf'].values
        temperature = df_train['temperature'].values

        ## Define priors
        t_base_force = pm.Normal("t_base_force", mu=5, sigma=2)  # Prior for base temperature
        if infer_chilling:
            threshold_cum_chill = pm.DiscreteUniform("threshold_cum_chill", lower=0, upper=50)  # Prior for chilling threshold
            t_base_chill = pm.Normal("t_base_chill", mu=5, sigma=2)  # Prior for chilling base temperature
            if zoned_chilling:
                t_bottom_chill = pm.Normal("t_bottom_chill", mu=0, sigma=2)  # Prior for chilling bottom temperature
        else:
            start_doy = pm.DiscreteUniform("start_date", lower=60, upper=100)  # Prior for GDD start date
        
        ## Calculate variables
        t_above_base = pm.math.maximum(0, temperature - t_base_force)  # GDD calculation
        gdd = pm.math.zeros_like(t_above_base)  # Initialize GDD array
        for s in df_train['season'].unique():
            inds_s = df_train['season'] == s
            inds_s = inds_s.values
            doy_s = df_train['doy'][inds_s].values
            
            gdd_s = t_above_base[inds_s]
            if infer_chilling:
                cum_chill_days = pt.where(temperature[inds_s] < t_base_chill, 1, 0)
                if zoned_chilling:
                    cum_chill_days = pt.where(temperature[inds_s] < t_bottom_chill, 0, cum_chill_days)
                cum_chill_days = pt.cumsum(cum_chill_days)
                gdd_s = pt.where(cum_chill_days >= threshold_cum_chill, gdd_s, 0)
            else:
                gdd_s = pt.where(doy_s >= start_doy, gdd_s, 0)
            gdd_s = pt.cumsum(gdd_s)
            gdd = pt.set_subtensor(gdd[inds_s], gdd_s)  # Alternative if gdd is also a tensor

        ## Logistic function: maps GDD to cumulative fraction
        alpha = pm.Normal("alpha", mu=0, sigma=10)  # Intercept
        beta = pm.LogNormal("beta", mu=0, sigma=1)  # Slope
        mu = pm.Deterministic("mu", pm.math.sigmoid(alpha + beta * gdd))  # Sigmoid function
        
        ## Likelihood: Normal distribution with uncertainty
        sigma = pm.HalfNormal("sigma", sigma=0.1)
        bb_cdf_likelihood = pm.Normal("bb_cdf", mu=mu, sigma=sigma, observed=bb_cdf_obs)

        # Sample posterior
        trace = pm.sample(draws=mcmc_draw_samples, tune=mcmc_tune_samples, 
                          chains=mcmc_chains, cores=mcmc_cores, return_inferencedata=True)
        
    return trace, df_train, df_test

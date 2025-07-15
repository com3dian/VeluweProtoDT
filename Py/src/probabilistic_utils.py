import os, sys
import numpy as np 
import pandas as pd 
from datetime import datetime
import pymc as pm 
import pytensor
import pytensor.tensor as pt
from collections import Counter
from dataloadermaker import DataLoaderMaker

def prep_budburst_data_for_regression(budburst_df=None, temp_df=None, 
                            #  t_base_force=4, gdd_month_day_start=None, 
                             species_sel='Quercus robur L.',
                             location_list=['Hoge Veluwe']):
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

def prep_moth_data_for_regression(moth_df, temp_df, location_list=['HV'], verbose=1):
    if temp_df is None:
        assert False, 'implement'

    moth_df = moth_df[moth_df['AreaShortName'].isin(location_list)]
    years = sorted(moth_df.YearHatch.unique())
    dict_data_per_year = {}
    season_start_doy = 250

    for y in years:
        moth_sel = moth_df[moth_df['YearHatch'] == y]
        if len(moth_sel) == 0:
            if verbose > 0:
                print(f'No data for year {y}')
            continue 
        arr_doy = moth_sel.DOY_hatch.values
        arr_doy = np.sort(arr_doy)
        moth_cdf = np.arange(len(arr_doy)) // float(len(arr_doy) - 1)
        arr_doy = np.round(arr_doy).astype(int)
        moth_cdf = pd.DataFrame({'doy': arr_doy, 'moth_cdf': moth_cdf}).groupby('doy').max().reset_index()
        moth_cdf['date'] = pd.to_datetime(moth_cdf['doy'], format='%j').dt.tz_localize('UTC') + pd.offsets.DateOffset(years=y - 1900)  ## 1900 is default start year for pd dt
        
        temp_sel = temp_df[((temp_df['date'].dt.year == y) & 
                            (temp_df['date'].dt.day_of_year < season_start_doy)) | 
                            ((temp_df['date'].dt.year == y-1) & 
                            (temp_df['date'].dt.day_of_year >= season_start_doy))].copy()

        if len(temp_sel) == 0:
            print(f"No temperature data for year {y}")
            continue

        temp_sel = temp_sel[['date', 'temperature']].groupby('date').mean().reset_index()
        temp_sel = temp_sel.rename(columns={'date': 'date_temp'})
        moth_cdf = moth_cdf.rename(columns={'date': 'date_moth'})
        regression_df = pd.merge(moth_cdf, temp_sel, left_on='date_moth', right_on='date_temp', how='right')
        del regression_df['date_moth']
        del regression_df['doy']
        regression_df = regression_df.rename(columns={'date_temp': 'date'})
        regression_df['moth_cdf'] = regression_df['moth_cdf'].ffill().fillna(0)
        regression_df['date'] = pd.to_datetime(regression_df['date'], format='%Y-%m-%d')

        s = f'{y-1}-{y}'
        regression_df['season'] = s

        dict_data_per_year[s] = regression_df

    regression_df = pd.concat(dict_data_per_year.values(), ignore_index=True)
    return regression_df


def split_data_by_season(df_regression, split_seasons_traintest: int, 
                         equal_number_obs_per_season=True, split_method='sequential', n_splits=6):
    assert split_method in ['sequential', 'mean_temperature']
    assert n_splits == 6, "Currently only supports 6 splits for seasons (6 years of data)."
    df_regression['doy'] = df_regression['date'].dt.day_of_year
    df_regression = df_regression[df_regression['date'].dt.month < 7]  # delete Dec effectively, just to make DOY prior easier to deal with 

    seasons = sorted(df_regression["season"].unique())
    assert len(seasons) == 36, f"Expected 36 seasons, got {len(seasons)}"
    assert type(split_seasons_traintest) == int and split_seasons_traintest in np.arange(n_splits), f"split_seasons_train must be in {np.arange(n_splits)}, got {split_seasons_traintest}"
    if split_method == 'mean_temperature':
        df_mean_temp = df_regression[df_regression['date'].dt.month <= 4].groupby('season')['temperature'].mean().sort_values(ascending=False)  # sorted from warmest to coldest
        seasons_sorted = df_mean_temp.index.tolist()
        train_seasons = seasons_sorted[:split_seasons_traintest * n_splits] + seasons_sorted[(split_seasons_traintest + 1) * n_splits:]
        test_seasons = seasons_sorted[split_seasons_traintest * n_splits:(split_seasons_traintest + 1) * n_splits]
    elif split_method == 'sequential':
        ## Create 6 blocks of 6 consecutive seasons, use split_seasons_train to select the test block 
        train_seasons = seasons[:split_seasons_traintest * n_splits] + seasons[(split_seasons_traintest + 1) * n_splits:]
        test_seasons = seasons[split_seasons_traintest * n_splits:(split_seasons_traintest + 1) * n_splits]
    assert len(test_seasons) == int(len(seasons) // n_splits)
    assert len(train_seasons) == len(seasons) - len(test_seasons), "Train and test seasons do not match expected lengths."
    print(f"Training seasons: {train_seasons}, test seasons: {test_seasons}")

    if equal_number_obs_per_season:
        min_max_doy = min(list(Counter(df_regression['season']).values()))
        assert min_max_doy >= 145, f"Expected at least 145 observations per season, got {min_max_doy}."
        ## filter out days greater than min_max_doy
        df_regression = df_regression[df_regression['doy'] <= min_max_doy]
        assert len(df_regression) > 0, "No data left after filtering for DOY and seasons."
        assert len(df_regression) == len(seasons) * min_max_doy, "Data length does not match expected number of seasons and DOY."

    df_train = df_regression[df_regression["season"].isin(train_seasons)]
    df_test = df_regression[df_regression["season"].isin(test_seasons)]
    return df_train, df_test

def bayesian_inference(
        split_seasons_traintest=None,
        mcmc_draw_samples=100,
        mcmc_tune_samples=200,
        mcmc_chains=32,
        mcmc_cores=8,
        infer_chilling=False,
        zoned_chilling=False,
        species_sel='Quercus robur L.',
        location_list=None,
        equal_number_obs_per_season=True,
        scale_sigma_by_mu=False,
        split_method='sequential'
        ):
    df_regression = prep_budburst_data_for_regression(species_sel=species_sel, location_list=location_list)
    df_train, df_test = split_data_by_season(df_regression=df_regression,
                                            split_seasons_traintest=split_seasons_traintest, 
                                            equal_number_obs_per_season=equal_number_obs_per_season,
                                            split_method=split_method)

    with pm.Model() as model:
        ## Extract data
        temperature = pm.Data("temperature", df_train['temperature'].values, 
                              mutable=True, dims='obs_id')  # Use MutableData for temperature to allow for dynamic updates
        if not infer_chilling:
           doy = pm.Data('doy', df_train['doy'].values, mutable=True, dims='obs_id')  
        bb_cdf_obs = pm.Data("bb_cdf_obs", df_train['bb_cdf'].values, mutable=True, dims='obs_id')

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
            inds_s = pm.math.where(inds_s)[0]  # Convert boolean mask to indices
            
            gdd_s = t_above_base[inds_s]
            if infer_chilling:
                cum_chill_days = pt.where(temperature[inds_s] < t_base_chill, 1, 0)
                if zoned_chilling:
                    cum_chill_days = pt.where(temperature[inds_s] < t_bottom_chill, 0, cum_chill_days)
                cum_chill_days = pt.cumsum(cum_chill_days)
                gdd_s = pt.where(cum_chill_days >= threshold_cum_chill, gdd_s, 0)
            else:
                doy_s = doy[inds_s]
                gdd_s = pt.where(doy_s >= start_doy, gdd_s, 0)
            gdd_s = pt.cumsum(gdd_s)
            gdd = pt.set_subtensor(gdd[inds_s], gdd_s)  # Alternative if gdd is also a tensor

        ## Logistic function: maps GDD to cumulative fraction
        alpha = pm.Normal("alpha", mu=0, sigma=10)  # Intercept
        beta = pm.LogNormal("beta", mu=0, sigma=1)  # Slope
        mu = pm.Deterministic("mu", pm.math.sigmoid(alpha + beta * gdd))  # Sigmoid function
        
        ## Likelihood: Normal distribution with uncertainty
        sigma = pm.HalfNormal("sigma", sigma=0.1)
        if scale_sigma_by_mu:
            ## sigma * (1 - 2 * np.abs(mu - 0.5)) + 1e-3
            eps = 5e-2
            # std = pm.Deterministic("sigma_scaled", pt.sqr(1 - 2 * pt.abs(mu - 0.5)) * sigma + eps)  # Avoid division by zero
            std = pm.Deterministic("sigma_scaled",  (mu * (1 - mu)) ** 2 * sigma + eps)  # Avoid division by zero
        else:
            std = sigma
        bb_cdf_likelihood = pm.Normal("bb_cdf", mu=mu, sigma=std, 
                                      shape=temperature.shape, observed=bb_cdf_obs, dims='obs_id')

        # Sample posterior
        trace = pm.sample(draws=mcmc_draw_samples, tune=mcmc_tune_samples, 
                          chains=mcmc_chains, cores=mcmc_cores, 
                          idata_kwargs={"log_likelihood": True},
                          return_inferencedata=True)
        
    return trace, df_train, df_test, model

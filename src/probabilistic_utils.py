import os, sys
import numpy as np 
import pandas as pd 
from datetime import datetime
import pymc as pm 
import pytensor
import pytensor.tensor as pt
from collections import Counter
from dataloadermaker import DataLoaderMaker
from collections import namedtuple

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
        print('No dataframes provided -- using default budburst and temperature data')
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

def get_path_repo():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    script_dir = script_dir.split('/')
    new_dir = ''
    for i in range(len(script_dir)):
        new_dir += script_dir[i] + '/'
        if script_dir[i] == 'bayesian-budburst':
            break
    return new_dir

def load_and_prep_moth_data(fp_path=None, location_list=['HV']):
    if fp_path is None:
        new_dir = get_path_repo()        
        fp_path = os.path.join(new_dir, '1994-2019_field-d50.csv')
        assert os.path.exists(fp_path), f"File {fp_path} does not exist. Please provide a valid path to the moth data file."
    try:
        df_moth = pd.read_csv(fp_path, header=0, sep=';')
    except pd.errors.ParserError:
        print(f"ParserError: Trying to read {fp_path} with custom column names.")
        if fp_path.endswith('1994-2019_field-d50.csv'):
            col_names = ['YearCatch', 'YearHatch', 'AreaShortName', 'Site', 'Tree', 'NovemberDate', 'TubeNumber', 'D50Calc']
        else:
            raise ValueError(f"Unknown file format for {fp_path}. Please provide the correct column names.")
        df_moth = pd.read_csv(fp_path, names=col_names, sep=';', skiprows=1)
    if fp_path.endswith('1994-2019_field-d50.csv'):
        assert len(df_moth) == 5792, "Expected 5792 rows in the moth data from excel file, but found a different number."
    else:
        print(f'THere are {len(df_moth)} rows in the moth data from {fp_path}')
    df_moth = df_moth[df_moth['AreaShortName'].isin(location_list)]
    nan_rows = df_moth[df_moth['D50Calc'].isna()]
    if len(nan_rows) > 0:
        print(f"Warning: There are {len(nan_rows)} rows with NaN D50Calc values. These rows will be removed.")
        df_moth = df_moth[~df_moth['D50Calc'].isna()]
    # Calculate DOY_hatch as the day of year for April in YearHatch
    df_moth['D50Calc'] = df_moth['D50Calc'].apply(lambda x: float(str(x).replace(',', '.')))
    df_moth['D50Calc'] = df_moth['D50Calc'].round(0).astype(int)
    df_moth['MonthHatch'] = -1 
    df_moth['DayHatch'] = -1
    for id, row in df_moth.iterrows():
        if row['D50Calc'] < 61 and row['D50Calc'] > 30:
            df_moth.loc[id, 'MonthHatch'] = 5
            df_moth.loc[id, 'DayHatch'] = row['D50Calc'] - 30
        elif row['D50Calc'] < 1 and row['D50Calc'] > -30:
            df_moth.loc[id, 'MonthHatch'] = 3
            df_moth.loc[id, 'DayHatch'] = 31 + row['D50Calc']
        elif row['D50Calc'] < 31 and row['D50Calc'] > 0:
            df_moth.loc[id, 'MonthHatch'] = 4
            df_moth.loc[id, 'DayHatch'] = row['D50Calc']
        else:
            assert False, f"Unexpected D50Calc value {row['D50Calc']} for row {id}: {row}"
    df_moth['DOY_hatch'] = pd.to_datetime(dict(year=df_moth['YearHatch'], month=df_moth['MonthHatch'], day=df_moth['DayHatch'])).dt.dayofyear
    
    df_moth = df_moth[['YearHatch', 'DOY_hatch', 'AreaShortName', 'Site', 'Tree']]
    # print(f"Loaded moth data from {fp_path}, shape: {df_moth.shape}")
    return df_moth

def prep_moth_data_for_regression(moth_df=None, temp_df=None, dir_moth_data=None, file_name=None, location_list=['HV'], verbose=1):
    if temp_df is None or moth_df is None:
        VeluweTreeData = DataLoaderMaker()
        VeluweTreeData.load()
        temp_df = VeluweTreeData.get("temp_climwin_input")

        if dir_moth_data is None:
            fp_moth_csv = None 
        else:
            if file_name is None:
                file_name = '1994-2019_field-d50.csv'
            fp_moth_csv = os.path.join(dir_moth_data, file_name)
        moth_df = load_and_prep_moth_data(fp_path=fp_moth_csv, location_list=location_list)

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
        moth_cdf = np.arange(len(arr_doy)) / float(len(arr_doy) - 1)
        arr_doy = np.round(arr_doy).astype(int)
        moth_cdf = pd.DataFrame({'doy': arr_doy, 'moth_cdf': moth_cdf}).groupby('doy').max().reset_index()  # take max cdf per day
        moth_cdf['date'] = pd.to_datetime(moth_cdf['doy'], format='%j').dt.tz_localize('UTC') + pd.offsets.DateOffset(years=y - 1900)  ## 1900 is default start year for pd dt
        
        temp_sel = temp_df[((temp_df['date'].dt.year == y) & 
                            (temp_df['date'].dt.day_of_year < season_start_doy)) | 
                            ((temp_df['date'].dt.year == y-1) & 
                            (temp_df['date'].dt.day_of_year >= season_start_doy))].copy()

        if len(temp_sel) == 0:
            print(f"No temperature data for year {y} - removing MOTH data for this year.")
            continue

        temp_sel = temp_sel[['date', 'temperature']].groupby('date').mean().reset_index()
        temp_sel = temp_sel.rename(columns={'date': 'date_temp'})
        moth_cdf = moth_cdf.rename(columns={'date': 'date_moth'})
        regression_df = pd.merge(moth_cdf, temp_sel, left_on='date_moth', right_on='date_temp', how='right')
        del regression_df['date_moth']
        del regression_df['doy']
        regression_df = regression_df.rename(columns={'date_temp': 'date'})
        regression_df['moth_cdf'] = regression_df['moth_cdf'].ffill().fillna(0)  # forward fill missing values (eg days without cdf vals get previous day cdf), then fill all days before 1st measurement with 0
        regression_df['date'] = pd.to_datetime(regression_df['date'], format='%Y-%m-%d')

        s = f'{y-1}-{y}'
        regression_df['season'] = s

        dict_data_per_year[s] = regression_df

    regression_df = pd.concat(dict_data_per_year.values(), ignore_index=True)
    return regression_df


def load_and_prep_bb_int_data(folder_bb_int='/Users/tplas/data/2026-05-11 budburst international/',
                          include_photoperiod=True):
    assert os.path.exists(folder_bb_int), f"Folder {folder_bb_int} does not exist. Please check the path."
    csv_file = os.path.join(folder_bb_int, '20260430_budburst_temp_photoperiod.csv')
    assert os.path.exists(csv_file), f"File {csv_file} does not exist. Please check the path."
    df_all = pd.read_csv(csv_file)

    dict_df_bb_int = {}
    coord_dict = {}
    ## create named tuple with lat lon and s_id 
    tuple_site_info = namedtuple('CountryData', ['lat', 'lon', 's_id', 'country'])
    for s_id in df_all.s_id.unique():
        s_id = int(s_id)
        country = df_all[df_all.s_id == s_id].country.unique()[0]
        tmp = df_all[df_all.s_id == s_id].copy()
        assert tmp.lon.nunique() == 1, f"Expected only one unique longitude for country {country}, but found {tmp.lon.unique()}."
        assert tmp.lat.nunique() == 1, f"Expected only one unique latitude for country {country}, but found {tmp.lat.unique()}."
        assert tmp.s_id.nunique() == 1, f"Expected only one unique s_id for country {country}, but found {tmp.s_id.unique()}." 
        coord_dict[s_id] = tuple_site_info(lat=tmp.lat.unique()[0], lon=tmp.lon.unique()[0], s_id=tmp.s_id.unique()[0], country=country)
        
        tmp = tmp.drop(columns=['lat', 'lon', 's_id', 'country']).copy()
        ## convert date to datetime and extract day of year
        tmp['date'] = pd.to_datetime(tmp['date'], format=r"%d/%m/%Y")
        tmp['doy'] = tmp['date'].dt.dayofyear
        tmp['bb_cdf'] = 0.0
        
        for year in tmp.year.unique():
            tmp_year = tmp[tmp.year == year]
            if tmp_year.empty:
                continue
            doy_bb = tmp_year['budburst_day'].values[0]
            if pd.isna(doy_bb):
                continue
            tmp.loc[(tmp.year == year) & (tmp.doy >= doy_bb) & (tmp.doy < 300), 'bb_cdf'] = 1.0

        tmp['season'] = (tmp['year'] - 1).astype('str') + '-' + tmp['year'].astype('str')
        tmp = tmp.rename(columns={'mean_temperature': 'temperature'})
        tmp = tmp.sort_values(by=['year', 'date']).reset_index(drop=True)
        if include_photoperiod:
            list_cols = ['bb_cdf', 'date', 'temperature', 'photoperiod', 'season', 'doy']
        else:
            list_cols = ['bb_cdf', 'date', 'temperature', 'season', 'doy']
        tmp = tmp[list_cols].copy()

        dict_df_bb_int[s_id] = tmp.copy()
    return dict_df_bb_int, coord_dict

def split_data_by_season(df_regression, split_seasons_traintest: int, 
                         equal_number_obs_per_season=True, split_method='sequential', n_splits=6):
    assert split_method in ['sequential', 'mean_temperature']
    df_regression['doy'] = df_regression['date'].dt.day_of_year
    df_regression = df_regression[df_regression['date'].dt.month < 7]  # delete Dec effectively, just to make DOY prior easier to deal with 
    seasons = sorted(df_regression["season"].unique())
    n_seasons_per_split = len(seasons) // n_splits
    # assert len(seasons) == 36, f"Expected 36 seasons, got {len(seasons)}"
    # assert int(n_seasons_per_split * n_splits) == len(seasons), f"Expected {len(seasons)} seasons, got {int(n_seasons_per_split * n_splits)}"
    assert type(split_seasons_traintest) == int and split_seasons_traintest in np.arange(n_splits), f"split_seasons_train must be in {np.arange(n_splits)}, got {split_seasons_traintest}"
    if split_method == 'mean_temperature':
        df_mean_temp = df_regression[df_regression['date'].dt.month <= 4].groupby('season')['temperature'].mean().sort_values(ascending=False)  # sorted from warmest to coldest
        seasons_use = df_mean_temp.index.tolist() # sorted from warmest to coldest
    elif split_method == 'sequential':
        ## Create 6 blocks of 6 consecutive seasons, use split_seasons_train to select the test block 
        seasons_use = seasons
    train_seasons = seasons_use[:split_seasons_traintest * n_seasons_per_split] + seasons_use[(split_seasons_traintest + 1) * n_seasons_per_split:]
    test_seasons = seasons_use[split_seasons_traintest * n_seasons_per_split:(split_seasons_traintest + 1) * n_seasons_per_split]
    # print(f'Splitting seasons: {len(seasons_use)} total seasons, {len(train_seasons)} train seasons, {len(test_seasons)} test seasons')
    assert len(test_seasons) == int(len(seasons_use) // n_splits), f'Expected {len(seasons_use) // n_splits} test seasons, got {len(test_seasons)}'
    assert len(train_seasons) == len(seasons_use) - len(test_seasons), f'Expected {len(seasons_use) - len(test_seasons)} train seasons, got {len(train_seasons)}'
    print(f"Training seasons: {train_seasons}, test seasons: {test_seasons}")

    if equal_number_obs_per_season:
        min_max_doy = min(list(Counter(df_regression['season']).values()))
        assert min_max_doy >= 145, f"Expected at least 145 observations per season, got {min_max_doy}."
        ## filter out days greater than min_max_doy
        df_regression = df_regression[df_regression['doy'] <= min_max_doy]
        assert len(df_regression) > 0, "No data left after filtering for DOY and seasons."
        assert len(df_regression) == len(seasons_use) * min_max_doy, "Data length does not match expected number of seasons and DOY."

    df_train = df_regression[df_regression["season"].isin(train_seasons)]
    df_test = df_regression[df_regression["season"].isin(test_seasons)]
    return df_train, df_test

def bayesian_inference(
        data_type='budburst',
        split_seasons_traintest=None,
        mcmc_draw_samples=100,
        mcmc_tune_samples=200,
        mcmc_chains=32,
        mcmc_cores=8,
        infer_chilling=False,
        zoned_chilling=False,
        photoperiod=False,
        species_sel='Quercus robur L.',
        location_list=None,
        equal_number_obs_per_season=True,
        scale_sigma_by_mu=False,
        split_method='sequential',
        n_splits=6,
        dir_moth_data=None
        ):
    if data_type == 'moth':
        df_regression = prep_moth_data_for_regression(location_list=location_list, dir_moth_data=dir_moth_data,
                                                      file_name='Qry_D50_AllClutches_CommonGarden.csv')
        name_cdf = 'moth_cdf'
    elif data_type == 'budburst':
        df_regression = prep_budburst_data_for_regression(species_sel=species_sel, location_list=location_list)
        name_cdf = 'bb_cdf'
    else:
        raise ValueError(f"Unknown data_type {data_type}. Use 'budburst' or 'moth'.")
    df_train, df_test = split_data_by_season(df_regression=df_regression,
                                            split_seasons_traintest=split_seasons_traintest, 
                                            equal_number_obs_per_season=equal_number_obs_per_season,
                                            split_method=split_method, n_splits=n_splits)

    with pm.Model() as model:
        ## Extract data
        temperature = pm.Data("temperature", df_train['temperature'].values, 
                              mutable=True, dims='obs_id')  # Use MutableData for temperature to allow for dynamic updates
        if not infer_chilling:
           doy = pm.Data('doy', df_train['doy'].values, mutable=True, dims='obs_id')  
        bb_cdf_obs = pm.Data("bb_cdf_obs", df_train[name_cdf].values, mutable=True, dims='obs_id')

        ## Define priors
        t_base_force = pm.Normal("t_base_force", mu=5, sigma=2)  # Prior for base temperature
        if infer_chilling:
            threshold_cum_chill = pm.DiscreteUniform("threshold_cum_chill", lower=0, upper=50)  # Prior for chilling threshold
            t_base_chill = pm.Normal("t_base_chill", mu=5, sigma=2)  # Prior for chilling base temperature
            if zoned_chilling:
                t_bottom_chill = pm.Normal("t_bottom_chill", mu=0, sigma=2)  # Prior for chilling bottom temperature
        else:
            start_doy = pm.DiscreteUniform("start_date", lower=60, upper=100)  # Prior for GDD start date
        if photoperiod:
            delta_light = sigma = pm.HalfNormal("delta_light", sigma=0.1)
        
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
                if photoperiod: ## add delta_light to each chill day 
                    cum_chill_days = cum_chill_days + delta_light
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
            eps = 1e-2
            # std = pm.Deterministic("sigma_scaled", pt.sqr(1 - 2 * pt.abs(mu - 0.5)) * sigma + eps)  # Avoid division by zero
            std = pm.Deterministic("sigma_scaled",  (mu * (1 - mu)) ** 1 * sigma + eps)  # Avoid division by zero
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
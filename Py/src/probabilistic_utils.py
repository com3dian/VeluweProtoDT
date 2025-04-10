import os, sys
import numpy as np 
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, WhiteKernel
import pymc as pm 
import pytensor as pt

from dataloadermaker import DataLoaderMaker

def prep_data_for_regression(budburst_df, temp_df, 
                             t_base=4, gdd_month_day_start=None, 
                             species_sel='Quercus robur L.'):
    """
    Prepares the data for regression analysis by merging the budburst and temperature dataframes.

    Parameters:
    - budburst_df: DataFrame containing budburst data.
    - temp_df: DataFrame containing temperature data.
    - t_base: Base temperature for growing degree days (default is 4).

    Returns:
    - regression_df: DataFrame containing merged data for regression analysis.
        Use winter seasons as years (Nov - June). Create DFs for each year separately.
        Each row is one DOY
        Each column is one feature (temperature, GDD, bb_frac).
    """
    assert species_sel in budburst_df['species'].unique(), f"Species {species_sel} not found in budburst data {budburst_df['species'].unique()}" 
    season_start_doy = 250 
    dict_data_per_year = {}
    years = sorted(budburst_df['year'].unique())

    for y in years:
        bb_sel = budburst_df[np.logical_and(budburst_df['year'] == y, 
                                           budburst_df['species'] == species_sel)]
        if len(bb_sel) == 0:
            print(f"No data for year {y}")
            continue
        arr_doy = bb_sel.bud_burst_DOY.values
        arr_doy = np.sort(arr_doy)
        bb_cdf = np.arange(len(arr_doy)) / float(len(arr_doy) - 1) ## there can be duplicate doys, so we need the maximum the budburst fraction per doy
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

        ## Calculate GDD
        t_above_base = np.maximum(0, regression_df['temperature'] - t_base)
        regression_df['t_above_base'] = t_above_base
        if gdd_month_day_start is not None:
            assert type(gdd_month_day_start) == tuple, f"gdd_month_day_start {gdd_month_day_start} is not a tuple"
            assert len(gdd_month_day_start) == 2, f"gdd_month_day_start {gdd_month_day_start} is not a tuple of length 2"
            gdd_month, gdd_day = gdd_month_day_start
            assert type(gdd_month) == int and type(gdd_day) == int, f"gdd_month_day_start {gdd_month_day_start} is not a tuple of integers"
            assert gdd_month >= 1 and gdd_month <= 12, f"gdd_month {gdd_month} is not between 1 and 12"
            assert gdd_day >= 1 and gdd_day <= 31, f"gdd_day {gdd_day} is not between 1 and 31"
            if gdd_month > 9:
                gdd_start = datetime(year=y - 1, month=gdd_month, day=gdd_day, tzinfo=regression_df['date'][0].tzinfo)
            else:
                gdd_start = datetime(year=y, month=gdd_month, day=gdd_day, tzinfo=regression_df['date'][0].tzinfo)
        else:
            gdd_doy_start = season_start_doy
            gdd_start = datetime(year=y - 1, month=1, day=1, tzinfo=regression_df['date'][0].tzinfo) + pd.offsets.DateOffset(days=gdd_doy_start - 1)
        gdd_start = pd.to_datetime(gdd_start, format='%Y-%m-%d')
        gdd_start = max(gdd_start, regression_df['date'].min())
        
        ## gdd is cumulative t_above_base from gdd_start to date
        regression_df['gdd'] = regression_df['t_above_base'].cumsum()
        gdd_on_start_day = regression_df[regression_df['date'] == gdd_start]['gdd']
        assert len(gdd_on_start_day) == 1, f"gdd_on_start_day {gdd_on_start_day} not unique"
        gdd_on_start_day = gdd_on_start_day.values[0]
        regression_df['gdd'] = regression_df['gdd'] - gdd_on_start_day
        if regression_df['gdd'].dtype == pt.tensor.TensorVariable:
            regression_df['gdd'] = pm.math.maximum(0, regression_df['gdd'])
        else:
            regression_df['gdd'] = np.maximum(0, regression_df['gdd'].values)
        del regression_df['t_above_base']
        s = f'{y-1}-{y}'
        regression_df['season'] = s

        dict_data_per_year[s] = regression_df
    ## concatenate all years
    regression_df = pd.concat(dict_data_per_year.values(), ignore_index=True)
    return regression_df



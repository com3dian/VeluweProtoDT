import os, sys
import numpy as np 
import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime

from dataloadermaker import DataLoaderMaker

def plot_bb_one_year(budburst_df, temp_df, ax=None, 
                  y=2015, plot_mean_std=True, plot_cdf=True):
    
    ub = y + 0.75
    lb = y - 0.25
    min_temp = -12
    max_temp = 25


    df_bb_mini = budburst_df[['TreeID', 'bud_burst_DOY', 'year', 'species']]
    df_bb_sel = df_bb_mini[df_bb_mini['year'] == y]
    if len(df_bb_sel) == 0:
        ax.axis('off')
        return ax
    
    temp_df_plot = temp_df.copy()
    temp_df_plot['year_decimal'] = temp_df_plot['date'].dt.year + (temp_df_plot['date'].dt.dayofyear - 1) / 365
    temp_df_plot = temp_df_plot[['year_decimal', 'temperature']]
    temp_df_plot['temperature'] = temp_df_plot['temperature'].astype(float)
    temp_df_plot = temp_df_plot.groupby('year_decimal').mean().reset_index()

    if ax is None:
        ax = plt.subplot(111)

    ax = temp_df_plot[np.logical_and(temp_df_plot['year_decimal'] >= lb,
                                temp_df_plot['year_decimal'] < ub)].plot(
                                    x='year_decimal', y='temperature', color='k', linestyle='', 
                                    marker='o', alpha=0.7, markersize=2, ax=ax
                                    )
    ax.set_ylabel('Temperature (C)') 
    ax.set_xlim(lb + 0.1, ub - 0.3)
    ax.set_xticks([y + xx for xx in [-0.1, 0, 0.1, 0.2, 0.3, 0.4]])
    ax.set_xticklabels(['Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May'])
    ax.set_xlabel('')
    ax.annotate(f"{y}, N={len(df_bb_sel)}", xy=(0.03, 0.9), xycoords='axes fraction', ha='left', weight='bold')
    ax.get_legend().remove()
    ax.set_ylim([min_temp, max_temp])

    if plot_mean_std:
        mean, std = df_bb_sel['bud_burst_DOY'].mean(), df_bb_sel['bud_burst_DOY'].std()
        mean_y_dec = y + (mean - 1) / 365
        std_dec = std / 365
        ax.axvline(mean_y_dec, color='blue', linestyle='--', label='Mean')
        ax.axvline(mean_y_dec + std_dec, color='blue', linestyle=':', label='Std')
        ax.axvline(mean_y_dec - std_dec, color='blue', linestyle=':', label='Std')

    if plot_cdf:
        CDF_PER_SPECIES = False
        if CDF_PER_SPECIES:
            for sp in df_bb_sel.species.unique():
                arr_doy = df_bb_sel[np.logical_and(df_bb_sel['year'] == y, df_bb_sel['species'] == sp)].bud_burst_DOY.values
                arr_doy = np.sort(arr_doy)
                yvals = np.arange(len(arr_doy)) / float(len(arr_doy) - 1)
                arr_doy_dec = y + (arr_doy - 1) / 365
                yvals_normalised = yvals * (max_temp - min_temp) + min_temp
                ax.plot(arr_doy_dec, yvals_normalised, linestyle='-', alpha=0.5)
        else:
            arr_doy = df_bb_sel[np.logical_and(df_bb_sel['year'] == y, df_bb_sel['species'] == 'Quercus robur L.')].bud_burst_DOY.values
            # arr_doy = df_bb_mini[df_bb_mini['year'] == y].bud_burst_DOY.values
            arr_doy = np.sort(arr_doy)
            yvals = np.arange(len(arr_doy)) / float(len(arr_doy) - 1)
            arr_doy_dec = y + (arr_doy - 1) / 365
            yvals_normalised = yvals * (max_temp - min_temp) + min_temp
            ax.plot(arr_doy_dec, yvals_normalised, color='blue', linestyle='-', alpha=0.5)

    return ax, (df_bb_sel, temp_df_plot) 

def plot_bb_all_years(budburst_df, temp_df, sort_by_bb_doy=False):

    if sort_by_bb_doy:
        years = budburst_df[['TreeID', 'bud_burst_DOY', 'year']].groupby('year').mean().sort_values('bud_burst_DOY', ascending=False).reset_index()['year']
    else:
        years = np.arange(budburst_df['year'].min(), budburst_df['year'].max() + 1, 1)
    
    fig, ax = plt.subplots(len(years) // 5 + 1, 5, figsize=(21, len(years) / 2), 
                        gridspec_kw={'hspace': 0.3, 'wspace': 0.3})
    for i, y in enumerate(years):
        curr_ax = plot_bb_one_year(budburst_df=budburst_df, temp_df=temp_df, y=y, 
                                   ax=np.ravel(ax)[i], plot_mean_std=False, plot_cdf=True)

    ## remove empty subplots
    for j in range(i + 1, len(np.ravel(ax))):
        curr_ax = np.ravel(ax)[j]
        curr_ax.axis('off')

    return (fig, ax)

def plot_full_timeseries(budburst_df, temp_df, ax=None):
    df_bb_mini = budburst_df[['TreeID', 'bud_burst_DOY', 'year', 'species']]
    if ax is None:
        fig, ax = plt.subplots(1,1, figsize=(20, 4))
    sns.lineplot(data=df_bb_mini, x='year', y='bud_burst_DOY', errorbar='ci', ax=ax)
    temp_df_plot = temp_df.copy()
    temp_df_plot['year_decimal'] = temp_df_plot['date'].dt.year + (temp_df_plot['date'].dt.dayofyear - 1) / 365
    temp_df_plot = temp_df_plot[['year_decimal', 'temperature']]
    temp_df_plot['temperature'] = temp_df_plot['temperature'].astype(float)
    temp_df_plot = temp_df_plot.groupby('year_decimal').mean().reset_index()
    ax2 = ax.twinx()
    temp_df_plot.plot(x='year_decimal', y='temperature', color='orange', ax=ax2, linestyle='', marker='o', alpha=0.5, markersize=2)
    ax2.set_ylabel('Temperature (C)')
    ax.set_ylabel('Budburst DOY')
    ax.set_xlabel('Year')

    return (fig, ax, ax2)
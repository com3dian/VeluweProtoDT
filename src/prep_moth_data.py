import os, sys
import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns
from tqdm import tqdm
from scipy.optimize import curve_fit

def load_processed_moth_data(folder='/Users/tplas/data/2025-10-07 moth raw data 2023/', 
                             file='Qry_D50_AllClutches_CommonGarden.xlsx'):
    '''Eg 1994-2019_field-d50.csv or Qry_D50_AllClutches_CommonGarden.xlsx'''
    full_path = os.path.join(folder, file)
    assert os.path.exists(full_path), f"File not found: {full_path}"
    if full_path.endswith('.csv'):
        df_proc = pd.read_csv(full_path, sep=';')
    elif full_path.endswith('.xlsx'):
        df_proc = pd.read_excel(full_path)

    assert 'TubeID' in df_proc.columns, "Expected column 'TubeID' not found in data."

    val_count_series = df_proc['TubeID'].value_counts()
    # df_plot = df_proc[df_proc.TubeID.isin(val_count_series[val_count_series == 1].index.to_list())]
    if any(val_count_series > 1):
        print(f"Warning: There are {sum(val_count_series > 1)} TubeID(s) with multiple entries.")

    return df_proc

def load_raw_moth_data(folder='/Users/tplas/data/2025-10-07 moth raw data 2023/', 
                      file='Qry_NumberOfCaterpillarsPerClutchPerDay.txt'):
    fp_raw_moth_data = os.path.join(folder, file)
    assert os.path.exists(fp_raw_moth_data)

    ## read as csv 
    df_raw = pd.read_csv(fp_raw_moth_data, sep=';')

    return df_raw

def add_relative_caterpillar_counts(df, remove_zero_tubes=True, min_num_caterpillars=10):
    ''''''
    df = df.copy()
    ## Add total caterpillars per tube:
    df_cp_per_tube = df.groupby(['TubeID']).agg({'Caterpillars':'sum'}).reset_index()
    df['TotalCaterpillarsPerTube'] = df['TubeID'].map(df_cp_per_tube.set_index('TubeID')['Caterpillars'])
    if remove_zero_tubes:
        df = df[df['TotalCaterpillarsPerTube'] > 0]
    if min_num_caterpillars is not None:
        df = df[df['TotalCaterpillarsPerTube'] >= min_num_caterpillars]

    ## Add (cumulative) relative caterpillar counts per tube:
    df['RelativeCaterpillarsPerTube'] = df['Caterpillars'] / df['TotalCaterpillarsPerTube']
    df['CumulativeRelativeCaterpillarsPerTube'] = df.groupby('TubeID')['RelativeCaterpillarsPerTube'].cumsum()
    return df 

def calculate_d50_linear_interpolation(df, column='Caterpillars', verbose=0):
    assert 'TubeID' in df.columns and column in df.columns and 'AprilDay' in df.columns
    max_cp = int(df[column].max())
    if max_cp == 0.0:
        if verbose:
            print("WARNING: max_cp is 0.0"
                  f" for TubeID {df.TubeID.values[0]}")
        return np.nan
    
    if column == 'RelativeCaterpillarsPerTube':
        half_cp = 0.5
        max_cp = 1.0
    else:
        half_cp = max_cp / 2

    ## get the first day where the caterpillars are above half_cp
    df_half_top = df[df[column] >= half_cp]
    day_top = df_half_top.AprilDay.min()
    count_top = df_half_top[df_half_top.AprilDay == day_top][column].values[0]
    # count_top = df_half_top[column].min()
    # day_top = df_half_top[df_half_top[column] == count_top].AprilDay.values[0]

    ## get the last day where the caterpillars are below half_cp
    df_half_bottom = df[df[column] <= half_cp]
    if df_half_bottom.shape[0] == 0 or df_half_bottom[column].max() == 0.0:
        if verbose:
            print("WARNING: no bottom half found"
                f" for TubeID {df.TubeID.values[0]} with max_cp {max_cp}")
        return np.nan
    else:
        count_bottom = df_half_bottom[column].max()
        day_bottom = df_half_bottom[df_half_bottom[column] == count_bottom].AprilDay.values[0]

    if verbose:
        print(f"TubeID {df.TubeID.values[0]}: max_cp {max_cp}, half_cp {half_cp}, "
              f"day_bottom {day_bottom} (count_bottom {count_bottom}), "
              f"day_top {day_top} (count_top {count_top})")
        
    if count_top == count_bottom:
        if verbose:
            print(f"count_top == count_bottom for TubeID {df.TubeID.values[0]} with max_cp {max_cp}")
        return day_top
    
    # linear interpolation to find the day where the count is exactly half_cp
    d50 = day_bottom + (half_cp - count_bottom) * (day_top - day_bottom) / (count_top - count_bottom)
    return float(d50)

def sigmoid(x, x0, k):
    y = 1 / (1 + np.exp(-k*(x - x0)))
    return y

def inv_sigmoid(y, x0, k):
    assert 0 < y < 1, "y must be in (0, 1)"
    return float(x0 + (1.0 / k) * np.log(y / (1 - y)))

def fit_sigmoid_to_caterpillar_data(df, column='CumulativeRelativeCaterpillarsPerTube', verbose=0,
                                    replace_nans=True):
    '''Fit a sigmoid function to the cumulative relative caterpillar counts per tube.
    
    Returns:
        dict with TubeID as keys and fitted parameters (x0, k) as values.
    '''
    tube_ids = df.TubeID.unique()
    print(f'Fitting sigmoid to {len(tube_ids)} TubeIDs with {len(df)} data points.')
    dict_sigmoid_params = {x: [] for x in ['TubeID', 'x0', 'k']}

    tube_ids_fit = []
    tube_ids_no_low_count = []
    tube_ids_no_high_count = []
    tube_ids_nans = []
    tube_id_fit_fail = []
    ## fit sigmoid to cumulative relative caterpillar counts

    for tube_id in tqdm(tube_ids):
        tube_id = int(tube_id)
        df_tube = df[df.TubeID == tube_id]
        if df_tube.isna().sum().sum() > 0:
            if replace_nans:
                df_tube = df_tube.dropna()
                if df_tube.shape[0] == 0:
                    tube_ids_nans.append(tube_id)
                    if verbose: 
                        print(f"Skipping  TubeID {tube_id} with all NaN values after dropna")
                    continue
            else:
                tube_ids_nans.append(tube_id)
                if verbose: 
                    print(f"Skipping  TubeID {tube_id} with NaN values")
                continue
        max_count = df_tube.CumulativeRelativeCaterpillarsPerTube.max()
        if max_count < 0.8:
            tube_ids_no_high_count.append(tube_id)
            if verbose:
                print(f"Skipping TubeID {tube_id} with max cumulative relative count {max_count}")
            continue
        days_below_half = df_tube[df_tube.CumulativeRelativeCaterpillarsPerTube <= 0.5]
        if len(days_below_half) == 0:
            if verbose:
                print(f"Skipping TubeID {tube_id} with no days below 0.5 cumulative relative count")
            tube_ids_no_low_count.append(tube_id)
            continue

        try:
            popt, pcov = curve_fit(sigmoid, df_tube.AprilDay, df_tube.CumulativeRelativeCaterpillarsPerTube, 
                                p0=[0, 1], maxfev=10000)
            dict_sigmoid_params['TubeID'].append(tube_id)
            dict_sigmoid_params['x0'].append(popt[0])
            dict_sigmoid_params['k'].append(popt[1])
            tube_ids_fit.append(tube_id)
        except RuntimeError as e:
            tube_id_fit_fail.append(tube_id)
            if verbose:
                print(f"Could not fit sigmoid for TubeID {tube_id}: {e}")

    print(f'Success: {len(dict_sigmoid_params["TubeID"])}. NaN: {len(tube_ids_nans)}. No High Count: {len(tube_ids_no_high_count)}. No Low Count: {len(tube_ids_no_low_count)}. Fit Fail: {len(tube_id_fit_fail)}.')

    dict_tube_ids = {
        'fitted': tube_ids_fit,
        'nans': tube_ids_nans,
        'no_high_count': tube_ids_no_high_count,
        'no_low_count': tube_ids_no_low_count,
        'fit_fail': tube_id_fit_fail
    }

    df_sigmoid_params = pd.DataFrame(dict_sigmoid_params)
    df_sigmoid_params['D50_Sigmoid'] = df_sigmoid_params['x0']
    return df_sigmoid_params, dict_tube_ids

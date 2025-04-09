import os
import pandas as pd
import numpy as np
from datetime import datetime

class DataLoaderMaker:
    def __init__(self):
        pass

    def load(self, path = ""):
        if path == "":
            # Get script directory
            script_dir = os.path.dirname(os.path.abspath(__file__))
            # Go up two directories and add 'data' folder
            path = os.path.join(script_dir, '..', '..', 'data')
            # Check if directory exists
            if not os.path.exists(path):
                raise FileNotFoundError(f"Data directory not found at {path}")

        # Get list of all CSV files in path
        csvFiles = [f for f in os.listdir(path) if f.endswith('.csv')]
        
        # Dictionary to store dataframes
        dataframes = {}
        
        # Load each CSV file and store with filename as variable name
        for csvFile in csvFiles:
            # Get name without .csv extension to use as variable name
            varName = os.path.splitext(csvFile)[0]
            
            # Read CSV into dataframe
            filePath = os.path.join(path, csvFile)
            df = pd.read_csv(filePath)
            
            # Store in dictionary
            dataframes[varName] = df
            
        self.dataframes = dataframes
        self.datasets = {}

    def get(self, varName):
        return self.dataframes[varName]
    
    def getDataset(self, varName):
        return self.datasets[varName]
    
    def makeBudBurstDataset(self):
        eventDF = self.get("event")
        occurrenceDF = self.get("occurrence")
        extendedmeasurementorfactDF = self.get("extendedmeasurementorfact")
        EventOccurrenceDF = pd.merge(eventDF, occurrenceDF, on="eventID", how="inner")

        fullEventDF = pd.merge(EventOccurrenceDF, extendedmeasurementorfactDF, on="eventID", how="inner")
        renamedEventDF = fullEventDF.rename(columns={
            "eventDate": "BudBurstDate",
            "decimalLatitude": "Latitude",
            "decimalLongitude": "Longitude",
            "verbatimLocality": "Location",
            "organismID": "TreeID",
            "scientificName": "Species",
            "measurementType": "MeasurementType",
            "measurementValue": "BudBurstScore"
        })

        self.dataframes["budburstScore"] = renamedEventDF

        # Define bud burst criteria
        criterionMeasurementType = "bud burst stage (PO:0025532) of the tree crown"
        criterionMeasurementValue = 1

        # Filter for tree crown measurements
        crown = renamedEventDF[renamedEventDF["MeasurementType"] == criterionMeasurementType]

        # Convert BudBurstDate to datetime and calculate Day of Year (DOY)
        crown["BudBurstDate"] = pd.to_datetime(crown["BudBurstDate"])
        crown["DOY"] = crown["BudBurstDate"].dt.dayofyear

        # Find the earliest date where BudBurstScore >= criterionMeasurementValue
        minAboveCriterion = (
            crown[crown["BudBurstScore"] >= criterionMeasurementValue]
            .groupby(["year", "TreeID"])
            .agg(min_DOY_above_criterion=("DOY", "min"), min_value=("BudBurstScore", "first"))
            .reset_index()
        )

        # Join minAboveCriterion back to d_bb_crown
        crown2 = pd.merge(crown, minAboveCriterion, on=["year", "TreeID"], how="left")

        # Find the latest date where BudBurstScore < crit_measValue and DOY < min_DOY_above_criterion
        maxBelowCriterion = (
            crown2[
                (crown2["BudBurstScore"] < criterionMeasurementValue) & 
                (crown2["DOY"] < crown2["min_DOY_above_criterion"])
            ]
            .groupby(["year", "TreeID"])
            .agg(max_DOY_below_criterion=("DOY", "max"), max_value=("BudBurstScore", "first"))
            .reset_index()
        )

        # Merge min_bb and max_bb
        minMaxTable = pd.merge(minAboveCriterion, maxBelowCriterion, on=["year", "TreeID"], how="left")

        # For trees where stage 1 was NOT observed during a visit (interpolate)
        interpolated = minMaxTable[minMaxTable["min_value"] != criterionMeasurementValue].copy()

        # Calculate interpolation values
        interpolated["diff_date"] = interpolated["min_DOY_above_criterion"] - interpolated["max_DOY_below_criterion"]
        interpolated["diff_value"] = interpolated["min_value"] - interpolated["max_value"]
        interpolated["value_per_day"] = interpolated["diff_value"] / interpolated["diff_date"]
        interpolated["days_to_reach_criterion"] = (criterionMeasurementValue - interpolated["max_value"]) / interpolated["value_per_day"]
        interpolated["interpolated_DOY"] = interpolated["max_DOY_below_criterion"] + interpolated["days_to_reach_criterion"]

        # Handle NaN values in interpolated_DOY
        interpolated = interpolated.dropna(subset=["interpolated_DOY"])  # Option 1: Drop NaN rows

        # Convert interpolated_DOY to integers
        interpolated["interpolated_DOY"] = interpolated["interpolated_DOY"].astype(int)

        # Apply the conversion using a lambda that has access to the full row
        interpolated["bud_burst_date"] = interpolated.apply(
            lambda x: datetime.strptime(f"{int(x.year)}-{int(x.interpolated_DOY)}", "%Y-%j").date(),
            axis=1
        )

        # Get species information from the original crown dataframe
        species_info = crown[["TreeID", "Species"]].drop_duplicates()
        
        # Merge species information with interpolated dataframe
        interpolated = pd.merge(interpolated, species_info, on="TreeID", how="left")

        # Rename columns
        interpolated = interpolated.rename(columns={
            "interpolated_DOY": "bud_burst_DOY",
            "Species": "species"
        })

        self.dataframes["interpolated"] = interpolated

    def makeSpatioTemporalDataset(self):
        """
        Creates a spatio-temporal dataset by processing temperature data with location information.
        The dataset covers periods from December 1st to May 31st for each year.
        """
        # Get temperature data
        tempDF = self.get("temp_climwin_input")
        
        # Ensure date is in datetime format with UTC timezone
        tempDF['date'] = pd.to_datetime(tempDF['date'])
        if tempDF['date'].dt.tz is None:
            tempDF['date'] = tempDF['date'].dt.tz_localize('UTC')
        
        # Get unique years excluding the first year (need December of previous year)
        years = sorted(tempDF['date'].dt.year.unique())
        years = years[1:]
        
        # Calculate number of years and pre-allocate array
        n_years = len(years)
        # Shape: (years, features[lat,lon,temp,doy], days)
        temp_data = np.zeros((n_years, 4, 182))
        
        for i, year in enumerate(years):
            # Define season dates
            start_date = pd.Timestamp(f"{year-1}-12-01", tz='UTC')
            end_date = pd.Timestamp(f"{year}-05-31", tz='UTC')
            
            # Filter data for current season
            mask = (tempDF['date'] >= start_date) & (tempDF['date'] <= end_date)
            season_df = tempDF[mask].copy()
            
            # Create complete date range and merge
            date_range = pd.date_range(start_date, end_date, freq='D', tz='UTC')
            template_df = pd.DataFrame(index=date_range)
            template_df.index.name = 'date'
            
            season_df = season_df.set_index('date')
            merged_df = template_df.join(season_df)
            
            # Fill missing values
            # merged_df = merged_df.fillna(method='ffill').fillna(method='bfill')
            merged_df = merged_df.ffill().bfill()
            
            # Calculate day of year and adjust December days
            doy = merged_df.index.dayofyear.values
            doy[merged_df.index.month == 12] += 365
            
            # Get data arrays, ensuring 182 days
            lat_data = merged_df['Latitude'].values[:182]
            lon_data = merged_df['Longitude'].values[:182]
            temp_data_season = merged_df['temperature'].values[:182]
            doy_data = doy[:182]
            
            # Store in pre-allocated array
            temp_data[i, 0, :] = lat_data
            temp_data[i, 1, :] = lon_data
            temp_data[i, 2, :] = temp_data_season
            temp_data[i, 3, :] = doy_data
        
        # Store the processed data
        self.datasets['spatio_temporal'] = temp_data
        
        # Store metadata about the dimensions
        self.spatio_temporal_metadata = {
            'years': years,
            'features': ['latitude', 'longitude', 'temperature', 'day_of_year'],
            'shape': temp_data.shape,
            'period': 'Dec 1 to May 31'
        }


    def preapreDataset(self):
        self.makeBudBurstDataset()
        self.makeSpatioTemporalDataset()
        

    def prepareAndSplitTrainingTestDataset(self):
        pass
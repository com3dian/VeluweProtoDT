library(tidyverse)

# --- 1. Read the file ---
site <- "your_site_name"           # change to match
folder_save <- "path/to/your/data" # change to match
file_prefix <- "posterior-predictions_2025-07-17-0757_hoge-veluwe_split1" 

file_path <- file.path(folder_save, paste0(file_prefix, "_station-", site, ".csv"))

df <- read_csv(file_path)

# --- 2. Inspect ---
glimpse(df)      # column names, types, and a few values
# head(df)       # first 6 rows
# summary(df)    # basic stats per column

# --- 3. Select the columns you want (edit this list after inspection) ---
df_selected <- df |>
  select('mean', 'date', 'bb_cdf', 'perc_20', 'perc_80', 'doy')  # replace with your actual column names

# Or drop specific columns instead:
# df_selected <- df |>
#   select(-col_to_drop, -another_to_drop)
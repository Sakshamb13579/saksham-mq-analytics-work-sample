# Victoria Road Crash Data Analysis

## Overview

An individual analytics project examining Victorian road-crash data through
cleaning, integration, visualisation, and interpretation. Three linked source
datasets were used: crash, person, and vehicle records. The analysis covers
roughly 430,000 person records and focuses on temporal patterns, geographic
concentration, and injury severity across the ten highest-volume local
government areas.

## Key finding

Geelong has the highest fatal/serious-injury rate among the selected top ten
local government areas and ranks third by raw crash volume.

## Files

- `code/analysis.R` contains the R cleaning, integration, recoding, and
  visualisation workflow.
- `code/visualisations.ipynb` contains the Python cleaning and visualisation
  work.
- `report/Victoria_Road_Crash_Report.pdf` contains the public report copy.
- `documentation/data_dictionary.xlsx` contains field definitions for the
  source datasets.

## Data note

The original crash, person, and vehicle CSV files are not included because
they are large public source datasets rather than original work products.
The data dictionary is included to document the schema. The code therefore
requires the source data files to be obtained separately and placed in the
expected working location before it can be rerun.

## Methods and tools

- Python: pandas and notebook-based visualisation
- R: tidyverse, data.table, lubridate, and ggplot2
- Data integration using cleaned accident identifiers and documented joins
- Rule-based imputation and category recoding for inconsistent source values

# Execution Steps for Processing the ENUT-I Pipeline

Below is the complete step-by-step workflow required to process the data from absolute zero.

### 1. Run the Expenditures Model Definition
**Script to run:** `data_processing/expenditures.R`
- **What it does**: This script loads the EPF surveys (`base-personas-viii-epf-(stata).dta` and `base-gastos-viii-epf-(stata).dta`). It maps the variables, creates the standard `data/gastos.csv` file, and estimates the Fractional MNL model via Apollo (`FMNL-epf-viii`).
- **Why do it now**: The models and the `.csv` generated here are strict dependencies required later when ENUT time records try to impute their budget shares. 

### 2. Generate the Preliminary ENUT DataFrame
**Script to run:** `Rscript data_processing/data_processing.R --pre`
- **What it does**: Runs the script from the top down to `write_csv(..., "data/raw/ENUT_PRE_WEEKEND_IMPUTATION.csv")` and stops there (the `--pre` flag). 
- **Why do it now**: This reads the raw ENUT-I dataset, generates the basic mapping and prefilters, formats variables, runs Vallejo's outlier detection, and spits out the preliminary dataset. The resulting CSV acts as the direct, clean input required by Python in the next step to figure out covariance profiles.
- *(Note: without `--pre` the script continues and reads the twin matrix, which must match the pre weekend file).*

### 3. Generate the Twin Matrix
**Script to run:** `data_processing/gemelos_matriz.py` (or through the interactive `.ipynb`)
- **What it does**: This loads the freshly minted `ENUT_PRE_WEEKEND_IMPUTATION.csv` file into Python. Using multiprocessing and convex optimization (`cvxpy` + `ECOS` solvers), it finds comparable individuals and calculates the optimal weights for twin matching based heavily on Mahalanobis/Covariance distances.
- **Why do it now**: Because it mathematically calculates the missing proxy variables required for imputing weekend time allocations.
- The script writes `data/raw/matriz_gemelos.csv.gzip`, the file `data_processing.R` reads. Set `TWIN_WORKERS` to limit the number of processes (default: all cores).
- The `fix-processing` changes leave the pre weekend rows and the twin covariates unchanged (10,597 rows, checked), so the existing matrix can be reused and this step skipped.

### 4. Complete the Pipeline
**Script to run:** `Rscript data_processing/data_processing.R` (full run; it repeats the cheap step 2 and then reads the twin matrix)
- **What it does**: Resume execution from `twin_matrix <- read_csv(...)` down to the end of the script.
- **Why do it now**: Armed with the twin matrix file and the pre-computed expenditure models, this script seamlessly imputes missing weekend diary data (`impute_weekend`), merges activities into their 168-hour groupings (`agregar_actividades` which captures the new `Tw` classifications), pulls expenditure budget shares utilizing the Apollo FMNL models (`imputacion_gastos()`), maps translations (`rename_to_english`), and finally automatically spits out the completed `enut-i-raw` and `enut-i` datasets in DTA and CSV formats, with English variable-name copies.

# Opt-In Privacy

This repository contains two simulation demonstrations of opt-in disclosure protections. The first demonstration applies opt-in formal privacy to Decennial Census data. The second demonstration applies sequential data synthesis to American Community Survey microdata.  

## Table of Contents

* [Background](#background)
* [Formally Private Decennial Census](#formally-private-decennial-census)
* [Synthetic ACS](#synthetic-acs)
* [Contact](#contact)

## Background


## Formally Private Decennial Census

The 2010 Census Stateside Public Use Microdata Sample data files and supporting resources can be found on the [Census Bureau website](https://www.census.gov/data/datasets/2010/dec/stateside-pums.html).

1. Run `analysis/01_prep-decennial-resources.qmd` to prepare the PUMS layout file for processing the microdata.
2. Run `analysis/02_prep-decennial.qmd` to read and process the microdata.
3. Run `analysis/03_decennial-exploratory.qmd` to conduct exploratory analysis of the DC PUMS data.
4. Run `analysis/04_decennial-dp-simulations.qmd` to run the full set of scenarios.
5. Run `analysis/05_decennial-dp-evaluation.qmd` to generate and visualize summary evaluation metrics.


## Synthetic ACS

1. This code was run on an r5.x4large AWS instance. This allows the memory-intensive syntheses to run in parallel. Pick an appropriate computing environment. 
2. Run `config.sh` to install needed software.
3. Create an [IPUMS USA](https://usa.ipums.org/usa-action/menu) account. Apply for an [IPUMS API key](https://account.ipums.org/api_keys). Use `set_ipums_api_key("", save = TRUE)` to install the API key. Visit [here](https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html) to learn more about accessing the IPUMS API.
4. Run `analysis/11_prep-acs.qmd` to download and prepare the ACS microdata.
5. Run `analysis/12_synthesize-full-acs.qmd` to synthesize all of the records in the gold standard data set (GSDS). 
6. Run `analysis/13_synthesize-acs.qmd` to run the different scenarios. 
7. Run `analysis/14_evaluate-synthetic-acs.qmd` to calculate utility and disclosure risk metrics.

## Contact

Please contact [Aaron R. Williams](awilliams@urban.org) with questions.

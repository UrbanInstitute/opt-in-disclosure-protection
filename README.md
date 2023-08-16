# Opt-In Privacy

This repository contains two simulation demonstrations of opt-in disclosure proetctions. The first demonstration applies opt-in formal privacy to Decennial Census data. The second demonstration applies sequential data synthesis to American Community Survey microdata.  

## Table of Contents

* [Background](#background)
* [Formally Private Decennial Census](#formally-private-decennial-census)
* [Synthetic ACS](#synthetic-acs)
* [Contact](#contact)

## Background


## Formally Private Decennial Census


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

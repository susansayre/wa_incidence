# Replication archive for Chan and Sayre

## Dependencies
WARNING: Upon downloading the archive and opening the included .Rproj file, the R package `renv` will attempt to install itself if it has not already been installed on the computer. To disable this behavior, delete the included .Rprofile file before opening the .Rproj file.

The R packages used in the analysis and the versions used are listed in the `renv.lock` file. These packages can all be installed by running `renv::restore()`. Note that `renv` creates a project specific package library, so packages previously installed on the computer will not work in this project without being explicitly installed here.

The tidycensus R package requires users to request a census api key and store it in their .Renviron file. Follow the directions from the tidycensus package [here](https://walker-data.com/tidycensus/articles/basic-usage.html) to accomplish this. You should only need to do this once.

## targets package
The code use the `targets` package to create a reproducible pipeline of targets, although the individual functions can be used independently if users want to avoid targets. Doing so would require converting the targets workflow to a regular script.

The _targets.R file in the root constructs the pipelines. The file calls `R/target_factory_functions.R` which uses the `tarchetypes` packages to construct groups of targets that accomplish particular tasks based on the user options.

The main functions used in the analysis are included in the individual R scripts in the R/ subfolder. Many of these functions are designed to be generic for similar types of analysis and are drawn from the `carbonsms` package developed by Susan Sayre and available upon request. For simplicity in replication, all functions are included here separately.

## Downloading and preparing input data
The analysis relies on several large publicly available datasets. The utility script `R/prepare_input_data.R` is used to help construct input files for the main analysis from these files. Before running this script, users should download the following files and place them in the described locations. Users will need to create the subfolders described below. Once the files have been downloaded, run `source("R/prepare_input_data.R")`. This will create a subfolder called `prepared-data` that will be used in the actual analysis.

Additional files will be downloaded to this folder in the process of creating the figures.

Download the following files from the CEX and place them in data-raw/cex using the default file names.
https://www.bls.gov/cex/pumd/data/comma/intrvw12.zip
https://www.bls.gov/cex/pumd/data/comma/intrvw13.zip
https://www.bls.gov/cex/pumd/data/comma/intrvw14.zip
https://www.bls.gov/cex/pumd/data/comma/intrvw15.zip
https://www.bls.gov/cex/pumd/data/comma/intrvw16.zip
https://www.bls.gov/cex/pumd/data/comma/intrvw17.zip

https://www.bls.gov/cex/pumd/2012/csxintstub.txt
https://www.bls.gov/cex/pumd/2013/csxintstub.txt
https://www.bls.gov/cex/pumd/2014/csxintstub.txt
https://www.bls.gov/cex/pumd/2015/csxintstub.txt
https://www.bls.gov/cex/pumd/2016/csxintstub.txt
https://www.bls.gov/cex/pumd/2017/csxintstub.txt

Download the following files from the PUMS and place them in data-raw/pums/2016-5-year and then unzip them.
https://www2.census.gov/programs-surveys/acs/data/pums/2016/5-Year/csv_hwa.zip
https://www2.census.gov/programs-surveys/acs/data/pums/2016/5-Year/csv_pwa.zip

Download the following files from the Bonneville Power Administration and place them in the indicated subfolders
data-raw/shapefiles/utilities_public 
https://data-bpagis.hub.arcgis.com/maps/bpa-customerpublic
data-raw/shapefiles/utilities_tribal
https://data-bpagis.hub.arcgis.com/maps/bpa-customertribal
data-raw/shapefiles/utilities_investor
https://data-bpagis.hub.arcgis.com/maps/bpa-customeriou

## Other data
In addition to the files described above, the analysis relies on other input files included in the replication archive subfolder `data/`. These files are documented in [data/README.md](data/README.md).

## Running the pipeline
Once the package is installed and the main data has been downloaded/prepared, the user can run the analysis with the command `targets::tar_make()`. The first time through is time intensive and the resulting `_targets` folder is fairly large.

When running the pipeline, xgboost will issue warnings about loading serialized versions of the model. Best practice is now to save xgboost models using xgb.save but this is not easily accomplished using the `caret` package. For the purposes of this paper, the current method works but users adapting this code later may want to explore better methods of saving the files and later versions of xgboost may not be able to use the previously fitted models.

## Working with results
The `_targets/objects` folder contains a list of target names that were stored by targets. To store the list in a dataframe that can be inspected, run `obj_list <- targets::tar_meta()`. Any of the objects can be loaded by calling `targets::tar_read(objectname)`

## The tables and figures in the paper can be generated once the pipeline has been run by running the following commands:

`source("R/figs_for_paper.R")`
`source("R/votes_tract.R")`

*Note:* The map creation relies on the `tmap` package which as of July 2023 had not yet migrated away from reliance on the soon-to-be-archived `sp` package. Future users might need to construct maps differently.

## Archived results
*Note:* The code in the replication archive is set to skip the steps of fitting the spending functions and constructing the synthetic household samples. Instead, it will use the previously fitted model and synthetic datasets bundled in the replication archive. 

- To refit the models, set `reuse = F` in the call to `fit_factory` near the beginning of the `_targets.R` file.
- To reconstruct the synthetic datasets, set `use_existing = F` in the calls to `rake_factory()` in the `_targets.R` file.

Calling `targets::tar_make()` after making either of these changes so will refit all of the machine learning models and/or re-construct the synthetic household sample. It will then reconstruct any targets that depend on the changed files. Manually re-run `source("R/figs_for_paper.R")` and `source("R/votes_tract.R")` to reconstruct the figures/tables with the new results.
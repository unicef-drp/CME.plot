# CME.plot

<!-- badges: start -->

<!-- badges: end -->

CME.plot wraps plotting functions into a package.

## Installation

You can install the developing version from Github.  
If this repo is set as private, you could download the repo and compile the library locally.

All the scripts are also combined into the `Code/R` for internal users so could be sourced locally too

``` r
devtools::install_github("unicef-drp/CME.plot")
```

## Notes on the code

**The main function is `savePlotResults` ("R/savePlotResults_202001.R") and it is well-documented.**

There are some quick and easy examples saved in "data-raw/simple_examples.R".

"data-raw" also contains the script to process IHEM and WPP series.

The script "R/0.directories.R" contains functions to help with navigating directories, they are the same as in the `CME.assistant` package so `CME.plot` doesn't depend on it.

The script "R/1.helper_plot_funcs.R" contains some helper functions used in the main function `savePlotResults`. There are also some helpful functions to transform among different formats of results: `get.cqt.from.results`, `check.cqt.vs.results.csv`

The script "R/2.mark_new_series.R" contains internal functions to highlight new entries used in the main function `savePlotResults`. `get.new.series` and `mark.new.series` are the main functions (not exported). The exported functions include `review.date.of.dataentry`(quickly table the data by entry dates), `copy_code_to_dropbox` (copy code to local directories) and `copy_data_to_dropbox`

The tricky part is to determine which countries have updated VR by comparing the values of all VR series from a new data file against an old one. So which files are used? You can check this using the exported `find.dir.for.VR.comparison` function, which by default picks the latest U5MR database from `this.year()`'s folder versus `last.year()`. To overwrite, supply `dir_new_data_U5MR` and `dir_old_data_U5MR` in the global environment and these two objects will be used for comparison. 

"R/4.mydata.R" records data files included in the package.

"R/5.mics.R" can be ignored, it is used to please the CRAN package checking.

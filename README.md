# CME.plot

<!-- badges: start -->

<!-- badges: end -->

CME.plot wraps plotting functions into a package.

## Installation

Since now this repository is set as private, you could download the repo and compile the library locally. But it's set up as a package that could be installed from Github.

All the scripts are also combined into the `Code/R` for internal users so could be sourced locally too: when load the code using `LoadLibrariesAndCodes` set argument `load.CME.plot` to be `TRUE` to load scripts and data from this package)


## Notes on the code

**The main function is `savePlotResults` ("R/savePlotResults_202001.R") and it is well-documented.**

There are some quick and easy examples saved in "data-raw/simple_examples.R".

"data-raw" also contains the script to process IHEM and WPP series.

The script "R/0.directories.R" contains functions to help with navigating directories, they are the same as in the `CME.assistant` package so `CME.plot` doesn't depend on it.

The script "R/1.helper_plot_funcs.R" contains some helper functions used in the main function `savePlotResults`. There are also some helpful functions to transform among different formats of results: `get.cqt.from.results`, `check.cqt.vs.results.csv`

The script "R/2.mark_new_series.R" contains (internal) functions used by the main function `savePlotResults` to highlight newly added series in the plot. 
* `get.new.series` is the main functions (also exported). if set `return_dt_cme = TRUE` it could also be used to mark new entries in databases by returning the dataset with a new column `new_entry`  
*  Another exported function `review.date.of.dataentry` can quickly table the data by entry dates  


One tricky issue is to determine which countries have updated VR by comparing the values of all VR series from a new data file against an old one. So which files are used? You can check this using the exported `find.dir.for.VR.comparison` function, which by default picks the latest U5MR database from `current.year()`'s round versus `current.year()-1`.  
To overwrite when auto searching fails, supply `dir_new_data_U5MR` and `dir_old_data_U5MR` in the global environment and these two objects will be picked up by `find.dir.for.VR.comparison`, and will be used for comparison. 

"R/4.mydata.R" records data files included in the package.

"R/5.mics.R" can be ignored, it was used to please the CRAN package checking.

## News

2021-07: major bug fix, avoid array dimension lose so can plot for one-country run.   
2021-08: streamline the logic to supply `res.cqt`: since `output.dir` is required anyway, `res.cqt1` is more prioritized. If `res.cqt1` is supplied, will not use the default `res.cqt.Lw.rda` from `output.dir`. But `res.cqt2`, `res.cqt3`, `res.cqt4` are less prioritized than their respective output.dir arguments, which means if both `res.cqt2` and `output.dir2` are supplied, will use the default `res.cqt.Lw.rda` from `output.dir2`.   
2021-09: make some adjustments to accommodate sex-specific 5-24 charts.  
2022-07: update to WPP2022

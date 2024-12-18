# goa_restrat
Contained herein are analyses to evaluate impacts of restratification in the Gulf of Alaska (GOA) bottom trawl survey.
The overarching workflow is to:
1. query data for a stock or set of stocks
2. reclassify the historical hauls to be within the new strata that will be implemented starting with the 2025 survey
3. compute the design-based estimates of biomass and population numbers (with associated variance)
4. compare between the original time-series (often referred to as og = 'original gangster' in code) and the restratified time series

# Installation and use instructions
This is not a package yet, so, you will need to clone this repository (see these [instructions](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) on how to clone a repository).

To evaluate the comparison between original and restratified indices for your stock please see this [script](/dev/run_restrat.R), which contains the workflow described above.
Note that you will not be able to perform the second step without the necessary shapefile, but, you will be able to run these analyses so long as the 'data.rds' and 'new_haul.csv' files are contained within the 'data' folder after you've cloned this repository.
Within the analyses [script](/dev/run_restrat.R) section 'plot comparison between restratified and og indices' there are examples for how to plot figures for your specific stock.
You can either have a single species code (an example is provided for Pcod), or you can have multiple species codes (as would be the case for stock complexes, and example is provided for rougheye-blackspotted rockfish).
There are two arguments you need to provide for the plot_restrat() function, the first is the spcies code, the second is a text string that defines the name of your stock (it can be whatever you want, but don't include spaces).

What the plot_restrat() function will return are four plots:
1. The time-series comparison between the original biomass (with 95% CIs) and the point estimates for the restratified index
2. The time-series comparison between the original population numbers (with 95% CIs) and the point estimates for the restratified index
3. The percent difference between the original biomass and the point estimates for the restratified index (note that text denoting the survey's percent difference shown in bold font means that the restratified estimate for that year was outside the original 95% CIs)
4. The percent difference between the original population numbers and the point estimates for the restratified index (note that text denoting the survey's percent difference shown in bold font means that the restratified estimate for that year was outside the original 95% CIs)

This function will return these plots to you two ways.
The first, is that it will save the plots within the 'plots' folder.
The second, is that it will plot these to your device (i.e., Rstudio) and you can toggle through the different plots.

# Other important notes
The primary functions that are performing this analysis are the restrat() and get_index() functions.
The restrat() function uses the new strata shapefile to assign historical hauls within the new strata, the get_index() function then performs the design-based computations of biomass and numbers (that follows from the functions contained within the [gapindex](https://github.com/afsc-gap-products/gapindex) package).
To troubleshoot and/or dig further these would be the functions to look at.



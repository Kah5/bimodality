# biomodality
Repository for code for pls & fia tree cover bimodality analysis.

# Contents:
step_one_clean_IN.R cleans the Indiana & Illinois PLS data and creates a "cleaned" dataframe (final.data) to be used in supsequent analysis. 

morisita.R is a function that can be called calculates the tree density (stems / hectare) based on the azimuths, diameter and distances to trees.

calculate_dens_v2.R uses final.data from step_one_clean_IN.R and the morista density estimater to calculate point level densities. This script also calculates tree level Crown width from allometries in Bechtold (2003) Crown-diameter prediction models for 87 species of stand-grown trees in the Eastern United States. Southern Journal of Applied Forestry Vol. 27, No4. 
We rasterize Crown widths & crown area and estimate crown cover

extract_PT.R extracts GHCN 0.5deg x 0.5deg gridded preciptitation data from 1900-1910 to generate mean annual precipitation over the region. This script also generates pdfs with crown cover and precipitation histograms

FIA_density_processing_v2.R uses the density estimates from FIA and from calculate_dens_v2.R 

# Data
GHCN datasets found here: http://climate.geog.udel.edu/~climate/html_pages/Global2011/
PLS (Public Land Survey) and FIA datasets





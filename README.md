# Code-and-data-for-forest-fragmentation-and-yellow-fever-spillover
Repository containing final data and code for forest fragmentation and yellow fever spillover project

The code file: "Yellow_Fever_project_analysis_file_Final.R" analyzes the effect of forest fragmentation on the probability of Yellow Fever spillover events across the Amazon region of South America.

It includes a main model specification as well as 3 additional linear probability model specifications and a conditional logistic regression model as robustness checks.

It also includes code to export final tables that appear in the SI (which export linear probability model output as average marginal effects), as well as code to replicate Figure 2.

The data file: "YF_final_data.csv" is the final cleaned dataset used in the analysis, and includes the following columns:

1) Yellow_Fever_YN: binary indicator of whether a yellow fever spillover event occurred in that municipality/year
2) log.Forest_Area_ha: log-transformed forest area in hectares
3) log.area_mn_forest: log-transformed average forest patch area
4) log.forest_edge_density: log-tranformed density of forest edge per municipality area
5) log.Forest_Urban: log-tranformed forest-urban adjacency
6) NDVI_std: z-score transformed average annual normalized difference vegetation index
7) LST_Day_std: z-score transformed average annual land surface temperature
8) Precip_std: z-score transformed cumulative annual precipitation
9) Population_Density_std: z-score transformed annual population density
10) Region_Code: unique code indicating region within which each municipality sits
11) Year: time variable indicating year
12) Country: indicates whether municipality lies within Brazil, Peru or Colombia
13) Code_unique: a concatenation of code and country that uniquely identifies municipalities
14) Code: municipality code

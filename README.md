# Weather Variables Linked to Latent Factors Explaining Genotype-by-Environment Interaction Effects in Plant Disease

## Citation
This is the source code for our publication to be submitted to *Agricultural and Forest Meteorology*:  

```
@article{garnica202x,
  author = {Vinicius C. Garnica and Peter S. Ojiambo},
  title = {Weather Variables Linked to Latent Factors Explaining Genotype-by-Environment Interaction Effects in Plant Disease},
  year = {202x},
  doi = {xxx},
  journal = {xxx}
}
```

## Introduction

This repository contains data and code for analyzing weather elements linked to latent environmental factors in a factor analytic model that describes genotype-by-environment interaction effects on Stagonospora nodorum blotch (SNB) epidemics in winter wheat. The analysis uses hourly weather data collected from the period before disease onset up to the final disease assessment, along with three factor loadings of SNB foliar severity, to generate weather-based variables. An algorithm called *window pane* is applied to examine variable associations over time, and a machine learning technique called *stability selection* is used to filter out unstable predictors before performing bootstrap correlations.

The pipeline used for analysis emphasizing the required Rdata files and R scripts for each step is described below.

*Obs: R scripts 4 and 5 were run in a HPC cluster. Running them in your local machine may take some time but may be possible.*

The folder structure and key files are as follows:

```
Window_pane/
├── data/
│   ├── weather_data_2022.Rdata
│   ├── weather_data_2023.Rdata
│   ├── weather_data_2024.Rdata
│   ├── weather_data.Rdata
│   ├── outcome.Rdata
│   ├── engineered_variables.Rdata
│   ├── rolling_windows.Rdata
│   ├── windows.Rdata
│   ├── loadings.Rdata
│   ├── weather_library.Rdata
│   ├── locations.csv
│   ├── sev_final.csv
│   └── stable
├── code/
│   ├── 0_intra_day.R
│   ├── 1_first_level.R
│   ├── 2_rolling_window.R
│   ├── 3_anthesis.R
│   ├── 4_stabiliy_selection.R
│   ├── 5_bootstrap_correlations_5.R
│   ├── 5_bootstrap_correlations_10.R
│   ├── 5_bootstrap_correlations_15.R
│   ├── 5_bootstrap_correlations_20.R
│   ├── 5_bootstrap_correlations_25.R
│   ├── 5_bootstrap_correlations_30.R
│   ├── 6_second_level.R
│   └── 7_scatterplot.R
├── csh/
├── figures/
│       ├── fa1_total.png
│       ├── fa2_total.png
│       ├── fa3_total.png
│       ├── fig1.png
│       ├── fig2.tiff
│       ├── fig3.tiff
│       ├── fig4.tiff
│       ├── fig5.tiff
│       ├── predictors_batch_1.png
│       ├── predictors_batch_2.png
│       ├── predictors_batch_3.png
│       ├── predictors_batch_4.png
│       ├── predictors_batch_5.png
│       ├── predictors_batch_6.png
│       └── predictors_batch_7.png
└── results/
│       ├── result_5.Rdata
│       ├── result_10.Rdata
│       ├── result_15.Rdata
│       ├── result_20.Rdata
│       ├── result_25.Rdata
│       ├── result_30.Rdata
│       ├── loading.csv
│       ├── fa1_variables.docx
│       ├── fa2_variables.docx
│       └── fa3_variables.docx
```

## Analysis pipeline

The `0_intra_day.R` script is the first script and combines the `weather_data_2022.Rdata`, `weather_data_2023.Rdata`, and `weather_data_2024.Rdata` files. It also creates intra-day intervals (24h, daytime, nighttime, dusk, etc.) based on geographical coordinates and local sunset and sunrise times, extracted from the `suncal` R package. This script exports an Rdata file named `weather_data.Rdata` for use in the next step.

The `1_first_level.R` script summarizes hourly data into daily weather features according to specific criteria set by the user, typically based on expert knowledge of the system under investigation. For example, variables capturing conditions like moderate temperatures and rainfall, which were previously reported to be conducive to SNB epidemics, are engineered by the user in this step. The result is exported as `engineered_variables.Rdata`.

The `2_rolling_window.R` script performs the rolling sum or averages for the previously engineered features, over the time series (here, the time of the growing season between sometime in Janurary and June). It exports the `rolling_window.RData` back into the data folder. The selection of window sizes constitutes a critical step in this study and should be based on expert understanding of the biological system under examination. For instance, for *Parastagonospora nodorum*, the latent period ranges from 6 to 49 days; yet, under field conditions, it typically spans between 8 to 14 days. Consequently, we assumed that window lengths greater than 30 days are likely not going to provide insights and may even diminish significant epidemiological effects occurring within shorter intervals. This dilution may result from the inclusion of days devoid of informative weather effects. So we created six windows, as you can see in: wind_size = seq(5, 30, 5).

The `3_anthesis.R` code downloads daily weather data used to predict the wheat anthesis date for each environment. It also imports the loadings from another study exploiting genotype-by-environment interactions and left join them into the lists of variables. Responses are called fa1, fa2, and fa3 in the scripts. Results are stored in `windows.RData`. This section also defines the LAGs, which represent the number of days from the anthesis date. Positive LAGs indicate days before anthesis, while negative LAGs represent days after. In this analysis, anthesis serves as the reference point, although for traits like yield, the estimated date of physiological crop maturity might be a more suitable reference. The choice of reference depends on the intended use of the weather variables. Here, our goal is to create a dense matrix of variables to predict disease risk before anthesis, which is typically the final window for fungicide application.

The `4_stability_selection.R` script applies stability selection using the `sharp` R package. This step is performed on an HPC cluster using custom shell scripts for job submission and execution, available in the `csh` folder. Alternatively, you may execute `4_stability_selection.R` on your local machine, although this process may take some time. Adjusting the number of bootstrap samples can assist in reducing analysis time but may influence results. Results from this step will be stored in `window_pane/data/stable/`. Six different Rdata files with stable variables for each window will be returned.

`5_bootstrap_correlations_5.R` through`5_bootstrap_correlations_30.R` also run within the HPC cluster environment, conducting bootstrap Spearman's correlations for first-level variables identified in the stability analysis. As mentioned, this code should be run for each Rdata file of window size (5 through 30 in this study) **independently** by manually changing input files in the code stored in `window_pane/data/stable/`. Outputs from this step are stored in the folder `results`. 

Finally, results can be visualized on your local machine by loading the files from `results` using the `6_second_level.R` script. We also created functions to calculate the second-level predictors and export them as Word files. `7_scatterplot.R` plots all variables against the average value of SBN severity for each environment as well as six selected second-level weather variables. You should be familiar that weather variables derived from different environmental loadings will have differential effects on the response.

This code is released under a permissive open-source license, allowing you to use or adapt it as long as you adhere to the terms of the license. If you plan to utilize the code, please consider citing the paper.


## References

* Dalla Lana, F., Madden, L.V. and Paul, P.A., 2021. Natural occurrence of maize Gibberella ear rot and contamination of grain with mycotoxins in association with weather variables. Plant Disease, 105(1), pp.114-126.

* Kriss, A.B., Paul, P.A. and Madden, L.V., 2010. Relationship between yearly fluctuations in Fusarium head blight intensity and environmental variables: a window-pane analysis. Phytopathology, 100(8), pp.784-797.

* Mehra, L. K., Adhikari, U., Ojiambo, P. S., Cowger, C. 2019. Septoria nodorum blotch of wheat. The Plant Health Instructor. 



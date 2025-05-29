#  Leveraging window-pane analysis with environmental factor loadings of genotype-by-environment interaction to identify high-resolution weather-based variables associated with plant disease

## Citation

```
@article{garnica202x,
  author = {Vinicius C. Garnica and Peter S. Ojiambo},
  title = {Leveraging window-pane analysis with environmental factor loadings of genotype-by-environment interaction to identify high-resolution weather-based variables associated with plant disease},
  year = {202x},
  doi = {xxx},
  journal = {xxx}
}
```

## Introduction

This repository provides data and code for analyzing weather variables linked to latent environmental factors derived from a factor analytic (FA) model characterizing genotype-by-environment (G×E) interaction effects on Stagonospora nodorum blotch (SNB) epidemics in winter wheat. The workflow integrates hourly weather data (collected from pre-disease onset to final disease assessment) with three latent factor loadings of SNB foliar severity to generate weather-based predictors. An algorithm termed window pane evaluates temporal associations between variables, while stability selection (a machine learning technique) filters unstable predictors prior to bootstrap correlation analysis.

The pipeline used for analysis emphasizing the required Rdata files and R scripts for each step is described below.

*Note: Scripts 4 and 5 were executed on an HPC cluster. Running them locally is feasible but computationally intensive.*

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
│   ├── severity_final.csv  
│   └── stable/  
├── code/  
│   ├── 0_intra_day.R  
│   ├── 1_first_level.R  
│   ├── 2_rolling_window.R  
│   ├── 3_anthesis.R  
│   ├── 4_stability_selection.R  
│   ├── 5_bootstrap_correlations_5.R  # Window size: 5 days  
│   ├── 5_bootstrap_correlations_10.R # Window size: 10 days  
│   ├── 5_bootstrap_correlations_15.R # Window size: 15 days  
│   ├── 5_bootstrap_correlations_20.R # Window size: 20 days  
│   ├── 5_bootstrap_correlations_25.R # Window size: 25 days  
│   ├── 5_bootstrap_correlations_30.R # Window size: 30 days  
│   ├── 6_second_level.R  
│   └── 7_scatterplot.R  
├── csh/  
├── figures/  
│   ├── fa1_total.png  
│   ├── fa2_total.png  
│   ├── fa3_total.png  
│   ├── fig1.png  
│   ├── fig2.tiff  
│   ├── fig3.tiff  
│   ├── fig4.tiff  
│   ├── fig5.tiff  
│   ├── predictors_batch_1.png  
│   └── ...  
└── results/  
│   ├── result_5.Rdata  
│   ├── result_10.Rdata  
│   ├── ...  
│   ├── loading.csv  
│   ├── fa1_variables.docx  
│       ├── fa2_variables.docx
│       └── fa3_variables.docx
│   └── ...  
```

## Analysis pipeline

The analysis pipeline initiates with  `0_intra_day.R`, a script is the first script and combines the `weather_data_2022.Rdata`, `weather_data_2023.Rdata`, and `weather_data_2024.Rdata` files.  It consolidates multi-year hourly weather data (2022–2024) and calculates intra-day intervals (24-hour, daytime, nighttime, dusk) using geographic coordinates and solar metrics from the`suncal` R package. This script exports an Rdata file named `weather_data.Rdata` for use in the next step.

The `1_first_level.R` script summarizes hourly data into daily weather features according to specific criteria set by the user, typically based on expert knowledge of the system under investigation. For example, variables capturing conditions like moderate temperatures and rainfall, which were previously reported to be conducive to SNB epidemics, are engineered by the user in this step. The result is exported as `engineered_variables.Rdata`.

The `2_rolling_window.R` applies rolling sums or averages to these variables across the growing season (January–June), testing six window sizes (5–30 days, increments of 5) to balance biological relevance (latent period: 8–14 days) and signal preservation, with results stored in `fixed_window.RData`. The selection of window sizes is an important step in this framework and should be based on understanding of the biological system under investigation. For instance, for *Parastagonospora nodorum*, the latent period ranges from 6 to 49 days; yet, under field conditions, it typically spans between 8 to 14 days. Consequently, we assumed that window lengths greater than 30 days are likely not going to provide insights and may even diminish significant epidemiological effects occurring within shorter intervals.

The `3_anthesis.R` script downloads daily weather data to estimate the anthesis (flowering) date for each wheat trial environment. It also imports factor loadings from a separate GxE study and merges them with other variables. In the script, the response variables are labeled as fa1, fa2, and fa3 in the script. Results are stored in `rolling_windows.RData`. This script also defines *LAG values*, which represent the number of days relative to the anthesis date. Positive LAGs count days before anthesis, while negative LAGs count days after. In this analysis, anthesis is used as the central reference point. However, for traits like yield, it might be more appropriate to use the estimated date of physiological maturity instead. The choice of reference depends on the goal of the analysis.

The `4_stability_selection.R` script applies stability selection using the `sharp` R package. This step is performed on an HPC cluster using custom shell scripts for job submission and execution, available in the `csh` folder. Alternatively, you may execute `4_stability_selection.R` locally, although this process may take some time. Adjusting the number of bootstrap samples can assist in reducing analysis time. Results from this step will be stored in `window_pane/data/stable/` folder. Six different Rdata files, one for each window size (e.g., stable_5.RData, stable_10.RData, ...) will be returned.

`5_bootstrap_correlations_5.R` through`5_bootstrap_correlations_30.R` are designed to run on an HPC cluster. These scripts perform bootstrap Spearman correlations for first-level weather variables identified in the stability analysis. Each script corresponds to a specific window size (from 5 to 30 days) and must be run independently. To do this, you need to manually change the input files in `window_pane/data/stable/`. The results from each run are saved in the `window_pane/results` folder. 

Once the analysis is complete, results can be visualized on your local computer using the `6_second_level.R`. This script also includes functions to compute second-level predictors and export summary tables as Word documents.

Finally, `7_scatterplot.R` script creates scatterplots showing how each variable relates to the average SBN severity across environments. It also highlights six selected second-level weather predictors. Keep in mind that weather variables derived from different G×E loadings may influence the response differently.

This code is open-source and available under a permissive license. You’re free to use or modify it, provided you comply with the license terms. If you use the code in your work, please cite the corresponding paper.


## Key References

* Dalla Lana, F., Madden, L.V. and Paul, P.A., 2021. Natural occurrence of maize Gibberella ear rot and contamination of grain with mycotoxins in association with weather variables. Plant Disease, 105(1), pp.114-126.

* Bodinier, B., Filippi, S., Nøst, T.H., Chiquet, J. and Chadeau-Hyam, M., 2023. Automated calibration for stability selection in penalised regression and graphical models. Journal of the Royal Statistical Society Series C: Applied Statistics, 72(5), pp.1375-1393.

# Installation
1. Clone the repository
## If you run with local environment:
1. Install R ver. 4.3.3 and Rstudio ver. 2023.12.1, and open this repository in Rstudio (You can find R from [CRAN](https://cran.r-project.org/), and Rstudio from [Rstudio](https://www.rstudio.com/products/rstudio/download/)).

## If you run with Docker:
2. Install Docker and docker-compose (You can find Docker from [Docker](https://www.docker.com/products/docker-desktop) and docker-compose from [docker-compose](https://docs.docker.com/compose/install/)).
3. Build the Docker image
```bash
./compose_docker.sh
```
4. Run the Docker container
```bash
docker-compose up
```

# Run the analysis
1. Enter the R environment. You can Rstudio via the link (http://localhost:8585). Default User name and password are `rstudio` and `password`.

2. Install the required packages (This may take a while)
```r
install.packages(c("renv"))
renv::restore()
```
2. Run the behaviour analysis script
```r
source("behaviour/analysis_scripts/execute_behaviour_analysis.R")
```
3. Run the model based anaysis script 

> [!NOTE] 
> This script will take a long time with CPU resource to run simulations.
> To avoid this, comment out the `source(here::here("model_based_analysis", "analysis", "MLE.R"))` and `source(here::here("model_based_analysis", "model", "parameter_recovery.R"))`. However, you need pre-computed data in `model_based_analysis/R_result`.

> [!NOTE] 
> Due to a technical issue, the random seed is not set in the `model_based_analysis/analysis/MLE.R` script and the `model_based_analysis/model/parameter_recovery.R` script. This may cause the results to be slightly different from the original paper.

```r
source("model_based_analysis/execute_model_analysis.R")
```

# data
- `df_rule_hit_switch.csv`: The data of the rule prediction main task used in the paper.
- `df_practice.csv`: The data of the practice task for the rule prediction main task used in the paper.
- `df_score_hit.csv`: The data of the score prediction task used in the paper.
- `players_rewards.csv`: The data of the participants' demographics and rewards.

## Figures and corresponding scripts

The table below lists each figure in the manuscript, the script that generates it, and the location of the output produced by `save_svg_figure()` (both `.svg` and `.png` files are written).

| Figure | Script (line) | Output location |
| --- | --- | --- |
| Figure 1C | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L416` | `results/description/general accuracy plot.*`
| Figure 1D | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L814,L831` | `results/p_example.*`, `results/p_example2.*`
| Figure 1E | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L752` | `results/ratio of choice for score.*`
| Figure 1F | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1143` | `results/per_score/p_per_score_T.*`
| Figure 2A | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1274` | `results/accuracy_score_truerule/p_accuracy_score_truerule.*`
| Figure 2C | `model_based_analysis/analysis/param_analysis.R:L555` | `results/p_param_all.*`
| Figure 2D | `model_based_analysis/analysis/param_analysis.R:L579` | `results/p_param_corr.*`
| Figure 2E | `model_based_analysis/analysis/anova_plots.R:L73` | `results/p_distance_model_accuracy_score_truerule.*`
| Figure 3A | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L993` | `results/time_series/p_switch_acc.*`
| Figure 3B | `model_based_analysis/analysis/temporal_analysis.R:L134` | `results/p_distance_model_switch_acc.*`
| Figure 3C | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1366` | `results/switch_prev_choice_score/p_switch_prev_choice_score.*`
| Figure 3D | `model_based_analysis/analysis/anova_plots.R:L177` | `results/p_distance_model_prev_choice_switch.*`
| Figure 4A | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1241` | `results/conf_correct/p_conf_correct.*`
| Figure 4B | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1212` | `results/conf_score/p_conf_score.*`
| Figure 4C | `model_based_analysis/analysis/confidence_distance_relation.R:L39` | `results/confidence_distance_relation/subjective_distance_vs_zscored_confidence.*`
| Figure 5A | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1318` | `results/conf_score_truerule/p_conf_score_truerule.*`
| Figure 5B | `model_based_analysis/analysis/anova_plots.R:L95` | `results/p_distance_model_entropy_score_truerule.*`
| Figure 5C | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1024` | `results/time_series/p_switch_conf.*`
| Figure 5D | `model_based_analysis/analysis/temporal_analysis.R:L178` | `results/p_distance_model_switch_conf.*`
| Figure 5E | `behaviour/analysis_scripts/execute_behaviour_analysis.R:L1387` | `results/switch_conf_score/p_conf_switch.*`
| Figure 5F | `model_based_analysis/analysis/anova_plots.R:L198` | `results/p_distance_model_conf_switch.*`

`*`Each path is saved with both `.svg` and `.png` extensions.

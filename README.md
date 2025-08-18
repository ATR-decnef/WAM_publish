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

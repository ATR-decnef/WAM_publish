# Installation
1. Clone the repository
## If you run with local environment:
2. Install R and Rstudio, and open this repository in Rstudio

## If you run with Docker:
2. Install Docker and docker-compose
3. Build the Docker image
```bash
./compose_docekr.sh
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

[!NOTE]
This script will take a long time with CPU resource to run simulations.
To avoid this, comment out the `source(here::here("model_based_analysis", "analysis", "MLE.R"))` and `source(here::here("model_based_analysis", "model", "parameter_recovery.R"))`. However, you need pre-computed data in `model_based_analysis/R_result`.

```r
source("model_based_analysis/execute_model_analysis.R")
```
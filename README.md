# Installation
1. Clone the repository
If you run with local environment:
2. Install R

If you run with Docker:
2. Install Docker and docker-compose
3. Build the Docker image
```bash
./compose_docekr.sh
```
4. Run the Docker container
```bash
docker-compose up
```
5. Enter the Docker container
```bash
docker exec -it <container_name> /bin/bash
```

# Run the analysis
1. Enter the R environment
2. Install the required packages
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
This script will take a long time with CPU resource to run simulations

```r
source("model_based_analysis/execute_model_analysis.R")
```
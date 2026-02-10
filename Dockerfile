FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libcairo2-dev libxt-dev \
    libharfbuzz-dev libfribidi-dev && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','DT','ggplot2','plotly','patchwork','promises','future','jsonlite','readxl','writexl','rootSolve'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

EXPOSE 3838
CMD ["Rscript", "-e", "setwd('/app/ITCSuiteWeb'); shiny::runApp('.', host='0.0.0.0', port=3838)"]

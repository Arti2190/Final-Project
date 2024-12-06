FROM rstudio/plumber

WORKDIR /
RUN mkdir /www

# Install system dependencies
RUN apt-get update -qq && apt-get install -y \
    libssl-dev libcurl4-gnutls-dev libpng-dev pandoc
    
# Install R packages separately
RUN R -e "install.packages('GGally')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('tidymodels')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('yardstick')"
RUN R -e "install.packages('ggfortify')"
RUN R -e "install.packages('ranger')"

COPY diabetes_binary_health_indicators_BRFSS2015.csv /
COPY project_api.R project_api.R
EXPOSE 8000

ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('project_api.R'); pr$run(host = '0.0.0.0', port = 8000, swagger = TRUE)"]


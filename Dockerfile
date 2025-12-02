FROM rocker/r-ver:4.4.0

WORKDIR /service

RUN apt clean && apt-get update && apt-get -y install alien

# R program dependencies
RUN apt-get install -y libudunits2-dev && apt-get install -y libgeos-dev && apt-get install -y libproj-dev && apt-get -y install libnlopt-dev && apt-get -y install pkg-config && apt-get -y install gdal-bin && apt-get install -y libgdal-dev
RUN apt-get -y install libcurl4-openssl-dev libfontconfig1-dev libxml2-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

RUN R --version

COPY dependencies ./dependencies

## Add additional program specific dependencies below ...
# install autoHLA package
RUN Rscript -e "install.packages(c('RColorBrewer', 'uwot', 'readxl', 'ggbeeswarm'), Ncpus = 10)"
RUN Rscript -e "install.packages(c('BH'), Ncpus = 10, repos = 'https://cloud.r-project.org/', dependencies = TRUE)"
RUN Rscript -e "install.packages(c('tidyverse'), Ncpus = 10, dependencies=TRUE)"
RUN Rscript -e "library(tidyverse)"

# entrypoint
COPY . ./

RUN ls /service

RUN mkdir -p data

ENTRYPOINT [ "Rscript", "/service/standalone_main.R" ]
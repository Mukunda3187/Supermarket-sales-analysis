
FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("shiny","bslib","ggplot2","dplyr","DT","rpart","rpart.plot","readr","scales"), repos="https://cloud.r-project.org")'

WORKDIR /app
COPY . /app

EXPOSE 10000

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '10000')))"]

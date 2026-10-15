
FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libuv1 \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("bslib","ggplot2","dplyr","DT","rpart","rpart.plot","readr","scales","fs"), repos="https://cloud.r-project.org")'

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server/

EXPOSE 10000

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '10000')))"]

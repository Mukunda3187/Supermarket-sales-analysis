
FROM rocker/shiny:latest

RUN R -e 'install.packages(c("bslib","ggplot2","dplyr","DT","rpart","rpart.plot","readr","scales"), repos="https://cloud.r-project.org")'

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server/

EXPOSE 10000

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '10000')))"]

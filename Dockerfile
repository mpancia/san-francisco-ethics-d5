FROM rocker/tidyverse

ENV USER rstudio
WORKDIR /home/$USER/project

RUN R -e 'install.packages("remotes", repos = c(CRAN = "https://cran.rstudio.com"))'
RUN R -e 'remotes::install_github("rstudio/renv")'

COPY renv.lock ./

RUN R -e 'renv::restore(confirm=FALSE)'


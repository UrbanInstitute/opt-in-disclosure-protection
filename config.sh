#!/bin/bash

# install script for cloud syntheses - run from the RStudio Docker Container Terminal Pane

# install R packages
sudo su - -c "R -e \"install.packages('rmutil', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('reticulate', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('furrr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('here', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tictoc', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('ipumsr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('caret', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('moments', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('randomForest', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidymodels', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('ranger', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('rpart.LAD', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('vip', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('patchwork', repos='http://cran.rstudio.com/')\""

# download tidysynthesis from github - will be prompted for credentials
cd $HOME
git clone git@github.com:UI-Research/tidysynthesis.git
R CMD build tidysynthesis
R CMD INSTALL tidysynthesis_0.0.0.9001.tar.gz

# download syntheval from github 
cd $HOME
git clone git@github.com:UI-Research/syntheval.git
R CMD build syntheval
R CMD INSTALL syntheval_0.0.0.9000.tar.gz


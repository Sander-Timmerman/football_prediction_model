library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)
library(xlsx)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)
flog.threshold(INFO)

source.all("R")

output <- run_prediction_application()
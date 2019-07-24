#contains functions to process the data

library(cranlogs)
library(ctv)
library(dplyr)
library(ggplot2)
library(scales)
library(cranly)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(magrittr)
library(xml2)
library(lubridate)

####get global data + tutti####
#generate global_data
global_edgelist <- data.frame()
global_api <- list()
global_download <- data.frame ()

#gather global data of all ctvs
for (view in ctv::available.views()) {
  packages <- view$packagelist$name
  download_statistics <- get_download_statistics(packages)
  api_data <- get_api_data(packages)
  global_api <- rbind(global_api, api_data)
  edgelist <- get_edgelist_of_packages(api_data)
  global_edgelist <- rbind(global_edgelist, edgelist)
  global_download <- rbind(global_download, download_statistics)
  #print(paste(view$name, "is done.")) #control
}

packages_per_ctv <- get_packages_per_ctv()

#gather dependencies with ctv classification (to be able to use the ctv category as well)
tutti_dependencies <-
  inner_join(global_edgelist, packages_per_ctv, by = c ("this" = "package"))

#match pacakge- downloads with ctv (-> ctv downloads possible)
tutti_timeseries <-
  inner_join(global_download, packages_per_ctv, by = c("package" = "package"))%>%
  filter(core == FALSE)









library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(srvyr) # survey analysis functions
library(DataExplorer) # EDA tools

# some custom functions
source("~/Data/r/basic functions.R")

anes_2024 <- readRDS("data/anes_2024")

## ---------------------------------------
## Load intraday Rdata futures files

## Loads R Data files and exports them to 
## CSV file so Amibroker can pick them up
##
## Created 9/4/2016
## ---------------------------------------

## Initialization
suppressWarnings(try(rm(list=ls()),silent=TRUE))   #clears the workspace
switch(Sys.info()[['sysname']],
       Windows = {setwd("c:/Users/jharper/Dropbox/03_School/Programming/R/FileDirectory/")},
       Linux = {stop("hold on, this is a Linux Box!")},
       Darwin ={setwd("/Users/harper/Dropbox/HarpFolders/04_Code/R/FileDirectory/")}
)
require(quantstrat)
require(Quandl)
options(digits.secs=6)  # shouldn't need to change this often

## Variables
dir.input   <- "/Users/harper/Dropbox/HarpFolders/02_Trading/Data/RawFiles/2_ConvertedToRdata/"
dir.output  <- "/Users/harper/Dropbox/HarpFolders/02_Trading/Data/RawFiles/1_RawCSVFiles/"
file.type   <- 0 # 0 for minute, 1 for daily

## Load File
file.type <- ifelse(file.type == 0, "minute", "daily")
my.files  <- list.files(dir.input, pattern = file.type)[1:3]
# my.files  <- list.files(dir.input, pattern = file.type)
# TO DO:  uncomment when testing is done 

for (i in seq(my.files)) {
  load(paste0(dir.input,my.files[i]))
}


## TESTING ONLY
## Subset the files for 2015
my.files <- ls(pattern = file.type)
for (i in seq(my.files)) {
    assign(my.files[i], value = get(my.files[i])["2015"])
}


## Save as CSV
for (i in seq(my.files)) {
  print(my.files[i])
  new.name <- paste(paste(strsplit(my.files[i], split = "[.]")[[1]][1:2], 
                          collapse = "."), "IQFeed", sep = ".")
  write.zoo(get(my.files[i]), paste0(dir.output, new.name, ".txt"))
}



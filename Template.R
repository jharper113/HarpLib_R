###########################################################################

## R Code Template

## Date


## Description of what was this script does

###########################################################################

# Initialize Workspace
suppressWarnings(try(rm(list=ls()),silent=TRUE))   #clears the workspace
switch(Sys.info()[['sysname']],
       Windows = {setwd("c:/Users/jharper/Dropbox/03_School/Programming/R/FileDirectory/")},
       Linux = {stop("hold on, this is a Linux Box!")},
       Darwin ={setwd("/Users/harper/Dropbox/03_School/Programming/R/FileDirectory/")}
)
require(quantstrat)
source(file = "./Scripts/HarpStrats/HarpFunctions.R")


###########################################################################
# Variables ---------------------------------------------------------------
###########################################################################
var1 <- 1
var2 <- 2
var3 <- 3


###########################################################################
# Functions ---------------------------------------------------------------
###########################################################################

#  ------------------------------------------------------------------------
SampleFunction <- function(arg1, arg2, arg3) {
  # Summary of Function
  #
  # Args      
  #   arg1         description and example, like "number, 1, 2, 3.1, etc"
  #   arg2
  #   arg3 
  #
  # Returns 
  #   creates a bunch of new folders and sets some variables 
  # -----------------------------------------------------------------------
}


###########################################################################
# Main --------------------------------------------------------------------
###########################################################################

## Print output to the console
cat("\n\n-------------------------\n")
cat("Print output to the console\n\n")
cat(date())
cat("\n")

## List files in the directory
files.in.dir <- list.files(path = input.dir, pattern = "*")


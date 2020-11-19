#!/usr/bin/env Rscript
###########################################################################

#   R Code Template

#   Description:
#     Template to quickly import and
#     get an overview of a dataset
#
#   Inputs
#   Outputs

#   Date

###########################################################################
# Packages ----------------------------------------------------------------
###########################################################################

# suppressWarnings(try(rm(list=ls()),silent=TRUE)) # clear the workspace
# switch(Sys.info()[['sysname']],
#       Windows = {setwd("c:/Users/jharper/Dropbox/03_School/Programming/R/FileDirectory/")},
#       Linux = {stop("hold on, this is a Linux Box!")},
#       Darwin ={setwd("/Users/harper/Dropbox/03_School/Programming/R/FileDirectory/")}
# )
library(tidyverse)
# library(readxl)
library(docopt)

# source(file = "./HarpFunctions.R")

###########################################################################
# Variables ---------------------------------------------------------------
###########################################################################
# set up arguments
'Usage:
  script.R [-a <arg1> -b <arg2> -c <arg2> -o <output>]

Options:
  -a Argument 1
  -b Argument 2
  -c Argument 3
  -o Output file
' 
#-> doc  -- 2020-10-28 not sure what this is
opts <- docopt(doc)

file_input  <- "input_file.txt"
file_output <- opts$o #"output_file.txt"
file_log    <- "logfile.txt"

var1 <- opts$a
var2 <- opts$b
var3 <- opts$c

###########################################################################
# Functions ---------------------------------------------------------------
###########################################################################
SampleFunction <- function(arg1, arg2, arg3, verbose = FALSE) {
  # Summary of Function
  #
  # Args:
  #   arg1: Description, like "One of two vectors whose sample covariance is to be calculated."
  #   arg2: Description, like "The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   arg3: Description and example.
  #   verbose: If TRUE, sends output to console or log file. default is False.
  #
  # Returns:
  #   a data frame
  # Variables
  # Error handling
  if (verbose)
    cat("Summary", round(covariance, 4), ".\n", sep = "", file = "file.log")
}

###########################################################################
# Main --------------------------------------------------------------------
###########################################################################

# Import Data -------------------------------------------------------------

# List files in the directory
files.in.dir <- list.files(path = input.dir, pattern = "*")

df <- read_csv(file = file.input,
         n_max =  10 * 1000,
         col_types = cols("cin")
         )
# df <- read_ods(file_input)
# df <- readRDS(file.input)
# df <- read_excel(file.input)
# df <- read_file()  # reads into a string
# names(df) <- make.names(names(df))  # fix spaces, characters in df names


# Explore Data ------------------------------------------------------------
dim(df)
str(df)
head(df)
tail(df)
sample_n(df, 10)
summary(df)

# Tidy data
gather(data = df, key = col1, value = col2)
spread(data = df, key = col1, value = col2)


# Transform Data ----------------------------------------------------------

df %>%
  select(col1, col2) %>%
  filter(col1 > 3, col2 != 10) %>%
  mutate() %>% # add variables
  arrange() %>%   # reorder rows
  rename() %>% # rename variables
  summarise() %>% # generate summary statistics
  group_by() %>%


# Visualize Data ----------------------------------------------------------
ggplot() +
  geom_point(mapping = aes(x = col1, y = col2))


# Print output to the console ---------------------------------------------
cat("\n\n-------------------------\n", file = file.log, append = TRUE)
cat("Print output to the console\n\n", file = file.log, append = TRUE)
cat(date(), file = file.log, append = TRUE)
cat("\n", file = file.log, append = TRUE)
readline("Press <Enter> to continue")


# Save Output -------------------------------------------------------------
# Save output
# ggsave()
# saveRDS()



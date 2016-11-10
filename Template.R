###########################################################################

#   R Code Template

#   Description of what was this script does
#   Inputs
#   Outputs

#   Date

###########################################################################

# Initialization ----------------------------------------------------------
# suppressWarnings(try(rm(list=ls()),silent=TRUE)) # clear the workspace 
# switch(Sys.info()[['sysname']],
#       Windows = {setwd("c:/Users/jharper/Dropbox/03_School/Programming/R/FileDirectory/")},
#       Linux = {stop("hold on, this is a Linux Box!")},
#       Darwin ={setwd("/Users/harper/Dropbox/03_School/Programming/R/FileDirectory/")}
# )
library(tidyverse)
# source(file = "./HarpFunctions.R")


# Variables ---------------------------------------------------------------
file.log    <- "logfile.txt"
file.input  <- "input.file.txt"
file.output <- "output.file.txt"
var1 <- 1
var2 <- 2
var3 <- 3


# Functions ---------------------------------------------------------------
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


# Main --------------------------------------------------------------------

# Import data 
x <- read_csv(file = file.input, 
         n_max =  10 * 1000,
         col_types = cols(col1 = col_double(), col2 = col_datetime())
         )

# Tidy data
gather(data = x, key = col1, value = col2)
spread(data = x, key = col1, value = col2)

# Transform 
x %>% 
  filter(col1 > 3, col2 != 10) %>% 
  select() %>% 
  mutate() %>% 
  arrange() %>% 
  summarise() %>% 
  group_by() %>%
  
# Visualize  
  ggplot2() + 
    geom_point(mapping = aes(x = col1, y = col2))

# Print output to the console
cat("\n\n-------------------------\n")
cat("Print output to the console\n\n")
cat(date())
cat("\n")

# Save output
ggsave()

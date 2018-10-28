# Title: Testing Functions and Strings
# Description: Testing different functions created using using if-conditionals, loop structures, and manipulation of strings.
# Input(s): NA
# Output(s): test file 'test-output.txt'
# Author(s): Roni Shen
# Date: 10-28-2018

library('testthat')

functions <- dir('../code/functions')
lapply(paste0('../code/functions/', functions), source)

sink(file = 'test-output.txt')
test_dir('../code/tests') 
sink()
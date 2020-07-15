#!/usr/bin/env Rscript
library(megautils)
library(dplyr)

main <- function(args) {
  name <- args[1]
  table <- tbl(db_conn('playax-columnstore'), name) %>% collect

  assign(name, value = table)
  eval(substitute(save(name, file = args[2])))
}

parsed <- optparse::parse_args(
  optparse::OptionParser(),
  args = commandArgs(trailingOnly = TRUE),
  positional_arguments = 2
)

main(parsed$args)

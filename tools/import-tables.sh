#!/usr/bin/env bash

TABLES='source_name_mapping cities states regions countries'

for table in $TABLES; do
  echo "Importing $table."
  Rscript ./import-table.R $table ../data/$table.RData
done

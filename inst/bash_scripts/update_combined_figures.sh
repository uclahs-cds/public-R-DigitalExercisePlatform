#!/bin/bash

TODAY="$(date +%Y-%m-%d)"

if [ -z $1 ]
then
  OUTPUT_DIR=.
else
  OUTPUT_DIR=$1
fi

# Replaces the figure files with todays date
sed -i'.bak' -e "s/\d\d\d\d-\d\d-\d\d/$TODAY/g" "$1/latest_figures/combined_figures2_3.svg"
sed -i'.bak' -e "s/\d\d\d\d-\d\d-\d\d/$TODAY/g" "$1/latest_figures/combined_figures4.svg"

# Extract the linked filenames from the combined figure files
egrep -ho "\"(/.*?png)\"" "$1/latest_figures/combined_figures2_3.svg" "$1/latest_figures/combined_figures4.svg" | xargs -I {} cp {} "${OUTPUT_DIR}/latest_figures/"

# TODO: zip up the latest files

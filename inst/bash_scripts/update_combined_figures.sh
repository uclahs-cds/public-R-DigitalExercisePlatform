#!/bin/bash

TODAY="$(date +%Y-%m-%d)"

if [ -z $1 ]
then
  OUTPUT_DIR=.
else
  OUTPUT_DIR=$1
fi

# Replaces the figure files with todays date
sed -i'.bak' -E "s/[0-9]{4}-[0-9]{2}-[0-9]{2}/$TODAY/g" "$1/latest_figures/combined_figures2_3.svg"
sed -i'.bak' -E "s/[0-9]{4}-[0-9]{2}-[0-9]{2}/$TODAY/g" "$1/latest_figures/combined_figures4.svg"

# Extract the linked filenames from the combined figure files
egrep -ho "\"(/.*?png)\"" "$1/latest_figures/combined_figures2_3.svg" "$1/latest_figures/combined_figures4.svg" | xargs -I {} cp {} "${OUTPUT_DIR}/latest_figures/"

# Convert to pdf
/Applications/Inkscape.app/Contents/MacOS/inkscape $1/latest_figures/combined_figures2_3.svg -d 1200 $1/latest_figures/combined_figures2_3.pdf
/Applications/Inkscape.app/Contents/MacOS/inkscape $1/latest_figures/combined_figures4.svg -d 1200 $1/latest_figures/combined_figures4.pdf

# Merge pdfs into single pdf
convert -density 600 $1/latest_figures/combined_figures2_3.pdf $1/latest_figures/combined_figures4.pdf $1/latest_figures/${TODAY}_DEXP_combined_figures_3_4_5.pdf

# Zip up the latest files
zip -r $1/latest_figures/${TODAY}_DEXP-figures.zip $1/latest_figures/${TODAY}_digIT*.png

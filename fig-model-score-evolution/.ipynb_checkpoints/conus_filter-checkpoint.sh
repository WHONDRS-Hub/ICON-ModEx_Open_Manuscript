#!/bin/bash
#================================
# Filter data to CONUS region
#================================

# Change into the requested directory
cd $1

# Make symbolic link to CONUS outline
# Can't use a link b/c won't propagate to container!
#ln -sv ../../us.xy ./us.xy
# Need to copy file
cp ../../us.xy ./us.xy

# Set input and output
input=output_all_sites_avgpre_stdpre_merged.csv
output=output_all_sites_avgpre_stdpre_merged_filtered.csv

# Preprocess by copying leftmost column (Sample_ID) to far right
# and then set the leftmost column to nothing and print each line
# so lon and lat are the first columns. Skip over the first (header)
# line in the input file.
awk -F, 'NR>1{print $0","$1}' $input | awk -F, '{$1=" "}1' > tmp1.xyz

# Remove all observed sites
grep -v CM_ tmp1.xyz | grep -v SSS | grep -v S19S_ > tmp2.xyz

# Get header for later and get rid of commas
awk -F, 'NR == 1 {print $0","$1}' $input | awk -F, '{$1=" "}1' | sed 's\  \\g' > tmp3.xyz

# Check Docker daemon is running
if [ `sudo systemctl is-active docker` == "active" ]
then
    #echo Docker daemon is already started. Do nothing.
    sleep 1
else
    #echo Docker daemon not started. Starting Docker daemon...
    sudo systemctl start docker
fi

# Select points only inside CONUS polygon
# Docs say -a9=name:STRING should do the trick, but doesn't work for me.
# (https://docs.generic-mapping-tools.org/6.0/gmt.html#aspatial-full)
gmt_cmd="gmt gmtselect tmp2.xyz -Fus.xy >> tmp3.xyz"
sudo docker run --rm -v $(pwd):/work -w /work parallelworks/gmt $gmt_cmd
sudo chmod u+rw tmp3.xyz

# Convert back to .csv and sort
cat tmp3.xyz | tr " " "," | tr "\t" "," | sort -t "," -g -k 9,10 > $output

# Clean up
rm -f tmp1.xyz
rm -f tmp2.xyz
rm -f tmp3.xyz
rm -f us.xy


#!/bin/bash

./megatt --tree ./inputs/140k-family-tree.nwk --names-map ./inputs/140k-family-map.txt --ranks ./inputs/140k-family-ranks.txt --target-rank family -ci ./inputs/family_ci.csv -leaf-counts ./inputs/leafNodeCounts.csv --ids ./ids/ids.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt --outfile ./outputs/family.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - family tree"
else
    echo "FAILED - family tree"
fi
./megatt --tree ./inputs/140k-phylum-tree.nwk --names-map ./inputs/140k-phylum-map.txt --ranks ./inputs/140k-phylum-ranks.txt --target-rank phylum -ci inputs/phylum_ci.csv -leaf-counts inputs/leafNodeCounts.csv --ids ./ids/ids1.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt --outfile ./outputs/phylum.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree
if [ $? = 0 ]; then
    echo "Completed - phylum tree"
else
    echo "FAILED - phylum tree"
fi
./megatt --tree ./inputs/140k-genus-tree.nwk --names-map ./inputs/140k-genus-map.txt --ranks ./inputs/140k-genus-ranks.txt --target-rank genus -ci inputs/genus_ci.csv -leaf-counts inputs/leafNodeCounts.csv --ids ./ids/ids2.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt --outfile ./outputs/genus.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs

if [ $? = 0 ]; then
    echo "Completed - genus tree"
else
    echo "FAILED - genus tree"
fi

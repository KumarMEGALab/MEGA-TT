#!/bin/bash

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-alphabetical-asc.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-alphabetical-asc.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-alphabetical-asc"
else
    echo "FAILED - hominidae-alphabetical-asc"
fi

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-alphabetical-desc.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-alphabetical-desc.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-alphabetical-desc"
else
    echo "FAILED - hominidae-alphabetical-desc"
fi

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-HS-first.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-HS-first.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-HS-first"
else
    echo "FAILED - hominidae-HS-first"
fi

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-HS-last.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-HS-last.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-HS-last"
else
    echo "FAILED - hominidae-HS-last"
fi

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-numerical-asc.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-numerical-asc.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-numerical-asc"
else
    echo "FAILED - hominidae-numerical-asc"
fi

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-numerical-desc.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-numerical-desc.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-numerical-desc"
else
    echo "FAILED - hominidae-numerical-desc"
fi

./megatt --tree ./inputs/140k-species-tree.nwk --names-map ./inputs/140k-species-map.txt --ranks ./inputs/140k-species-ranks.txt --target-rank species -ci ./inputs/140k-species-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/hominidae-splits.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/hominidae-splits.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - hominidae-splits"
else
    echo "FAILED - hominidae-splits"
fi

./megatt --tree ./inputs/140k-family-tree.nwk --names-map ./inputs/140k-family-map.txt --ranks ./inputs/140k-family-ranks.txt --target-rank family -ci ./inputs/140k-family-ci.csv -leaf-counts ./inputs/140k-leafNodeCounts.csv --ids ./ids/ids.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/family.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs
if [ $? = 0 ]; then
    echo "Completed - family tree"
else
    echo "FAILED - family tree"
fi
./megatt --tree ./inputs/140k-phylum-tree.nwk --names-map ./inputs/140k-phylum-map.txt --ranks ./inputs/140k-phylum-ranks.txt --target-rank phylum -ci inputs/140k-phylum-ci.csv -leaf-counts inputs/140k-leafNodeCounts.csv --ids ./ids/ids1.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/phylum.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree
if [ $? = 0 ]; then
    echo "Completed - phylum tree"
else
    echo "FAILED - phylum tree"
fi
./megatt --tree ./inputs/140k-genus-tree.nwk --names-map ./inputs/140k-genus-map.txt --ranks ./inputs/140k-genus-ranks.txt --target-rank genus -ci inputs/140k-genus-ci.csv -leaf-counts inputs/140k-leafNodeCounts.csv --ids ./ids/ids2.txt --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --outfile ./outputs/genus.svg --prune --earth-impacts ./inputs/earth_impacts.csv --width 900 -geo-bg-colors periods -tree-name timetree -gs

if [ $? = 0 ]; then
    echo "Completed - genus tree"
else
    echo "FAILED - genus tree"
fi

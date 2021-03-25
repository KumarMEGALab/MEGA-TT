#!/bin/bash

./megatt --timeline ./inputs/human.json --width 600 --height 800 --outfile ./outputs/human.svg --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - human timeline"
else
    echo "FAILED - human timeline"
fi
./megatt --timeline ./inputs/spiders.json --width 600 --height 700 --outfile ./outputs/spiders.svg --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - spiders timeline"
else
    echo "FAILED - spiders timeline"
fi
./megatt --timeline ./inputs/amniota.json --width 800 --height 1200 --outfile ./outputs/amniota.svg --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed -  amniota timeline"
else
    echo "FAILED - amniota timeline"
fi
./megatt --timeline ./inputs/cyanobacteria.json --width 1200 --height 800 --outfile ./outputs/cyanobacteria.svg --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - cyanobacteria timeline"
else
    echo "FAILED - cyanobacteria timeline"
fi



#!/bin/bash

./megatt --pairwise ./inputs/lizard_bear.json --outfile ./output/lizard_bear.svg --width 700 --height 600 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - lizard-bear"
else
    echo "FAILED - lizard-bear"
fi

./megatt --pairwise ./inputs/chimp_bear.json --outfile ./output/chimp_bear.svg --width 700 --height 600 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - chimp-bear"
else
    echo "FAILED - chimp-bear"
fi
./megatt --pairwise ./inputs/human_mouse.json --outfile ./output/human_mouse.svg --width 700 --height 600 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - human-mouse"
else
    echo "FAILED - human-mouse"
fi

./megatt --pairwise ./inputs/lizard_snake.json --outfile ./output/lizard_snake.svg --width 700 --height 600 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - lizard-snake"
else
    echo "FAILED - lizard-snake"
fi

./megatt --pairwise ./inputs/ursus_enhydrus.json --outfile ./output/ursus_enhydrus.svg --width 700 --height 600 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - ursus_enhydrus"
else
    echo "FAILED - ursus_enhydrus"
fi

./megatt --pairwise ./inputs/cat_tiger.json --outfile ./output/cat_tiger.svg --width 700 --height 800 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - cat-tiger"
else
    echo "FAILED - cat-tiger"
fi

./megatt --pairwise ./inputs/rheum_coracina.json --outfile ./output/rheum_coracina.svg --width 700 --height 800 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - rheum-coracina"
else
    echo "FAILED - rheum-coracina"
fi

./megatt --pairwise ./inputs/small_div_time.json --outfile ./output/small_div_time.svg --width 700 --height 800 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - small div time"
else
    echo "FAILED - small div time"
fi

./megatt --pairwise ./inputs/outlier_example.json --outfile ./output/outlier_example.svg --width 700 --height 800 --earth-impacts ./inputs/earth_impacts.csv --o2 ./inputs/O2.txt --co2 ./inputs/CO2.txt --luminosity ./inputs/luminosity.txt
if [ $? = 0 ]; then
    echo "Completed - outlier example"
else
    echo "FAILED - outlier example"
fi

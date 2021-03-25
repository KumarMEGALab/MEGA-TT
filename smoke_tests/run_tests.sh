#!/bin/bash

cp ../megatt ./pairwiseSvg/
cp ../megatt ./timelinesearch/
cp ../megatt ./pruneTree_97k/
cp ../megatt ./newickToTabular/
cp ../megatt ./countRanks
cp ../megatt ./megatt_svg

cd pairwiseSvg
echo "### running pairwise SVG tests..."
./run_tests.sh

cd ../timelinesearch
echo "### running timeline tests"
./run_tests.sh

cd ../pruneTree_97k
echo "### running prune tree tests"
./run_tests.sh

cd ../megatt_svg
echo "### running svg tests"
./run_megatt_tests.sh

cd ../newickToTabular
echo "### running newick to tabular tests"
./run_megatt_tests.sh

#cd ../countRanks
#echo "### running rank counts"
#./doRanksCount.sh

echo "launching diff tool for results comparison"
cd ../pairwiseSvg
meld output expected

cd ../timelinesearch
meld outputs expected

cd ../pruneTree_97k
meld outputs expected




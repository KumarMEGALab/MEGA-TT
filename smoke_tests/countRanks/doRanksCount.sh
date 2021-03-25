#!/bin/bash

./megatt --count-ranks -t ./inputs/97k-genus-tree.nwk --names-map ./inputs/97k-genus-map.txt --ranks ./inputs/97k-genus-ranks.txt -o ./output/97K_genus_rankCounts.csv
./megatt --count-ranks -t ./inputs/97k-family-tree.nwk --names-map ./inputs/97k-family-map.txt --ranks ./inputs/97k-family-ranks.txt -o ./output/97K_family_rankCounts.csv
./megatt --count-ranks -t ./inputs/97k-order-tree.nwk --names-map ./inputs/97k-order-map.txt --ranks ./inputs/97k-order-ranks.txt -o ./output/97K_order_rankCounts.csv
./megatt --count-ranks -t ./inputs/97k-class-tree.nwk --names-map ./inputs/97k-class-map.txt --ranks ./inputs/97k-class-ranks.txt -o ./output/97K_class_rankCounts.csv
./megatt --count-ranks -t ./inputs/97k-phylum-tree.nwk --names-map ./inputs/97k-phylum-map.txt --ranks ./inputs/97k-phylum-ranks.txt -o ./output/97K_phylum_rankCounts.csv

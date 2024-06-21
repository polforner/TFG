# bt-graph-dp

MORENO CRIME:

stack exec bt-graph-dp -- +RTS -A1G -H1G -N6 -c -RTS -f ./experiments/diepfy/moreno_crime/input.txt -c ./experiments/diepfy/moreno_crime/c-edge-high.txt -e moreno_crime

stack exec bt-graph-dp -- +RTS -A1G -H1G -N6 -c -RTS -f ./experiments/diepfy/moreno_crime/input.txt -c ./experiments/diepfy/moreno_crime/all.txt -e moreno_crime

DBPEDIA:

stack exec bt-graph-dp -- +RTS -A1G -H1G -N6 -c -RTS -f ./experiments/diepfy/dbpedia/input.txt -c ./experiments/diepfy/dbpedia/c-edge-high.txt -e dbpedia

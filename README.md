# String Clustering

## Ada
* To build the main program: `make`. This will create the executable "obj/main"
* Options of the program and how to run it:
  * First option: path to the data file (String).
  * Second option: number of classes (Integer).
  * Third option: whether to recompute the distance matrix (Boolean).
  * For example: `obj/main ../sampled_keys_1000_over_1.txt 7 n`
* To run tests: `make test`

## Haskell
Assuming we're in the haskell folder:
* To build the programme: `runhaskell Setup.hs configure --user; runhaskell Setup.hs build`
* To run the programme:
  * First option: path to the data file (String).
  * Second option: number of classes (Integer).
  * Example 1, redis keys: `./dist/build/string_clustering/string_clustering ../sampled_500_keys.txt 5 +RTS -N8 -lf`
  * Example 2, citations: `for i in {2..30}; do ./dist/build/string_clustering/string_clustering ../cora-refs $i +RTS -N8; echo $i; done`
* To run tests: `runhaskell Test.hs`

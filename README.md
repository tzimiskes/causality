# causality

TODO:
* cgraph.R
 - [ ] as.dag: should pick a dag out of patterns/pdags
 - [ ] as.pattern: should convert pdags to patterns
 - [ ] as.pdag: needs to be implemented
 - [ ] is_valid_cgraph: needs to check to make sure if the adjacencies are legal
 - [ ] need to make man pages for the entire cgraph.R file
 - [ ] write more tests, eventually
* dag_to_pattern.R
- [ ] consider making dag_to_pattern private. If so, figure out what to 
      do with all the documentation
import_export.R
- [ ] add in support to convert R-causal output to cgraph
- [ ] rewrite import_from_tetrad_file so it doesn't suck
- [ ] Document (eventually)
- [ ] add in type checking as "as.foo" is implemented
* bootstrap.R
- [ ] continue to build out the file to facilitate more boostrapping
- [ ] document
- [ ] tests
* adjacency.R
- [ ] look at implementation to see why i index over names of skeleton
* shd.R
- [ ] redo type checking

* Alg Comparison stuff
- [ ] F1 Statistics
- [ ] other statistics Erich can think of
* Parameter Space Sensitivity Analysis

* Latent Stuff (It is the Forbidden Land, Simba)

* misc
- [ ] convert cgraph to lavaan equation
- [ ] implement a good partial correlations routine
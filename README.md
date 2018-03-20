# causality

TODO:
* cgraph.R
 - [ ] as.dag
   - [x] implement pdag/pattern support
   - [ ] write tests
   - [ ] document
 - [ ] as.pattern
   - [x] implement pdag --> pattern
   - [ ] tests
   - [ ] document
 - [ ] as.pdag:
 - [ ] implement (nothing?)
 - [ ] is_valid_cgraph: needs to check to make sure if the adjacencies are legal
   - [ ] we need to check to see that the edges/adjacencies are correct
 - [ ] is.cyclic
   - [ ] make it so
 - [ ] remove_cycles
   - probably useful when bootstrapping
 - [ ] need to make man pages for the entire cgraph.R file
- [ ] I forsee the need to add functions to add remove edges
* dag_to_pattern.R
- [x] make dag_to_pattern private.
- [ ] Figure out what to do with all the documentation
- [ ] tests (see as.pattern)
import_export.R
- [x] add in support to convert R-causal output to cgraph
- [ ] rewrite import_from_tetrad_file so it doesn't suck
- [ ] Document (eventually; it needs to not suck first)
- [ ] add in type checking as "as.foo" is implemented
* bootstrap.R
- [x] continue to build out the file to facilitate more boostrapping
- [ ] document
- [ ] tests
- [ ] figure out how to meek/ pattern the output
* adjacency.R
- [ ] look at implementation to see why i index over names of skeleton
- [ ] make it in C??
* arrowhead.R
* - [ ] see above
* shd.R
- [ ] redo type checking
* meek.R
- [x] implement
- [ ] test
- [ ] document
* Alg Comparison stuff
- [ ] F1 Statistics
- [ ] other statistics Erich can think of
- [ ] calculate_statistics function for ease of use in simulation studies
* Parameter Space Sensitivity Analysis

* Latent Stuff (It is the Forbidden Land, Simba)

* misc
- [ ] convert cgraph to lavaan equation
- [ ] implement a good partial correlations routine

* simluate datasets functionality
- [ ] input dag, distribution of variables, functional form between them

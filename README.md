# causality

TODO:
* PC
- [ ] Implement nth order partial correlations
- [ ] Implement Meek Orientation Rules
* Alg Comparison stuff
- [ ] Need examples and editing for AP etc
- [ ] tests
- [ ] add override so we can compare dags to patterns etc
- [ ] wrap AP, AHP etc in a function that directs the objects being compared to the correct version of AP etc. eg DAG/Patterns should go to ap.dag and PAGs go to ap.pag
* write versions of AR etc that works with latents
- [ ]  Graph Marginalization 
* extend cgraph to include DAG, Patterns, PAGs, PDAGs
- [ ] implement is.foo, as.foo etc
  - [X] is.foo
  - [X] as.dag
  - [ ] other as.foo
- [X] Implement SHD
  - [X] document
  - [ ] tests
  - [ ] comment
* DAG to Pattern Algorithm
- [X] Topological Ordering
- [X] Edge Ordering
- [X] Find Compelled
- [ ] Write tests
- [ ] Comment
- [ ] Document
- [ ] Examples
* Import and Utils
- [ ] Document
- [X] change import_from_tetrad_file so that the user includes the type of graph being imported
- [ ] write tests for import from tetrad
- [ ] add in type checking
* Bootstrapping
* Sensitivity Analysis

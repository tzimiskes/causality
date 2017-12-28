# causality

TODO:
* PC
- [ ] Implement nth order partial correlations
- [ ] Implement Meek Orientation Rules
* Alg Comparison stuff
- [ ] document AP, AHP, AP AHP
- [X] print warning when running AP etc on patterns with "<->"
- [x] return NA when there are 0 predicted arrows, or 0 true arrows
- [ ] write versions of AR etc that works with latents
- [ ] extend cgraph to include DAG, Patterns, and PAGs
- [ ] wrap AP, AHP etc in a function that directs the objects being compared to the correct version of AP etc. eg DAG/Patterns should go to ap.dag and PAGs go to ap.pag
- [ ] add override so we can compare dags to patterns etc
* Import and Utils
- [ ] Document
- [ ] change import_from_tetrad_file so that the user includes the type of graph being imported
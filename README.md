# causality

`causality` is still in early development, but there are a few useful things you do with the R package.
You can aggrate graphs into order to access the stabiliity of algorithms, you
can convert dags to patterns and patterns to dags, and you can employ
causality's (BETA) implementation of greedy equivalence search (GES) to learn
bayesian networks. You can also export the graphs to latex so you can put them in papers.

## Development Roadmap

Testing:
- GES (single threaded, continuous)
- exporting causality.graphs to LaTeX

Future major features:
- GES with BDeu scoring for discrete data
- GES with CG scoring for mixed data
- Multithreaded GES
- Latent Variable Models
- FCI /GFCI
- FOFC/FTFC

Future minor features
- function to remove cycles from cgraphs
- mutators to safely add and delete edges/nodes from graphs
- d-separation test
- markov boundary/blanket calculation
- partial correlations (needed for G/FCI)

If you have any feedback ( suggestions, bugs, feature requests), let me know so I can address it!

# causality

`causality` is still in early development, but there are a few useful things you do with the R package.
You can agregrate graphs into order to access the stabiliity of algorithms, you
can convert dags to patterns and patterns to dags, and you can employ
causality's (BETA) implementation of greedy equivalence search (GES) to learn
bayesian networks. You can also export the graphs to latex so you can put them in papers.

## Installation

You can install `casuality` via the R package `devtools`

```
# install.packages("devtools")
devtools::install_github("tzimiskes/causality", build_opts = c())
```

You will need a C compiler that supports C99. This can be accomplished by installing Rtools on Windows and Xcode
on MacOS.


If you have any feedback ( suggestions, bugs, feature requests), let me know so I can address it!

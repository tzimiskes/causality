/* Author: Alexander Rix
 * Date  : 11/30/18
 * Description:
 * score_graph.c implements a function to generate the score of a bayesian
 * network via a score_func. It scores the SEM y ~ Pa(y). This is currently
 * intended to be used on DAGs only.
 */

#include <stdlib.h>

#include "../headers/causality.h"
#include "../headers/dataframe.h"
#include "../headers/scores.h"

/*
 * score_graph calculates the (BIC-like) score of a graph using a causality
 * graph, the data associated with the graph, and score function (with
 * associated floating point args and integer args). Roughly, we use cg
 * to construct the model x --> y, where x:= Parents(y), and then score
 * the model given the data.
 */
double ccf_score_graph(struct cgraph *cg, struct dataframe df, score_func score,
                                          struct score_args args)
{
    double       graph_score = 0.0f;
    int          n_nodes     = cg->n_nodes;
    struct ill **parents     = cg->parents;
    struct ill **spouses     = cg->spouses;
    for (int i = 0; i < n_nodes; ++i) {
        struct ill *p = parents[i];
        struct ill *s = spouses[i];
        int  n  = ill_size(p) + ill_size(s);
        int *xy = malloc((n + 1) * sizeof(int));
        xy[n] = i; /* set y to i */
        int j = 0;
        while (p) {
            xy[j++] = p->key;
            p = p->next;
        }
        while (s) {
            xy[j++] = s->key;
            s = s->next;
        }
        graph_score += score(df, xy, n, args);
        free(xy);
    }
    return graph_score;
}

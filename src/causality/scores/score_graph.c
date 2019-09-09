/* Author: Alexander Rix
 * Date  : 11/30/18
 * Description:
 * score_graph.c implements a function to generate the score of a bayesian
 * network via a score_func. It scores the SEM y ~ Pa(y). This is currently
 * intended to be used on DAGs only.
 */

#include <stdlib.h>

#include <causality.h>
#include <dataframe.h>
#include <scores/scores.h>
#include <cgraph/cgraph.h>
#include <cgraph/edge_list.h>

/*
 * score_graph calculates the (BIC-like) score of a graph using a causality
 * graph, the data associated with the graph, and score function (with
 * associated floating point args and integer args). Roughly, we use cg
 * to construct the model x --> y, where x:= Parents(y), and then score
 * the model given the data.
 */
double causality_score_graph(struct cgraph *cg, struct dataframe *df, score_func
                                 score, struct score_args *args)
{
    double graph_score = 0.0f;
    struct edge_list **parents     = cg->parents;
    struct edge_list **spouses     = cg->spouses;
    for (int i = 0; i < cg->n_nodes; ++i) {
        struct edge_list *p = parents[i];
        struct edge_list *s = spouses[i];
        int  n  = size_edge_list(p) + size_edge_list(s);
        int *xy = malloc((n + 1) * sizeof(int));
        xy[n] = i; /* set y to i */
        int j = 0;
        while (p) {
            xy[j++] = p->node;
            p = p->next;
        }
        while (s) {
            xy[j++] = s->node;
            s = s->next;
        }
        graph_score += score(df, xy, n, args);
        free(xy);
    }
    return graph_score;
}

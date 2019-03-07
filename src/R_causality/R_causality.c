#include <causality.h>
#include <R_causality/R_causality.h>

const char *DIRECTED_STR      = "-->";
const char *UNDIRECTED_STR    = "---";
const char *PLUSPLUSARROW_STR = "++>";
const char *SQUIGGLEARROW_STR = "~~>";
const char *CIRCLEARROW_STR   = "o->";
const char *CIRCLECIRCLE_STR  = "o-o";
const char *BIDIRECTED_STR    = "<->";

const char *NODES_STR       = "nodes";
const char *EDGES_STR       = "edges";
const char *ADJACENCIES_STR = "adjacencies";

const char *CAUSALITY_GRAPH_CLASS   = "causality.graph";
const char *CAUSALITY_PATTERN_CLASS = "causality.pattern";

SEXP create_causality_graph(int n_edges, int n_nodes, SEXP nodes)
{
    SEXP graph = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(graph, NODES,       duplicate(nodes));
    SET_VECTOR_ELT(graph, EDGES,       allocMatrix(STRSXP, n_edges, 3));
    SET_VECTOR_ELT(graph, ADJACENCIES, allocVector(VECSXP, n_nodes));
    /* Names stores the names of the elements of Graph */
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(names, NODES,       mkChar(NODES_STR));
    SET_STRING_ELT(names, EDGES,       mkChar(EDGES_STR));
    SET_STRING_ELT(names, ADJACENCIES, mkChar(ADJACENCIES_STR));
    /* Set Names to be Graphs names attribute */
    setAttrib(graph, R_NamesSymbol, names);
    setAttrib(VECTOR_ELT(graph, ADJACENCIES), R_NamesSymbol, duplicate(nodes));
    /* Similarly, we do the same with class */
    SEXP class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar(CAUSALITY_GRAPH_CLASS));
    setAttrib(graph, R_ClassSymbol, class);
    UNPROTECT(1);
    return graph;
}

struct cgraph * cgraph_from_causality_graph(SEXP graph)
{
    int n_nodes      = length(VECTOR_ELT(graph, NODES));
    SEXP graph_nodes = VECTOR_ELT(graph, NODES);
    struct cgraph *cg  = create_cgraph(n_nodes);
    const char **nodes = malloc(n_nodes * sizeof(const char *));
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = CHAR(STRING_ELT(graph_nodes, i));
    SEXP  graph_edges = VECTOR_ELT(graph, EDGES);
    int   n_edges     = nrows(VECTOR_ELT(graph, EDGES));
    for (int i = 0; i < n_edges; ++i) {
        int x = node_to_int(CHAR(STRING_ELT(graph_edges, i)), nodes);
        int y = node_to_int(CHAR(STRING_ELT(graph_edges, i + n_edges)), nodes);
        short edge = edge_to_int(CHAR(STRING_ELT(graph_edges, i + 2 * n_edges)));
        add_edge_to_cgraph(cg, x, y, edge);
    }
    free(nodes);
    return cg;
}

SEXP causality_graph_from_cgraph(struct cgraph *cg, SEXP graph_nodes)
{
    int  n_nodes = cg->n_nodes;
    int  n_edges = cg->n_edges;
    SEXP graph   = PROTECT(create_causality_graph(n_edges, n_nodes, graph_nodes));
    /* Now, fill in the edge matrix */
    SEXP graph_edges = VECTOR_ELT(graph, EDGES);
    SEXP adjacencies = VECTOR_ELT(graph, ADJACENCIES);
    struct edge_list **parents  = cg->parents;
    struct edge_list **spouses  = cg->spouses;
    struct edge_list **children = cg->children;
    /*
     * optimization to speed up filling in the edge matrix. This is safe because
     * all of the nodes are already registered in R's global (string(?)) pool.
     */
    void ** nodes = malloc(n_nodes * sizeof(void *));
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = STRING_ELT(graph_nodes, i);
    /* indices for parent, child, edge columns */
    int p_i = 0;
    int c_i = n_edges;
    int e_i = 2 * n_edges;
    for (int i = 0; i < n_nodes; ++i) {
        int         node  = i;
        struct edge_list *p     = parents[i];
        struct edge_list *s     = spouses[i];
        struct edge_list *c     = children[i];
        int n_adjs = size_edge_list(p) + size_edge_list(s) + size_edge_list(c);
        int adj_i  = 0;
        if (n_adjs)
            SET_VECTOR_ELT(adjacencies, i, allocVector(STRSXP, n_adjs));
        else
            SET_VECTOR_ELT(adjacencies, i, R_NilValue);
        SEXP node_adjacents = VECTOR_ELT(adjacencies, i);
        while (p) {
            int parent = p->node;
            int edge   = p->edge;
            SET_STRING_ELT(node_adjacents, adj_i++, nodes[parent]);
            SET_STRING_ELT(graph_edges,          p_i++,   nodes[parent]);
            SET_STRING_ELT(graph_edges,          c_i++,   nodes[node]);
            /* edge might not registered */
            SET_STRING_ELT(graph_edges, e_i++, mkChar(edge_to_char(edge)));
            p = p->next;
        }
        while (s) {
            int spouse = s->node;
            int edge   = s->edge;
            /* to prevent an undirected edge from appearing twice. */
            SET_STRING_ELT(node_adjacents, adj_i++, nodes[spouse]);
            if (node < spouse) {
                SET_STRING_ELT(graph_edges, p_i++, nodes[spouse]);
                SET_STRING_ELT(graph_edges, c_i++, nodes[node]);
                SET_STRING_ELT(graph_edges, e_i++, mkChar(edge_to_char(edge)));
            }
            s = s->next;
        }
        while (c) {
            int child = c->node;
            SET_STRING_ELT(node_adjacents, adj_i++, nodes[child]);
            c = c->next;
        }
    }
    free(nodes);
    UNPROTECT(3);
    return graph;
}

int node_to_int(const char *node, const char **nodes)
{
    int i = 0;
    while (strcmp(node, nodes[i]))
        i++;
    return i;
}

/* converts an edge string to an integer */
int edge_to_int(const char *edge)
{
    if (!strcmp(edge, DIRECTED_STR))
        return DIRECTED;
    if (!strcmp(edge, UNDIRECTED_STR))
        return UNDIRECTED;
    if (!strcmp(edge, PLUSPLUSARROW_STR))
        return PLUSPLUSARROW;
    if (!strcmp(edge, SQUIGGLEARROW_STR))
        return SQUIGGLEARROW;
    if (!strcmp(edge, CIRCLEARROW_STR))
        return CIRCLEARROW;
    if (!strcmp(edge, CIRCLECIRCLE_STR))
        return CIRCLECIRCLE;
    if (!strcmp(edge, BIDIRECTED_STR))
        return BIDIRECTED;
    error("Unrecognized edge type!"); /* This should never happen */
}

/* convert edge int into an edge string */
const char * edge_to_char(int edge) {
    switch (edge) {
    case DIRECTED:
        return DIRECTED_STR;
    case UNDIRECTED:
        return UNDIRECTED_STR;
    case PLUSPLUSARROW:
        return PLUSPLUSARROW_STR;
    case CIRCLEARROW:
        return CIRCLEARROW_STR;
    case CIRCLECIRCLE:
        return CIRCLECIRCLE_STR;
    case BIDIRECTED:
        return BIDIRECTED_STR;
    default:
        error("Failed match integer edge_type to char * edge type!\n");
    }
}

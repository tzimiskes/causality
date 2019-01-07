#include "headers/causality.h"
#include "headers/causalityRWrapper.h"

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

static int          edge_to_int(const char *edge);
static const char * edge_to_char(int edge);
static int          node_to_int(const char *node, const char **nodes,
                                                  int n_nodes);

int * calculateEdgesPtr(SEXP Graph)
{
    SEXP         Edges  = PROTECT(VECTOR_ELT(Graph, EDGES));
    SEXP         Nodes  = PROTECT(VECTOR_ELT(Graph, NODES));
    int          n_nodes = length(Nodes);
    /* make a table so we can easily refer to the nodes */
    const char **nodes  = malloc(n_nodes * sizeof(const char*));
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = CHAR(STRING_ELT(Nodes, i));
    int  n_edges   = nrows(Edges);
    int *edges = malloc(n_edges * 3 * sizeof(int));
    for (int i = 0; i < 2 * n_edges; ++i)
        edges[i] = node_to_int(CHAR(STRING_ELT(Edges, i)), nodes, n_nodes);
    for (int i = 2 * n_edges; i < 3 * n_edges; ++i)
        edges[i] = edge_to_int(CHAR(STRING_ELT(Edges, i)));
    free(nodes);
    UNPROTECT(2);
    return(edges);
}

void calcluateEdgesFromCgraph(struct cgraph *cg, SEXP Graph)
{
    SEXP         Edges   = PROTECT(VECTOR_ELT(Graph, EDGES));
    SEXP         Nodes   = PROTECT(VECTOR_ELT(Graph, NODES));
    int          n_nodes = cg->n_nodes;
    int          n_edges = cg->n_edges;
    struct ill **parents = cg->parents;
    struct ill **spouses = cg->spouses;
    int          p_i     = 0;
    int          c_i     = n_edges;
    int          e_i     = 2 * n_edges;
    for (int j = 0; j < n_nodes; ++j) {
        struct ill *p     = parents[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            SET_STRING_ELT(Edges, p_i++, STRING_ELT(Nodes, parent));
            SET_STRING_ELT(Edges, c_i++, STRING_ELT(Nodes, child));
            SET_STRING_ELT(Edges, e_i++, mkChar(edge_to_char(edge)));
            p = p->next;;
        }
        p = spouses[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            /* this is to prevent an undirected edge from appearing twice. */
            if (child < parent) {
                SET_STRING_ELT(Edges, p_i++, STRING_ELT(Nodes, parent));
                SET_STRING_ELT(Edges, c_i++, STRING_ELT(Nodes, child));
                SET_STRING_ELT(Edges, e_i++, mkChar(edge_to_char(edge)));
            }
            p = p->next;
        }
    }
    UNPROTECT(2);
}

SEXP causalityGraphFromCgraph(struct cgraph *cg, SEXP Nodes)
{
    int  n_nodes  = cg->n_nodes;
    int  n_edges  = cg->n_edges;
    /* Allocate memory for the causality graph */
    SEXP Graph = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(Graph, NODES,       duplicate(Nodes));
    SET_VECTOR_ELT(Graph, EDGES,       allocMatrix(STRSXP, n_edges, 3));
    SET_VECTOR_ELT(Graph, ADJACENCIES, R_NilValue);
    /* Names stores the names of the elements of Graph */
    SEXP Names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(Names, NODES,       mkChar(NODES_STR));
    SET_STRING_ELT(Names, EDGES,       mkChar(EDGES_STR));
    SET_STRING_ELT(Names, ADJACENCIES, mkChar(ADJACENCIES_STR));
    /* Set Names to be Graphs names attribute */
    setAttrib(Graph, R_NamesSymbol, Names);
    /* Similarly, we do the same with class */
    SEXP Class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(Class, 0, mkChar(CAUSALITY_GRAPH_CLASS));
    setAttrib(Graph, R_ClassSymbol, Class);
    /* Now, fill in the edge matrix */
    SEXP Edges = VECTOR_ELT(Graph, EDGES);
    struct ill **parents = cg->parents;
    struct ill **spouses = cg->spouses;
    /* indices for parent, child, edge columns */
    int     p_i   = 0;
    int     c_i   = n_edges;
    int     e_i   = 2 * n_edges;
    void ** nodes = malloc(n_nodes * sizeof(void *));
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = STRING_ELT(Nodes, i);
    for (int i = 0; i < n_nodes; ++i) {
        struct ill *p      = parents[i];
        int         child  = i;
        while (p) {
            int parent = p->key;
            int edge   = p->value;
            SET_STRING_ELT(Edges, p_i++, nodes[parent]);
            SET_STRING_ELT(Edges, c_i++, nodes[child]);
            SET_STRING_ELT(Edges, e_i++, mkChar(edge_to_char(edge)));
            p = p->next;
        }
        p = spouses[i];
        while (p) {
            int parent = p->key;
            int edge   = p->value;
            /* this is to prevent an undirected edge from appearing twice. */
            if (child < parent) {
                SET_STRING_ELT(Edges, p_i++, nodes[parent]);
                SET_STRING_ELT(Edges, c_i++, nodes[child]);
                SET_STRING_ELT(Edges, e_i++, mkChar(edge_to_char(edge)));
            }
            p = p->next;
        }
    }
    free(nodes);
    UNPROTECT(3);
    return Graph;
}

int node_to_int(const char *node, const char **nodes, int n_nodes)
{
    for (int i = 0 ; i < n_nodes; ++i) {
        if (!strcmp(nodes[i], node))
            return i;
    }
    error("Failed to match node\n"); /* This should never happen */
}

/* converts an edge string to an integer */
static int edge_to_int(const char *edge)
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
static const char * edge_to_char(int edge) {
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

#include "headers/causality.h"
#include "headers/edgetypes.h"

const char *DIRECTED_STR      = "-->";
const char *UNDIRECTED_STR    = "---";
const char *PLUSPLUSARROW_STR = "++>";
const char *SQUIGGLEARROW_STR = "~~>";
const char *CIRCLEARROW_STR   = "o->";
const char *CIRCLECIRCLE_STR  = "o-o";
const char *BIDIRECTED_STR    = "<->";

static int match_edge(const char * edge_str);
static const char * char_edge_from_int(const int edge_type);
static int match_node(const char * node, const char ** nodes, int n_nodes);

int * calculate_edges_ptr(SEXP Graph)
{
    SEXP Edges          = PROTECT(VECTOR_ELT(Graph, EDGES));
    SEXP Nodes          = PROTECT(VECTOR_ELT(Graph, NODES));
    int n_nodes         = length(Nodes);
    const char **nodes = CALLOC(n_nodes, const char*);
    // make a table so we can easily refer to the nodes
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = CHAR(STRING_ELT(Nodes, i));
    int n_edges = nrows(Edges);
    int* edges_ptr    = malloc(n_edges*NCOL_EDGES*sizeof(int));
    for (int i = 0; i < 2*n_edges; ++i)
        edges_ptr[i] = match_node(CHAR(STRING_ELT(Edges, i)), nodes, n_nodes);
    for (int i = 2*n_edges; i < 3*n_edges; ++i)
        edges_ptr[i] = match_edge(CHAR(STRING_ELT(Edges, i)));
    FREE(nodes);
    UNPROTECT(2);
    return(edges_ptr);
}

SEXP calculate_edges_from_ptr(int *edges_ptr, SEXP Graph)
{
    SEXP Nodes          = PROTECT(VECTOR_ELT(Graph, NODES));
    int n_edges         = nrows(VECTOR_ELT(Graph, EDGES));
    SEXP Output         = PROTECT(allocMatrix(CHARSXP, n_edges, NCOL_EDGES));
    for (int i = 0; i < n_edges * 2; ++i)
        SET_STRING_ELT(Output, i, STRING_ELT(Nodes, edges_ptr[i]));
    for (int i = 2 * n_edges; i < n_edges*3; ++i)
        SET_STRING_ELT(Output, i, mkChar(char_edge_from_int(edges_ptr[i])));
    UNPROTECT(2);
    return(Output);
}

void recalculate_edges_from_cgraph(cgraph_ptr cg_ptr, SEXP Graph)
{
    SEXP Edges        = PROTECT(VECTOR_ELT(Graph, EDGES));
    SEXP Nodes        = PROTECT(VECTOR_ELT(Graph, NODES));
    int n_nodes       = cg_ptr->n_nodes;
    int n_edges       = cg_ptr->n_edges;
    struct ill **parents = cg_ptr->parents;
    struct ill **spouses = cg_ptr->spouses;
    int i         = 0;
    for (int j = 0; j < n_nodes; ++j) {
        struct ill *p     = parents[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            SET_STRING_ELT(Edges, i,             STRING_ELT(Nodes, parent));
            SET_STRING_ELT(Edges, i + n_edges,   STRING_ELT(Nodes, child));
            SET_STRING_ELT(Edges, i + 2 * n_edges,
                                  mkChar(char_edge_from_int(edge)));
            i++;
            p = p->next;;
        }
        p = spouses[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            /* this is to prevent an undirected edge from appearing twice. */
            if (child < parent) {
                SET_STRING_ELT(Edges, i,             STRING_ELT(Nodes, parent));
                SET_STRING_ELT(Edges, i + n_edges,   STRING_ELT(Nodes, child));
                SET_STRING_ELT(Edges, i + 2 * n_edges,
                                      mkChar(char_edge_from_int(edge)));
                i++;
            }
            p = p->next;
        }
    }
    UNPROTECT(2);
}

int match_node(const char *node, const char **nodes, int n_nodes)
{
    for (int i = 0 ; i < n_nodes; ++i) {
        if (!strcmp(nodes[i], node))
            return i;
    }
    error("Failed to match node\n"); /* This should never happen */
}

inline const char * char_edge_from_int(const int edge_type) {
  switch (edge_type) {
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

static int match_edge(const char *edge_str)
{
    if (!strcmp(edge_str, DIRECTED_STR))
        return DIRECTED;
    if (!strcmp(edge_str, UNDIRECTED_STR))
        return UNDIRECTED;
    if (!strcmp(edge_str, PLUSPLUSARROW_STR))
        return PLUSPLUSARROW;
    if (!strcmp(edge_str, SQUIGGLEARROW_STR))
        return SQUIGGLEARROW;
    if (!strcmp(edge_str, CIRCLEARROW_STR))
        return CIRCLEARROW;
    if (!strcmp(edge_str, CIRCLECIRCLE_STR))
        return CIRCLECIRCLE;
    if (!strcmp(edge_str, BIDIRECTED_STR))
        return BIDIRECTED;
    error("Unrecognized edge type!"); /* This will probably never happen */
}

int is_directed(int edge)
{
    return (edge == DIRECTED || edge == CIRCLEARROW ||
            edge == SQUIGGLEARROW || edge == PLUSPLUSARROW);
}

#include "headers/causality.h"
#include "headers/edgetypes.h"

const char *DIRECTED_STR      = "-->";
const char *UNDIRECTED_STR    = "---";
const char *PLUSPLUSARROW_STR = "++>";
const char *SQUIGGLEARROW_STR = "~~>";
const char *CIRCLEARROW_STR   = "o->";
const char *CIRCLECIRCLE_STR  = "o-o";
const char *BIDIRECTED_STR    = "<->";

static int          edgeToInt(const char * EdgeStr);
static const char * edgeToChar(int edgeType);
static int          nodeToInt(const char * node, const char **nodes, int n_nodes);

int * calculateEdgesPtr(SEXP Graph)
{
    SEXP         Edges  = PROTECT(VECTOR_ELT(Graph, EDGES));
    SEXP         Nodes  = PROTECT(VECTOR_ELT(Graph, NODES));
    int          nNodes = length(Nodes);
    const char **nodes  = CALLOC(nNodes, const char*);
    // make a table so we can easily refer to the nodes
    for (int i = 0; i < nNodes; ++i)
        nodes[i] = CHAR(STRING_ELT(Nodes, i));
    int  nEdges   = nrows(Edges);
    int *edgesPtr = malloc(nEdges * 3 * sizeof(int));
    for (int i = 0; i < 2 * nEdges; ++i)
        edgesPtr[i] = nodeToInt(CHAR(STRING_ELT(Edges, i)), nodes, nNodes);
    for (int i = 2 * nEdges; i < 3 * nEdges; ++i)
        edgesPtr[i] = edgeToInt(CHAR(STRING_ELT(Edges, i)));
    free(nodes);
    UNPROTECT(2);
    return(edgesPtr);
}

void calcluateEdgesFromCgraph(struct cgraph *cgPtr, SEXP Graph)
{
    SEXP         Edges       = PROTECT(VECTOR_ELT(Graph, EDGES));
    SEXP         Nodes       = PROTECT(VECTOR_ELT(Graph, NODES));
    int          nNodes      = cgPtr->n_nodes;
    int          nEdges      = cgPtr->n_edges;
    struct ill **parents     = cgPtr->parents;
    struct ill **spouses     = cgPtr->spouses;
    int          parentIndex = 0;
    int          childIndex  = nEdges;
    int          edgeIndex   = 2 * nEdges;
    for (int j = 0; j < nNodes; ++j) {
        struct ill *p     = parents[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            SET_STRING_ELT(Edges, parentIndex++, STRING_ELT(Nodes, parent));
            SET_STRING_ELT(Edges, childIndex++,  STRING_ELT(Nodes, child));
            SET_STRING_ELT(Edges, edgeIndex++,   mkChar(edgeToChar(edge)));
            p = p->next;;
        }
        p = spouses[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            /* this is to prevent an undirected edge from appearing twice. */
            if (child < parent) {
                SET_STRING_ELT(Edges, parentIndex++, STRING_ELT(Nodes, parent));
                SET_STRING_ELT(Edges, childIndex++,  STRING_ELT(Nodes, child));
                SET_STRING_ELT(Edges, edgeIndex++,   mkChar(edgeToChar(edge)));
            }
            p = p->next;
        }
    }
    UNPROTECT(2);
}

SEXP causalityGraphFromCgraph(struct cgraph *cg, SEXP Nodes)
{
    int          nNodes      = cg->n_nodes;
    int          nEdges      = cg->n_edges;
    SEXP Graph = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(Graph, NODES, duplicate(Nodes));
    SET_VECTOR_ELT(Graph, EDGES, allocMatrix(STRSXP, nEdges, 3));
    SET_VECTOR_ELT(Graph, ADJACENCIES, R_NilValue);
    SEXP Names = PROTECT(allocVector(STRSXP, 3));
    SEXP Class = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(Names, NODES, mkChar("nodes"));
    SET_STRING_ELT(Names, EDGES, mkChar("edges"));
    SET_STRING_ELT(Names, ADJACENCIES, mkChar("adjacencies"));
    SET_STRING_ELT(Class, 0, mkChar("causality.graph"));
    setAttrib(Graph, R_NamesSymbol, Names);
    setAttrib(Graph, R_ClassSymbol, Class);
    SEXP Edges = VECTOR_ELT(Graph, EDGES);
    struct ill **parents     = cg->parents;
    struct ill **spouses     = cg->spouses;
    int          parentIndex = 0;
    int          childIndex  = nEdges;
    int          edgeIndex   = 2 * nEdges;
    for (int j = 0; j < nNodes; ++j) {
        struct ill *p     = parents[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            SET_STRING_ELT(Edges, parentIndex++, STRING_ELT(Nodes, parent));
            SET_STRING_ELT(Edges, childIndex++,  STRING_ELT(Nodes, child));
            SET_STRING_ELT(Edges, edgeIndex++,   mkChar(edgeToChar(edge)));
            p = p->next;
        }
        p = spouses[j];
        while (p) {
            int parent = p->key;
            int child  = j;
            int edge   = p->value;
            /* this is to prevent an undirected edge from appearing twice. */
            if (child < parent) {
                SET_STRING_ELT(Edges, parentIndex++, STRING_ELT(Nodes, parent));
                SET_STRING_ELT(Edges, childIndex++,  STRING_ELT(Nodes, child));
                SET_STRING_ELT(Edges, edgeIndex++,   mkChar(edgeToChar(edge)));
            }
            p = p->next;
        }
    }
    UNPROTECT(3);
    return Graph;
}

int nodeToInt(const char *node, const char **nodes, int n_nodes)
{
    for (int i = 0 ; i < n_nodes; ++i) {
        if (!strcmp(nodes[i], node))
            return i;
    }
    error("Failed to match node\n"); /* This should never happen */
}

static int edgeToInt(const char *edgeStr)
{
    if (!strcmp(edgeStr, DIRECTED_STR))
        return DIRECTED;
    if (!strcmp(edgeStr, UNDIRECTED_STR))
        return UNDIRECTED;
    if (!strcmp(edgeStr, PLUSPLUSARROW_STR))
        return PLUSPLUSARROW;
    if (!strcmp(edgeStr, SQUIGGLEARROW_STR))
        return SQUIGGLEARROW;
    if (!strcmp(edgeStr, CIRCLEARROW_STR))
        return CIRCLEARROW;
    if (!strcmp(edgeStr, CIRCLECIRCLE_STR))
        return CIRCLECIRCLE;
    if (!strcmp(edgeStr,BIDIRECTED_STR))
        return BIDIRECTED;
    error("Unrecognized edge type!"); /* This should never happen */
}

static const char * edgeToChar(int edgeType) {
    switch (edgeType) {
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

int is_directed(int edge)
{
    return (edge == DIRECTED || edge == CIRCLEARROW ||
            edge == SQUIGGLEARROW || edge == PLUSPLUSARROW);
}

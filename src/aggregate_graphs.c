#include"headers/causality.h"
#include"headers/int_redblacktree.h"
#include"headers/edgetypes.h"


/* 1   2   3   4   5   6   7   8   9   10  11
 * <-- --- --> <~~ ~~> <++ ++> <-o o-> <-> o-o
 */
#define NUM_EDGES_STORED 11


/*
 * There is almost certainly a better way to do this, but as it currently stands
 * the underlying rbt implementation will not allow for a better one
 */
static const int ARR_BACKDIRECTED [11] = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static const int ARR_UNDIRECTED   [11] = {0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static const int ARR_DIRECTED     [11] = {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0};
static const int ARR_BACKSQUIGGLE [11] = {0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0};
static const int ARR_SQUIGGLE     [11] = {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0};
static const int ARR_BACKPLUS     [11] = {0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0};
static const int ARR_PLUS         [11] = {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0};
static const int ARR_BACKCIRCLE   [11] = {0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0};
static const int ARR_CIRCLE       [11] = {0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0};
static const int ARR_BIDIRECTED   [11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0};
static const int ARR_CIRCLECIRCLE [11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1};



void convert_tree_to_matrix(double* const restrict matrix_ptr,
                            const int n_rows , const int parent, int* index,
                            irbt_ptr root);

const int* edge_decider(int parent, int child, int edge);
SEXP c_dag_to_rbt(SEXP cgraphs) {

  const int n_graphs = length(cgraphs);
  const int n_nodes = length(VECTOR_ELT(VECTOR_ELT(cgraphs, 0), NODES));

  irbt_ptr** trees = malloc(n_graphs * sizeof(irbt_ptr*));
  // can parallelize
  for(int j = 0; j < length(cgraphs); ++j) {

    SEXP cgraph = PROTECT(VECTOR_ELT(cgraphs, j));

    trees[j] = make_ptr_to_irbt(n_nodes);
    // create alias for trees[j]
    irbt_ptr* tree = trees[j];

    SEXP edges = PROTECT(VECTOR_ELT(cgraph, EDGES));
    const int n_edges = nrows(edges);
    // number of edges times 2
    const int n_edges_t2 = 2*n_edges;
    const int* const edges_ptr = INTEGER(edges);
    for(int i = 0; i < n_edges; ++i) {
      const int parent = edges_ptr[i             ];
      const int child  = edges_ptr[i + n_edges   ];
      const int edge   = edges_ptr[i + n_edges_t2];
      const int* foo = edge_decider(parent, child, edge);

      if(parent < child)
        tree[parent] = irbt_insert(tree[parent], child, NUM_EDGES_STORED, foo);
      else
        tree[child]  = irbt_insert(tree[child], parent, NUM_EDGES_STORED, foo);
    }
    UNPROTECT(2);

  }


  //reduce  all trees to the base tree, which is an alias for tree[0]
  // "easy" to parallelize
  irbt_ptr* base = trees[0];
  for(int i = 1; i < n_graphs; ++i) {
    // create alias for trees[i]
    irbt_ptr* src = trees[i];
    for( int j = 0; j < n_nodes; ++j) {
      base[j] = irbt_merge_trees(base[j], src[j], NUM_EDGES_STORED);
    }
  }


  // free all the memory except the base tree
  // IF YOU TRY TO ACCESS ANY OF THE OTHER TREES PREPARE FOR SEG FAULTS
  for(int i = 1; i < n_graphs; ++i) {
    for(int j = 0; j <  n_nodes; ++j) {
      irbt_free(trees[i][j]);
    }
    free(trees[i]);
  }
  free(trees);

  // calculate the total number of different edges
  int n_rows = 0;
  for(int i = 0; i < n_nodes; ++i)
    n_rows += irbt_size(base[i]);

  SEXP output_matrix = PROTECT(allocMatrix(REALSXP, n_rows,  NUM_EDGES_STORED + 2));
  double* output_matrix_ptr = REAL(output_matrix);
  // 0 the matrix
  memset(output_matrix_ptr, 0, n_rows*(NUM_EDGES_STORED +2)* sizeof(double));

  int index = 0;
  for(int i = 0; i < n_nodes; ++i)
  convert_tree_to_matrix(output_matrix_ptr, n_rows, i, &index, base[i]);

  // free the last tree
  free(base);


  UNPROTECT(1);
  return(output_matrix);
}


void convert_tree_to_matrix(double* const restrict matrix_ptr,
                            const int n_rows , const int parent, int* index,
                            irbt_ptr root)
{
  if( root != NULL) {
  matrix_ptr[*index + 0*n_rows] = parent + 1;
  matrix_ptr[*index + 1*n_rows] = irbt_key(root) + 1;
  const int * const restrict root_values_ptr = irbt_values_ptr(root);
  for(int i = 0; i < NUM_EDGES_STORED; ++i)
    matrix_ptr[*index + (i+2)*n_rows] = root_values_ptr[i];
  (*index)++;
  convert_tree_to_matrix(matrix_ptr, n_rows, parent, index,
                         irbt_left_child(root));
  convert_tree_to_matrix(matrix_ptr, n_rows, parent, index,
                         irbt_right_child(root));
  }
}

const int* edge_decider(int parent, int child, int edge) {

  switch(edge) {
  case UNDIRECTED:
      return ARR_UNDIRECTED;
  case CIRCLECIRCLE:
    return ARR_CIRCLECIRCLE;
  case BIDIRECTED:
    return ARR_BIDIRECTED;
  }

  if(parent < child) {
    switch(edge) {
    case DIRECTED:
      return ARR_DIRECTED;
    case PLUSPLUSARROW:
      return ARR_PLUS;
    case SQUIGGLEARROW:
      return ARR_SQUIGGLE;
    case CIRCLEARROW:
      return ARR_CIRCLE;
    default:
      error("failed to bin edge. Unrecognized edge type!\n");
    }
  }
  else {
    switch(edge) {
    case DIRECTED:
      return ARR_BACKDIRECTED;
    case PLUSPLUSARROW:
      return ARR_BACKPLUS;
    case SQUIGGLEARROW:
      return ARR_BACKSQUIGGLE;
    case CIRCLEARROW:
      return ARR_BACKCIRCLE;
    default:
      error("failed to bin edge. Unrecognized edge type!\n");
    }
  }
}
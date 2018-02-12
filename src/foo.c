#include"headers/rapi.h"
#include"headers/int_rbt.h"

#define FORWARD 1 // -->
#define UNDIRECTED 2 // ---
#define BIDIRECTED 4 // <->
#define CIRCLECIRCLE // o-o
#define CIRCLEARROW 5 // o->


#define N_EDGETYPES 3

const int ARR_BACKDIRECTED [3] = {1, 0, 0};
const int ARR_DIRECTED     [3] = {0, 1, 0};
const int ARR_UNDIRECTED   [3] = {0, 0, 1};

int_rbt_node_ptr* make_int_rbt_hash_table(const int n) {
  int_rbt_node_ptr* hash_table = malloc(n*sizeof(int_rbt_node_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.\n");
  for(int i = 0; i < n; ++i)
    hash_table[i] = NULL;
  return(hash_table);
}

void convert_tree_to_matrix(double* const restrict matrix_ptr,
                            const int n_rows , const int parent, int* index,
                            int_rbt_node_ptr root);

SEXP c_dag_to_rbt(SEXP cgraphs) {

  const int n_graphs = length(cgraphs);
  const int n_nodes = length(VECTOR_ELT(VECTOR_ELT(cgraphs, 0), 0));

  int_rbt_node_ptr** trees = malloc(n_graphs * sizeof(int_rbt_node_ptr*));
  // can parallelize
  for(int j = 0; j < length(cgraphs); ++j) {

    SEXP cgraph = PROTECT(VECTOR_ELT(cgraphs, j));

    trees[j] = make_int_rbt_hash_table(n_nodes);
    // create alias for trees[j]
    int_rbt_node_ptr* rbt_hash = trees[j];

    SEXP edges = PROTECT(VECTOR_ELT(cgraph, 2));
    const int n_edges = nrows(edges);
    // number of edges times 2
    const int n_edges_t2 = 2*n_edges;
    const int* const edges_ptr = INTEGER(edges);
    for(int i = 0; i < n_edges; ++i) {
      const int parent = edges_ptr[i];
      const int child = edges_ptr[i + n_edges];
      const int edge = edges_ptr[i + n_edges_t2];
      int * count_to_add;

      switch(edge) {
        case FORWARD: {
          count_to_add = (int*) &ARR_DIRECTED[0];
          break;
        }
        case UNDIRECTED: {
          count_to_add = (int*) &ARR_UNDIRECTED[0];
          break;
        }
        default: {
          error("failed to bin edge. Unrecognized edge type!\n");
        }
      }
      if(parent < child) {
        rbt_hash[parent] = int_rbt_insert(rbt_hash[parent], child, N_EDGETYPES,
                                         count_to_add);
      } else {
        // if we need to put the edge in backwards,
        // then we also need to reverse the edge_type
        if (edge == FORWARD)
          count_to_add = (int*) &ARR_BACKDIRECTED[0];

        rbt_hash[child] = int_rbt_insert(rbt_hash[child], parent, N_EDGETYPES,
                                          count_to_add);
      }
    }
    UNPROTECT(2);
  }
  //reduce  all trees to the base tree, which is an alias for tree[0]
  // "easy" to parallelize
  int_rbt_node_ptr* base = trees[0];
  for(int i = 1; i < n_graphs; ++i) {
    // create alias for trees[i]
    int_rbt_node_ptr* src = trees[i];
    for( int j = 0; j < n_nodes; ++j) {
      base[j] = int_rbt_merge_trees(base[j], src[j], N_EDGETYPES);
    }
  }


  // free all the memory except the base tree
  // IF YOU TRY TO ACCESS ANY OF THE OTHER TREES PREPARE FOR SEG FAULTS
  for(int i = 1; i < n_graphs; ++i) {
    for(int j = 0; j <  n_nodes; ++j) {
      int_rbt_free(trees[i][j]);
    }
    free(trees[i]);
  }
  free(trees);

  // calculate the total number of different edges
  int n_rows = 0;
  for(int i = 0; i < n_nodes; ++i)
    n_rows += int_rbt_size(base[i]);

  SEXP output_matrix = PROTECT(allocMatrix(REALSXP, n_rows, N_EDGETYPES + 2));
  double* output_matrix_ptr = REAL(output_matrix);

  int index = 0;
  for(int i = 0; i < n_nodes; ++i) {
  convert_tree_to_matrix(output_matrix_ptr, n_rows, i, &index, base[i]);
  }
  free(base);
  UNPROTECT(1);
  return(output_matrix);
}


void convert_tree_to_matrix(double* const restrict matrix_ptr,
                            const int n_rows , const int parent, int* index,
                            int_rbt_node_ptr root)
{
  if( root != NULL) {
  matrix_ptr[*index + 0*n_rows] = parent + 1;
  matrix_ptr[*index + 1*n_rows] = int_rbt_key(root) + 1;
  const int * const restrict root_values_ptr = int_rbt_values_ptr(root);
  for(int i = 0; i < N_EDGETYPES; ++i)
    matrix_ptr[*index + (i+2)*n_rows] = root_values_ptr[i];
  (*index)++;
  convert_tree_to_matrix(matrix_ptr, n_rows, parent, index,
                         int_rbt_left_child(root));
  convert_tree_to_matrix(matrix_ptr, n_rows, parent, index,
                         int_rbt_right_child(root));
  }
}

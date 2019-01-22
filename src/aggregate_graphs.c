#include "headers/causalityRWrapper.h"
#include "headers/causality.h"
#include "headers/int_redblacktree.h"
#include "headers/edgetypes.h"

/* 1   2   3   4   5   6   7   8   9   10  11
 * <-- --- --> <~~ ~~> <++ ++> <-o o-> <-> o-o
 */
#define NUM_EDGES_STORED 11

/*
 * There is almost certainly a better way to do this, but as it currently stands
 * the underlying rbt implementation will not allow for a better one
 */
static float ARR_BACKDIRECTED [11] = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static float ARR_UNDIRECTED   [11] = {0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static float ARR_DIRECTED     [11] = {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0};
static float ARR_BACKSQUIGGLE [11] = {0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0};
static float ARR_SQUIGGLE     [11] = {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0};
static float ARR_BACKPLUS     [11] = {0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0};
static float ARR_PLUS         [11] = {0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0};
static float ARR_BACKCIRCLE   [11] = {0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0};
static float ARR_CIRCLE       [11] = {0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0};
static float ARR_BIDIRECTED   [11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0};
static float ARR_CIRCLECIRCLE [11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1};
/*
 * converts irbt to matrix
 */
void convert_tree_to_matrix(double* const restrict matrix_ptr,
                            const int n_rows , const int parent, int* index,
                            struct irbt *root, float sum_weights);
/*
 * FUNCTION_NAME takes in an edge (parent, child, edge) and then returns a
 * pointer to of the arrays above. That pointer is used to increment
 * the count of the particular edge type by 1 in the red black tree
 */
void add_edge_to_irbt(struct irbt *** root, int parent, int child, int edge, float weight);

/*
 * aggregate_cgraphs takes in a list of cgraphs and then reduces them to a new
 * aggregated cgraph object. The function acomplishes this by turning each
 * cgraph into a red black tree (irbt), and then combining the trees into one
 * The final tree is then converted into a matrix and returned
 */
SEXP cf_aggregate_cgraphs(SEXP Cgraphs, SEXP Weights) {

  const int n_graphs = length(Cgraphs);
  const int n_nodes = length(VECTOR_ELT(VECTOR_ELT(Cgraphs, 0), NODES));
  double* weights_ptr = REAL(Weights);

  float sum_weights = 0;
  for(int i = 0; i < n_graphs; ++i) {
    sum_weights += weights_ptr[i];
  }
  struct irbt *** trees = malloc(n_graphs * sizeof(struct irbt **));
  // can parallelize
  for(int j = 0; j < n_graphs; ++j) {

    float weight = (float) weights_ptr[j];
    SEXP Cgraph = PROTECT(VECTOR_ELT(Cgraphs, j));

    trees[j] = calloc(n_nodes, sizeof(struct irbt *));
    // create alias for trees[j]
    struct irbt ** tree = trees[j];

    SEXP edges = PROTECT(VECTOR_ELT(Cgraph, EDGES));
    const int n_edges = nrows(edges);
    // number of edges times 2
    const int n_edges_t2 = 2*n_edges;
    const int* const edges_ptr = INTEGER(edges);
    for(int i = 0; i < n_edges; ++i) {
      const int parent = edges_ptr[i             ];
      const int child  = edges_ptr[i + n_edges   ];
      const int edge   = edges_ptr[i + n_edges_t2];

      // for example (1,0, -->) would become (1,0, <--)
      /* determine how to add the edge to the irbt. determine_edge_array figures
       * out if the array will be forwards or backwards (if applicable)
       * and then if (p)
       *
       */
      add_edge_to_irbt(&tree, parent, child, edge, weight);

    }
    UNPROTECT(2); /* unprotect R objects */
  }
  //reduce  all trees to the base tree, which is an alias for tree[0]
  // "easy" to parallelize
  struct irbt ** base = trees[0];
  for(int i = 1; i < n_graphs; ++i) {
    // create alias for trees[i]
    struct irbt ** src = trees[i];
    for( int j = 0; j < n_nodes; ++j) {
      base[j] = irbt_merge_trees(base[j], src[j]);
    }
  }
 /*
  * free all the memory except the base tree
  * this is why i starts at 1 in the follorwing for loop; base = trees[0]
  * IF YOU TRY TO ACCESS ANY OF THE OTHER TREES PREPARE FOR SEG FAULTS
  */
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

  SEXP output_matrix = PROTECT(allocMatrix(REALSXP, n_rows, NUM_EDGES_STORED + 2));
  double* output_matrix_ptr = REAL(output_matrix);
  // 0 the matrix
  memset(output_matrix_ptr, 0, n_rows*(NUM_EDGES_STORED + 2)* sizeof(double));

  int index = 0;
  for(int i = 0; i < n_nodes; ++i)
    convert_tree_to_matrix(output_matrix_ptr, n_rows, i, &index, base[i], sum_weights);

  // free the last tree
  free(base);

  UNPROTECT(1);
  return(output_matrix);
}

/*
 * Convert an integer red black tree to a matrix
 */
void convert_tree_to_matrix(double* const restrict matrix_ptr,
                            const int n_rows , const int parent, int* index,
                            struct irbt *root, float sum_weights)
{
  if( root != NULL) {
    double inv_sum_weights = 1.0f/((double) sum_weights);
    matrix_ptr[*index + 0 * n_rows] = parent + 1;
    matrix_ptr[*index + 1 * n_rows] = irbt_key(root) + 1;
    float* root_values_ptr = irbt_values_ptr(root);
    for(int i = 0; i < NUM_EDGES_STORED; ++i) {
      matrix_ptr[*index + (i + 2) * n_rows] = ((double) root_values_ptr[i]) * inv_sum_weights;
    }
  (*index)++;
  convert_tree_to_matrix(matrix_ptr, n_rows, parent, index,
                         irbt_left_child(root), sum_weights);
  convert_tree_to_matrix(matrix_ptr, n_rows, parent, index,
                         irbt_right_child(root), sum_weights);
  }
}

void add_edge_to_irbt(struct irbt ***root, int parent, int child, int edge, float weight) {
  float *array = NULL;
  // all these edges are undirected, so we just first check these
  switch(edge) {
    case UNDIRECTED: {
      array = ARR_UNDIRECTED;
      break;
    }
    case CIRCLECIRCLE: {
      array = ARR_CIRCLECIRCLE;
      break;
    }
    case BIDIRECTED: {
      array = ARR_BIDIRECTED;
      break;
    }
  }
  // use the fact that parent < child or child < parent to determine
  // whether or not an edge should be forward or backward
  if(array == NULL) {
    if(parent < child) {
      switch(edge) {
      case DIRECTED: {
        array = ARR_DIRECTED;
        break;
      }
      case PLUSPLUSARROW: {
        array = ARR_PLUS;
        break;
      }
      case SQUIGGLEARROW: {
        array = ARR_SQUIGGLE;
        break;
      }
      case CIRCLEARROW: {
        array = ARR_CIRCLE;
        break;
      }
      default:
        error("failed to bin edge. Unrecognized edge type!\n");
      }
    }
    else {
      switch(edge) {
      case DIRECTED: {
        array = ARR_BACKDIRECTED;
        break;
      }
      case PLUSPLUSARROW: {
        array = ARR_BACKPLUS;
        break;
      }
      case SQUIGGLEARROW: {
        array = ARR_BACKSQUIGGLE;
        break;
      }
      case CIRCLEARROW: {
        array = ARR_BACKCIRCLE;
        break;
      }
      default:
        error("failed to bin edge. Unrecognized edge type!\n");
      }
    }
  }

  if(parent < child)
    (*root)[parent] = irbt_insert((*root)[parent], child, array, weight);
  else
    (*root)[child]  = irbt_insert((*root)[child], parent, array, weight);
}

#include "headers/causality.h"
#include "headers/int_linked_list.h"

typedef struct cmpct_cg* cmpct_cg_ptr;
typedef struct cmpct_cg {
  int_ll_ptr* parents;
  int n_nodes;
  int n_edges;
} cmpct_cg;

 cmpct_cg_ptr create_cmpct_cg(int n_nodes, int n_edges) {
   cmpct_cg_ptr cg = malloc(sizeof(cg));

   cg->n_nodes = n_nodes;
   cg->n_edges = n_edges;

   int_ll_ptr* parents = make_int_ll_hash_table(n_nodes + 1);
   if(parents == NULL)
    printf("Failed to allocate memory for parents pointer in cmpct_cg!\n");
   for(int i = 0; i < n_nodes; ++i)
      parents[i] = NULL;
    parents[n_nodes] = create_ill_ptr(n_nodes);
    if(parents == NULL)
     printf("Failed to allocate memory for nodes pointer in cmpct_cg!\n");

    cg->parents = parents;
    return cg;
}

void free_cmpct_cg(cmpct_cg_ptr cg) {
  free(cg->parents[cg->n_nodes]);
  free(cg->parents);
}


void fill_in_cmpct_cg(cmpct_cg_ptr cg, int* edges_ptr,
                      void (*insert_fp)(int_ll_ptr*, int, int, int*, int_ll_ptr)) {
  int n_edges    = cg->n_edges;
  int n_edges_2t = n_edges*2;


  int_ll_ptr* parents = cg->parents;
  int_ll_ptr nodes = parents[cg->n_nodes];
  int nodes_index = 0;
  for(int i = 0; i < n_edges; ++i) {
   int parent = edges_ptr[i                 ];
   int child  = edges_ptr[i + n_edges       ];
   int edge_type  = edges_ptr[i + n_edges_2t];
   fp(&parents[child], parent, edge_type, &nodes_index, nodes);
  }
}

 void print_cmpct_cg(cmpct_cg_ptr cg) {
  int_ll_ptr ptr;
  for(int i = 0; i < cg->n_nodes; ++i) {
    ptr = cg->parents[i];
   Rprintf("Parent: %i\n", i);
    int_ll_print(ptr);
   }
 }
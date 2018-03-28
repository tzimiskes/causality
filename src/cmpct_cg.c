#include "headers/causality.h"
#include "headers/int_linked_list.h"
#include "headers/edgetypes.h"

typedef struct cmpct_cg* cmpct_cg_ptr;
typedef struct cmpct_cg {
  ill_ptr* parents;
  int n_nodes;
  int n_edges;
} cmpct_cg;

 cmpct_cg_ptr create_cmpct_cg(int n_nodes, int n_edges) {
   cmpct_cg_ptr cg = malloc(sizeof(cg));

   cg->n_nodes = n_nodes;
   cg->n_edges = n_edges;

   ill_ptr* parents = create_ill_ptr_star(n_nodes + 1);
   if(parents == NULL)
    printf("Failed to allocate memory for parents pointer in cmpct_cg!\n");
    parents[n_nodes] = create_ill_ptr(n_edges);
    if(parents == NULL)
     printf("Failed to allocate memory for nodes pointer in cmpct_cg!\n");

    cg->parents = parents;
    return cg;
}

int get_cmpct_cg_n_edges (cmpct_cg_ptr cg) {
  return cg->n_edges;
}

 int get_cmpct_cg_n_nodes (cmpct_cg_ptr cg) {
   return cg->n_nodes;
 }

void free_cmpct_cg(cmpct_cg_ptr cg) {
  free(cg->parents[cg->n_nodes]);
  free(cg->parents);
  free(cg);
}


void fill_in_cmpct_cg(cmpct_cg_ptr cg, int* edges_ptr,
                      void (*insert_fp)(ill_ptr*, int, int, int, ill_ptr))
  {
  int n_edges    = cg->n_edges;
  int n_edges_2t = n_edges*2;


  ill_ptr* parents = cg->parents;
  ill_ptr nodes = parents[cg->n_nodes];

  for(int i = 0; i < n_edges; ++i) {
   int parent = edges_ptr[i                 ];
   int child  = edges_ptr[i + n_edges       ];
   int edge_type  = edges_ptr[i + n_edges_2t];
   insert_fp(&parents[child], parent, edge_type, i, nodes);
  }
}

 void print_cmpct_cg(cmpct_cg_ptr cg) {
  ill_ptr ptr;
  for(int i = 0; i < cg->n_nodes; ++i) {
    Rprintf("Parent: %i\n", i);
    ill_print(cg->parents[i]);
   }
 }


 ill_ptr* get_cmpct_cg_parents(cmpct_cg_ptr cg) {
   return cg->parents;
 }

 int directed_parent(cmpct_cg_ptr cg, const int node) {
   ill_ptr node_parents = cg->parents[node];
   while(node_parents != NULL) {
     if(ill_value(node_parents) == DIRECTED)
       return ill_key(node_parents);
     node_parents = ill_next(node_parents);
   }
   return -1;
 }

 int nodes_adjacent_in_cg(cmpct_cg_ptr cg, const int node1, const int node2) {
   ill_ptr node1_parents = cg->parents[node1];
   ill_ptr node2_parents = cg->parents[node2];
   while(node1_parents != NULL) {
     if(ill_key(node1_parents) == node2)
       return 1;
     node1_parents = ill_next(node1_parents);
   }
   while(node2_parents != NULL) {
     if(ill_key(node2_parents) == node1)
       return 1;
     node2_parents = ill_next(node2_parents);
   }
   return 0;
 }

 int undirected_edge_in_cg(cmpct_cg_ptr cg, const int node1, const int node2)  {
   ill_ptr node1_parents = cg->parents[node1];
   ill_ptr node2_parents = cg->parents[node2];
   while(node1_parents != NULL) {
     if(ill_key(node1_parents) == node2)
       return ill_value(node1_parents) == UNDIRECTED;
     node1_parents = ill_next(node1_parents);
   }
   while(node2_parents != NULL) {
     if(ill_key(node2_parents) == node1)
       return ill_value(node2_parents) == UNDIRECTED;
     node2_parents = ill_next(node2_parents);
   }
   return 0;
 }

/* orient node1 --> node2 */
void orient_cmpct_cg_edge(cmpct_cg_ptr cg, int node1, int node2) {
  Rprintf("%i --> %i\n", node1, node2);

  ill_ptr* parents = cg->parents;

  // check to see if node1 is already a parent of node2
  ill_ptr edge = ill_search(parents[node2], node1);
  if(edge != NULL) { /* if it is not NULL, set the edge type to directed */
    Rprintf("%p\n", edge);
    ill_set_value(edge, DIRECTED);
  }
  else {
    Rprintf("else\n");
    // node1 is a child of node2

    ill_ptr nodes = parents[cg->n_nodes];
    ill_ptr node1_parents = parents[node1];
    int index;
    if(ill_key(node1_parents) == node2) {
      index = node1_parents - nodes; /* ptr arithmetic -- gives offset */
      // remove the node and set root to be the next node in the list
      cg->parents[node1] = ill_next(parents[node1]);
    }
    else {
      while(node1_parents != NULL) {
        ill_ptr next = ill_next(node1_parents);
        if(ill_key(next) == node2) {
          index = next - nodes; /* ptr arithmetic */
          ill_set_next(node1_parents, ill_next(next));
          break;
        }
        node1_parents = next;
      }
    }
    ill_insert2(&parents[node2], node1, DIRECTED, index, nodes);
  }
}

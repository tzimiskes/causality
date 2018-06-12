#include"headers/causality.h"
#include"headers/cmpct_cg.h"
#include"headers/int_linked_list.h"
#include"headers/edgetypes.h"

typedef struct foo* foo_ptr;

typedef struct foo {
  ill_ptr children;
  ill_ptr parents;
  foo_ptr foo_next;
  foo_ptr foo_prev;
} foo;

int adjacent(foo foo, int node) {
  ill_ptr parents  = foo.parents;
  ill_ptr children = foo.children;

  while(parents != NULL) {
    if(ill_key(parents) == node)
      return 1;
    parents = ill_next(parents);
  }
  while(children != NULL) {
    if(ill_key(children) == node)
      return 1;
    children = ill_next(children);
  }
  return 0;
}

inline int is_sink(foo_ptr foop) {
  return foop->children == NULL;
}

/* check to see if all undirected parents are adj to all other parents */
int check_condition(foo_ptr foo, foo_ptr foos) {
  ill_ptr parents = foo->parents;
  ill_ptr tmp = parents;
  /* look for undirect parents of the node */
  while(tmp != NULL) {
    if(ill_value(tmp) == UNDIRECTED) {
      int node = ill_key(tmp);
      ill_ptr tmp2 = parents;
      while(tmp2 != NULL) {
        if(ill_key(tmp2) != node) {
          if(!adjacent(foos[node], ill_key(tmp2)))
            return 0;
        }
        tmp2 = ill_next(tmp2);
      }
    }
    tmp = ill_next(tmp);
  }
  return 1;
}
/*
 * orient_in_dag takes in a node, and points all undirected parents towards it
 * in the edge list
 */
void orient_in_dag(foo_ptr foos, foo_ptr current_node,
                   int* dag_edges_ptr, int n_edges, int* index)
  {
  ill_ptr parents = current_node->parents;
  int i           = *index;
  int node        = current_node - foos; /* ptr arithmetic */
  while(parents != NULL) {
    dag_edges_ptr[i          ] = node;
    dag_edges_ptr[i + n_edges] = ill_key(parents);
    i++;
    parents = ill_next(parents);
  }
  *index = i;
}

void remove_node(foo_ptr foos, foo_ptr current_node) {
  int node = current_node - foos; /* ptr arithmetic */

  ill_ptr parents = current_node->parents;
  while(parents != NULL) {
    int parent = ill_key(parents);
      if(ill_key(parents) == DIRECTED) {
        ill_delete(&(foos[parent].children), node);
      }
      else {
      /* parent is a undirected parent of node, so node is also an
       * undirected parent of parent */
        ill_delete(&(foos[parent].parents), node);
      }
    parents = ill_next(parents);
  }

  // unlink the node from the circular linked list
  current_node->foo_prev->foo_next = current_node->foo_next;
  current_node->foo_next->foo_prev = current_node->foo_prev;
}


SEXP cf_extend_pdag(SEXP Pdag) {
  int n_nodes = length(VECTOR_ELT(Pdag, NODES));
  int n_edges = nrows(VECTOR_ELT(Pdag, EDGES));

  int* edges_ptr    = INTEGER(VECTOR_ELT(Pdag, EDGES));
  int* parents_ptr  = edges_ptr;
  int* children_ptr = edges_ptr + n_edges;
  int* edgetype_ptr = edges_ptr + 2*n_edges;

  foo_ptr foos = calloc(n_nodes, sizeof(foo));

  if(foos == NULL)
    error("Failed to allocate memory for foos in cf_extend_pdag\n");

  // fill in foos parents and children
  for(int i = 0; i < n_edges; ++i) {
    int parent   = parents_ptr[i];
    int child    = children_ptr[i];
    int edgetype = edgetype_ptr[i];
    if(edgetype == DIRECTED) {
      foos[parent].children = ill_insert(foos[parent].children, child, DIRECTED);
      foos[child].parents   = ill_insert(foos[child].parents, parent, DIRECTED);
    }
    else
      foos[child].parents = ill_insert(foos[child].parents, parent, UNDIRECTED);
  }

  Rprintf("Filled in foos\n");
  // set up circular linked list
  // could be speed up as % is expensive. probably unimportant
  for(int i = 0; i < n_nodes; ++i) {
    foos[i].foo_next = foos + (i + 1) % n_nodes;
    foos[i].foo_prev = foos + (i - 1) % n_nodes;
  }
  Rprintf("Created circluar linked list\n");
  /*
   * create output dag by duplicated pdag and reseting the edge list
   */
  SEXP Dag = PROTECT(duplicate(Pdag));
  int* dag_edges_ptr = INTEGER(VECTOR_ELT(Dag, EDGES));

  memset(dag_edges_ptr+2*n_edges,  1, n_edges*sizeof(int));
  Rprintf("Memeset\n");
  foo_ptr current_node = foos;
  int n_nodes_checked  = 0;
  int ll_size          = n_nodes;
  /* Comment needed */
  int index = 0;
  while(ll_size > 0 && n_nodes_checked <= ll_size) {
    Rprintf("Node: %i\n", current_node - foos);
    if(is_sink(current_node) && check_condition(current_node, foos)) {
      orient_in_dag(foos, current_node, dag_edges_ptr, n_edges, &index);
      remove_node(foos, current_node);
      ll_size--;
      n_nodes_checked = 0;
      Rprintf("Reset counter\n");
    }
    else {
      n_nodes_checked++;
    }
    current_node = current_node->foo_next;
  }
  int failure = ll_size  > 0 ? 1 : 0;

  // free malloc'd memory
  for(int i = 0; i < n_nodes; ++i) {
    ill_free(foos[i].parents);
    ill_free(foos[i].children);
  }
  free(foos);
  UNPROTECT(1);
  if(failure)
    return R_NilValue;
  else
    return Dag;
}

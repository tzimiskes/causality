#include"headers/causality.h"
#include"headers/cmpct_cg.h"
#include"headers/int_linked_list.h"
#include"headers/edgetypes.h"

typedef struct foo* foo_ptr;

typedef struct foo {
  ill_ptr children;
  ill_ptr parents;
  foo_ptr next_node;
  foo_ptr prev_node;
} foo;

int adjacent(foo foo, int node) {
  ill_ptr parents  = foo.parents;
  while(parents != NULL) {
    if(ill_key(parents) == node)
      return 1;
    parents = ill_next(parents);
  }
  ill_ptr children = foo.children;
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
int check_condition(foo_ptr foo, foo_ptr nodes) {
  ill_ptr parents = foo->parents;
  ill_ptr tmp = parents;
  /* look for undirect parents of the node */
  while(tmp != NULL) {
    if(ill_value(tmp) == UNDIRECTED) {
      int node = ill_key(tmp);
      ill_ptr tmp2 = parents;
      while(tmp2 != NULL) {
        if(ill_key(tmp2) != node) {
          if(!adjacent(nodes[node], ill_key(tmp2)))
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
void orient_in_dag(foo_ptr nodes, foo_ptr current_node,
                   int* dag_edges_ptr, int n_edges, int* index)
  {
  ill_ptr parents = current_node->parents;
  int i           = *index;
  int node        = current_node - nodes; /* ptr arithmetic */
  while(parents != NULL) {
    dag_edges_ptr[i          ]   = ill_key(parents);
    dag_edges_ptr[i + n_edges]   = node;
    dag_edges_ptr[i + 2*n_edges] = DIRECTED;
    i++;
    parents = ill_next(parents);
  }
  *index = i;
}

void remove_node(foo_ptr nodes, foo_ptr current_node) {
  int node = current_node - nodes; /* ptr arithmetic */

/* we need to current node in all of its adjacents */
  ill_ptr parents = current_node->parents;
  while(parents != NULL) {
    int parent = ill_key(parents);
      if(ill_value(parents) == DIRECTED) {
        ill_delete(&(nodes[parent].children), node);
      }
      else {
      /* parent is a undirected parent of node, so node is also an
       * undirected parent of parent */
        ill_delete(&(nodes[parent].parents), node);
      }
    parents = ill_next(parents);
  }

  // unlink the node from the circular linked list

  current_node->prev_node->next_node = current_node->next_node;
  current_node->next_node->prev_node = current_node->prev_node;
}


SEXP cf_extend_pdag(SEXP Pdag) {
  int n_nodes       = length(VECTOR_ELT(Pdag, NODES));
  int n_edges       = nrows(VECTOR_ELT(Pdag, EDGES));
  int* edges_ptr    = INTEGER(VECTOR_ELT(Pdag, EDGES));
  int* parents_ptr  = edges_ptr;
  int* children_ptr = edges_ptr + n_edges;
  int* edgetype_ptr = edges_ptr + 2*n_edges;

  foo_ptr nodes = calloc(n_nodes, sizeof(foo));
  if(nodes == NULL)
    error("Failed to allocate memory for nodes in cf_extend_pdag\n");

  // fill in nodes parents and children
  for(int i = 0; i < n_edges; ++i) {
    int parent   = parents_ptr[i];
    int child    = children_ptr[i];
    int edgetype = edgetype_ptr[i];
    if(edgetype == DIRECTED) {
      nodes[parent].children = ill_insert(nodes[parent].children, child, DIRECTED);
      nodes[child].parents   = ill_insert(nodes[child].parents, parent, DIRECTED);
    }
    else {
      nodes[child].parents = ill_insert(nodes[child].parents, parent, UNDIRECTED);
      nodes[parent].parents = ill_insert(nodes[parent].parents, child, UNDIRECTED);
    }
  }

  // set up circular linked list
  for(int i = 0; i < n_nodes; ++i) {
    nodes[i].next_node = nodes + (i + 1) % n_nodes;
    nodes[i].prev_node = nodes + (i + n_nodes - 1) % n_nodes;
  }

  // create output Dag by duplicating the  Pdag and reseting the edge list
  SEXP Dag = PROTECT(duplicate(Pdag));
  int* dag_edges_ptr = INTEGER(VECTOR_ELT(Dag, EDGES));
  memset(dag_edges_ptr, 0, 3*n_edges*sizeof(int));

  foo_ptr current_node = nodes;
  int n_nodes_checked  = 0;
  int ll_size          = n_nodes;
  /* Comment needed */
  int index = 0;
  while(ll_size > 0 && n_nodes_checked < ll_size) {
    if(is_sink(current_node) && check_condition(current_node, nodes)) {
      orient_in_dag(nodes, current_node, dag_edges_ptr, n_edges, &index);
      remove_node(nodes, current_node);
      ll_size--;
      n_nodes_checked = 0;
    }
    else {
      n_nodes_checked++;
    }
    current_node = current_node->next_node;
  }
  // check to see if the algorithm failed to generate an extension
  int failure = ll_size  > 0 ? 1 : 0;

  // free malloc'd memory
  for(int i = 0; i < n_nodes; ++i) {
    ill_free(nodes[i].parents);
    ill_free(nodes[i].children);
  }
  free(nodes);
  UNPROTECT(1);
  if(failure)
    return R_NilValue;
  else
    return Dag;
}

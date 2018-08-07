#include <causality.h>
#include <int_linked_list.h>
#include <edgetypes.h>
#include <cgraph.h>

cgraph_ptr create_cgraph(int n_nodes) {
  cgraph_ptr cg_ptr = malloc(sizeof(cgraph));
  if(cg_ptr == NULL)
    error("Failed to allocate memory for cgraph!\n");
  cg_ptr->n_nodes  = n_nodes;
  cg_ptr->n_edges  = UNDEFINED; /* 0 */
  cg_ptr->parents  = create_ptr_to_ill_ptr(n_nodes);
  if(cg_ptr->parents == NULL)
    error("Failed to allocate memory for parents in cgraph!\n");
  cg_ptr->children = create_ptr_to_ill_ptr(n_nodes);
  if(cg_ptr->children == NULL)
    error("Failed to allocate memory for children in cgraph!\n");
  cg_ptr->spouses = create_ptr_to_ill_ptr(n_nodes);
  if(cg_ptr->spouses == NULL)
    error("Failed to allocate memory for spouses in cgraph!\n");
  return cg_ptr;
}

void fill_in_cgraph(cgraph_ptr cg_ptr, int n_edges, int * edges_ptr) {
  cg_ptr->n_edges     = n_edges;
  int * node1_ptr     = edges_ptr;
  int * node2_ptr     = edges_ptr + n_edges;
  int * edge_type_ptr = edges_ptr + 2 * n_edges;
  ill_ptr * parents   = cg_ptr->parents;
  ill_ptr * children  = cg_ptr->children;
  ill_ptr * spouses   = cg_ptr->spouses;
  for(int i = 0; i < n_edges; ++i) {
    int node1 = node1_ptr[i];
    int node2 = node2_ptr[i];
    int edge  = edge_type_ptr[i];
    if(is_directed(edge)) {
      children[node1] = ill_insert(children[node1], node2, edge);
      parents[node2]  = ill_insert(parents[node2],  node1, edge);
    }
    else {
      spouses[node1]  = ill_insert(spouses[node1],  node2, edge);
      spouses[node2]  = ill_insert(spouses[node2],  node1, edge);
    }
  }
}

cgraph_ptr copy_cgraph(cgraph_ptr cg_ptr) {
  cgraph_ptr copy_ptr     = create_cgraph(cg_ptr->n_nodes);
  copy_ptr->n_edges       = cg_ptr->n_edges;
  ill_ptr * parents       = cg_ptr->parents;
  ill_ptr * spouses       = cg_ptr->spouses;
  ill_ptr * children      = cg_ptr->children;
  ill_ptr * copy_parents  = copy_ptr->parents;
  ill_ptr * copy_children = copy_ptr->children;
  ill_ptr * copy_spouses  = copy_ptr->spouses;
  for(int i = 0; i < cg_ptr->n_nodes; ++i) {
    copy_parents[i]  = copy_ill(parents[i]);
    copy_children[i] = copy_ill(children[i]);
    copy_spouses[i]  = copy_ill(spouses[i]);
  }
  return copy_ptr;
}

void add_node_to_cgraph(cgraph_ptr cg_ptr, int node1, int node2, int edge) {
  ill_ptr * parents   = cg_ptr->parents;
  ill_ptr * children  = cg_ptr->children;
  ill_ptr * spouses   = cg_ptr->spouses;
  if(is_directed(edge)) {
    children[node1] = ill_insert(children[node1], node2, edge);
    parents[node2]  = ill_insert(parents[node2],  node1, edge);
  }
  else {
    spouses[node1] = ill_insert(spouses[node1], node2, edge);
    spouses[node2] = ill_insert(spouses[node2], node1, edge);
  }
  cg_ptr->n_edges++;
}

void free_cgraph(cgraph_ptr cg_ptr) {
  cgraph cg          = *cg_ptr;
  ill_ptr * parents  = cg.parents;
  ill_ptr * children = cg.children;
  ill_ptr * spouses  = cg.spouses;
  for(int i = 0; i < cg_ptr->n_nodes; ++i) {
    ill_free(parents[i]);
    ill_free(children[i]);
    ill_free(spouses[i]);
  }
  free(parents);
  free(children);
  free(spouses);
  free(cg_ptr);
}

int adjacent_in_cgraph(cgraph_ptr cg_ptr, int node1, int node2) {
  cgraph cg   = *cg_ptr;
  ill_ptr tmp = cg.parents[node1];
  while(tmp) {
    ill node = *tmp;
    if(node.key == node2)
      return 1;
    tmp = node.next;
  }
  tmp = cg.children[node1];
  while(tmp) {
    ill node = *tmp;
    if(node.key == node2)
      return 1;
    tmp = node.next;
  }
  tmp = cg.spouses[node1];
  while(tmp) {
    ill node = *tmp;
    if(node.key == node2)
      return 1;
    tmp = node.next;
  }
  return 0;
}

int edge_undirected_in_cgraph(cgraph_ptr cg_ptr, int node1, int node2) {
  ill_ptr spouses = cg_ptr->spouses[node1];
  while(spouses) {
    ill spouse = *spouses;
    if(spouse.key == node2)
      return spouse.value == UNDIRECTED;
    spouses = spouse.next;
  }
  return 0;
}

int edge_directed_in_cgraph(cgraph_ptr cg_ptr, int parent, int child) {
  ill_ptr children = cg_ptr->children[parent];
  while(children) {
    ill node = *children;
    if(node.key == child)
      return node.value == DIRECTED;
    children = node.next;
  }
  return 0;
}

void orient_undirected_edge(cgraph_ptr cg_ptr, int parent, int child) {
  ill_ptr node = NULL;
  if(cg_ptr->spouses[child]->key == parent) {
    node = cg_ptr->spouses[child];
    cg_ptr->spouses[child] = cg_ptr->spouses[child]->next;
  }
  else {
    ill_ptr spouses = cg_ptr->spouses[child];
    while(spouses->next) {
      if(spouses->next->key == parent) {
        node = spouses->next;
        spouses->next = spouses->next->next;
        break;
      }
      spouses = spouses->next;
    }
  }
  node->next             = cg_ptr->parents[child];
  node->value            = DIRECTED;
  cg_ptr->parents[child] = node;
  /* now we need to do the oher */
  if(cg_ptr->spouses[parent]->key == child) {
    node = cg_ptr->spouses[parent];
    cg_ptr->spouses[parent] = cg_ptr->spouses[parent]->next;
  }
  else {
    ill_ptr spouses = cg_ptr->spouses[parent];
    while(spouses->next) {
      if(spouses->next->key == child) {
        node = spouses->next;
        spouses->next = spouses->next->next;
        break;
      }
      spouses = spouses->next;
    }
  }
  node->next               = cg_ptr->children[parent];
  node->value              = DIRECTED;
  cg_ptr->children[parent] = node;
}

void unorient_directed_edge(cgraph_ptr cg_ptr, int parent, int child) {
  ill_ptr node = NULL;
  if(cg_ptr->parents[child]->key == parent) {
    node = cg_ptr->parents[child];
    cg_ptr->parents[child] = cg_ptr->parents[child]->next;
  }
  else {
    ill_ptr parents = cg_ptr->parents[child];
    while(parents->next) {
      if(parents->next->key == parent) {
        node = parents->next;
        parents->next = parents->next->next;
        break;
      }
      parents = parents->next;
    }
  }
  node->next             = cg_ptr->spouses[child];
  node->value            = UNDIRECTED;
  cg_ptr->spouses[child] = node;
  /* now we need to do the oher */
  if(cg_ptr->children[parent]->key == child) {
    node = cg_ptr->children[parent];
    cg_ptr->children[parent] = cg_ptr->children[parent]->next;
  }
  else {
    ill_ptr children = cg_ptr->children[parent];
    while(children->next) {
      if(children->next->key == child) {
        node = children->next;
        children->next = children->next->next;
        break;
      }
      children = children->next;
    }
  }
  node->next              = cg_ptr->spouses[parent];
  node->value             = UNDIRECTED;
  cg_ptr->spouses[parent] = node;
}

void print_cgraph(cgraph_ptr cg_ptr) {
  cgraph cg = *cg_ptr;
  for (int i = 0; i < cg.n_nodes; ++i) {
    Rprintf("Parents of %i:\n", i);
    ill_print(cg.parents[i]);
    Rprintf("Spouses of  %i:\n", i);
    ill_print(cg.spouses[i]);
    Rprintf("Children of  %i:\n", i);
    ill_print(cg_ptr->children[i]);
    Rprintf("\n");
  }
}

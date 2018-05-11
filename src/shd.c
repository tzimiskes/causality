#include"headers/causality.h"
#include"headers/edgetypes.h"
#include"headers/int_linked_list.h"
#include"headers/cmpct_cg.h"
/*
 * cf_structural_hamming_distance calculates the SHD between two PDAGS as
 * described in Tsamardinos, L.E. Brown and C.F. Aliferis (2006).
 * The Max-Min Hill-Climbing Bayesian Network Structure Learning Algorithm, JMLR
 */
SEXP cf_structural_hamming_distance(SEXP Pdag1, SEXP Pdag2, SEXP K) {
  // turn K into a c integer
  int k             = asInteger(K);
  // grab parameter info from the pdags
  int n_nodes       = length(VECTOR_ELT(Pdag1, NODES));
  int n_pdag1_edges = nrows(VECTOR_ELT(Pdag1,  EDGES));
  int n_pdag2_edges = nrows(VECTOR_ELT(Pdag2,  EDGES));

  int* pdag1_edges_ptr = INTEGER(VECTOR_ELT(Pdag1, EDGES));
  int* pdag2_edges_ptr = INTEGER(VECTOR_ELT(Pdag2, EDGES));

  cmpct_cg_ptr cg_pdag1_ptr = create_cmpct_cg(n_nodes, n_pdag1_edges);
  cmpct_cg_ptr cg_pdag2_ptr = create_cmpct_cg(n_nodes, n_pdag2_edges);

  fill_in_cmpct_cg(cg_pdag1_ptr, pdag1_edges_ptr, ill_insert2);
  fill_in_cmpct_cg(cg_pdag2_ptr, pdag2_edges_ptr, ill_insert2);

  int distance = 0;
  for(int i = 0; i < n_pdag1_edges; ++i) {
    int parent   = pdag1_edges_ptr[i                  ];
    int child    = pdag1_edges_ptr[i +   n_pdag1_edges];
    int edgetype = pdag1_edges_ptr[i + 2*n_pdag1_edges];
    // check to see if the edge in pdag1 is also in pdag2
    if(adjacent_in_cg(cg_pdag2_ptr, parent, child)) {
      if(edgetype == DIRECTED){
        // check to see if the edge is misoriented in pdag2
        if(!edge_directed_in_cg(cg_pdag2_ptr, parent, child))
          distance++;
      }
      else {
        if(!edge_undirected_in_cg(cg_pdag2_ptr, parent, child))
          distance++;
      }
    }
    else /* the edge is missing in pdag2. increment by k */
      distance += k;
  }
  // now, we only need to check to see what edges in pdag2 are not in pdag1
  for(int i = 0; i < n_pdag2_edges; ++i) {
    int parent   = pdag2_edges_ptr[i                  ];
    int child    = pdag2_edges_ptr[i +   n_pdag2_edges];
    int edgetype = pdag2_edges_ptr[i + 2*n_pdag2_edges];
    if(!adjacent_in_cg(cg_pdag1_ptr, parent, child))
      distance += k;
  }
  // free malloc'd memory
  free_cmpct_cg(cg_pdag1_ptr);
  free_cmpct_cg(cg_pdag2_ptr);
  return ScalarInteger(distance);
}

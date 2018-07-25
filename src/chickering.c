#include "headers/causality.h"
#include "headers/cmpct_cg.h"
#include "headers/int_linked_list.h"
#include "headers/edgetypes.h"
#include "headers/int_a_stack.h"

// macros for dag_to_pattern
#define UNKNOWN -1
#define COMPELLED 1
#define REVERSABLE 2


// These two functions implement the topological sort as described in CLRS
// topological sort returns a SEXP because there's an R fuction which
// the user can call to get the topoligical sort
int * ccf_sort(int n_nodes, const ill_ptr * restrict children);
void order_edges(const int * sort_ptr, const int n_nodes, ill_ptr * parents);

SEXP ccf_chickering_wrapper(SEXP Graph);

ill_ptr * ccf_chickering(ill_ptr * parents, const int n_nodes) {

  // probably some kind of memory manipulation ?


  int * sort_ptr = ccf_sort(n_nodes, parents);
  // generate a sort

  // order the edges

  // core chickering algorithm,
  free(sort_ptr);
  // return pointer
  return parents;
}




ill_ptr cf_dag_to_pattern(ill_ptr * parents, const int n_nodes, const int* sort_ptr) {

  // order edges sets the value parameter for each edge, so we need to
  // change the value for everything to UNKNOWN
  for(int i = 0; i < n_nodes; ++i) {
    ill_ptr tmp_ptr = parents[i];
    while(tmp_ptr != NULL) {
      ill_set_value(tmp_ptr, UNKNOWN);
      tmp_ptr = ill_next(tmp_ptr);
    }
  }

  for(int i = 0; i < n_nodes; ++i) {
    // by lemma 5 in Chickering, all the incident edges on y are unknown
    // so we don't need to check to see its unordered
    // look at the edges that go into y

    const int y     = sort_ptr[i];
    // parents of y
    ill_ptr poy_ptr = parents[y];

    // if there are incident edges into y, run steps 5-8 of the algorithm.
    // if y has no incident edges, go to the next node in the
    // topological order
    if(poy_ptr != NULL) {

      const int x        = ill_key(poy_ptr);
      ill_ptr pox_ptr    =  parents[x];

      // for each parent of x, w, where w -> x is compelled
      // check to see if w forms the chain w -> x -> y
      // or shielded collider w -> x -> y,
      // and w -> x
      while(pox_ptr != NULL) {

        if (ill_value(pox_ptr) == COMPELLED) {
          const int w = ill_key(pox_ptr);
          // parents of y duplicate
          ill_ptr poy_dup_ptr = poy_ptr;

          int chain = 1;
          while(poy_dup_ptr != NULL) {
            if(ill_key(poy_dup_ptr) == w) {
              // the triple forms a shielded collider so execute step 7
              // set w -> y compelled
              ill_set_value(poy_dup_ptr, COMPELLED);
              chain = 0;
              break;
            }
            else
              poy_dup_ptr = ill_next(poy_dup_ptr);
          }

          // if the triple forms a chain so execute step 6
          if(chain) {
            // reset poy_dup
            poy_dup_ptr = poy_ptr;
            while(poy_dup_ptr != NULL) {
              ill_set_value(poy_dup_ptr, COMPELLED);
              poy_dup_ptr = ill_next(poy_dup_ptr);
            }
            // jump to the end of the for loop
            goto JMP_TO_EOFL;
          }
        }
        // if step 7 is executed, goto the next grandparent
        pox_ptr = ill_next(pox_ptr);
      }
      // now, we need to search for z, where z -> y, x != z,
      // and z is not a parent of x. That is, an unshielded collider

      // by starting at the second parent (might not exist),
      // we avoid the need to check to see if z = x
      // STEP 7.5: look for an unshielded collider
      int unshielded_collider = 0;
      ill_ptr poy_dup_ptr = parents[y];
      while(poy_dup_ptr != NULL) {
        if(ill_key(poy_dup_ptr) != x) {
          const int z = ill_key(poy_dup_ptr);
          // reset parents of x
          if(!adjacent_in_cg(cg, x, z)) {
            unshielded_collider = 1;
            goto STEP_89;
          }
        }
        poy_dup_ptr = ill_next(poy_dup_ptr);
      }

      STEP_89: {};
      // STEP 8, if there is one, label all incident edges compelled
      if(unshielded_collider) {
        while(poy_ptr != NULL) {
          ill_set_value(poy_ptr, COMPELLED);
          poy_ptr = ill_next(poy_ptr);
        }
      }
      // STEP 9, label all unknown edges reversable
      else {
        while(poy_ptr != NULL) {
          if(ill_value(poy_ptr) == UNKNOWN)
            ill_set_value(poy_ptr, REVERSABLE);
          poy_ptr = ill_next(poy_ptr);
        }
      }
    }
    // JUMP TO END OF FOR LOOP
    JMP_TO_EOFL:{};
  }
  return parents;
}

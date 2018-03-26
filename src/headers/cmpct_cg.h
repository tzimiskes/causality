#ifndef _CMPCT_CG_
#define _CMPCT_CG_
typedef struct cmpct_cg* cmpct_cg_ptr;
typedef struct cmpct_cg cmpct_cg;
cmpct_cg_ptr create_cmpct_cg(int n_nodes, int n_edges);

void free_cmpct_cg(cmpct_cg_ptr cg);
void fill_in_cmpct_cg(cmpct_cg_ptr cg, int* edges_ptr);
void print_cmpct_cg(cmpct_cg_ptr cg);

#endif
#ifndef GES_UTILS_H
#define GES_UTILS_H

struct gesrec {
    int   x;
    int   y;
    int   set_size;
    int   naxy_size;
    int  *set;
    int  *naxy;
}; /*  32 bytes */

void   free_gesrec(struct gesrec g);
int    forms_clique(struct cgraph *cg, int *s_u_naxy, int s_u_naxy_size);
int    cycle_created(struct cgraph *cg, int y, int x, int *s_u_naxy,
                                        int s_u_naxy_size, int *mem);
void   partition_neighbors(struct cgraph *cg, struct gesrec *g);
int  * deterimine_nodes_to_recalc(struct cgraph *cpy, struct cgraph *cg,
                                                      struct gesrec *gesrecp,
                                                      int *visited,
                                                      int n_visited,
                                                      int *n_nodes);
#endif

#ifndef INT_RBT_H
#define INT_RBT_H

typedef struct int_rbt_node int_rbt_node;
typedef int_rbt_node* int_rbt_node_ptr;

int_rbt_node_ptr int_rbt_insert(int_rbt_node_ptr root, const int key, const int n,
                                const int * const values);

void int_rbt_print_tree(int_rbt_node_ptr root, const int n);
#endif
#ifndef _INT_RBT_
#define _INT_RBT_

typedef struct int_rbt_node int_rbt_node;
typedef int_rbt_node* int_rbt_ptr;

int_rbt_ptr int_rbt_insert(int_rbt_ptr root, const int key,
                                const int n, const int * const values);
void int_rbt_print_tree(int_rbt_ptr root, const int n);
void int_rbt_free(int_rbt_ptr root);
int_rbt_ptr int_rbt_merge_trees(int_rbt_ptr dst, int_rbt_ptr src,
                          const int n);
int int_rbt_size(int_rbt_ptr root);
int int_rbt_key(int_rbt_ptr root);
int* int_rbt_values_ptr(int_rbt_ptr root);
int_rbt_ptr int_rbt_left_child(int_rbt_ptr root);
int_rbt_ptr int_rbt_right_child(int_rbt_ptr root);

#endif
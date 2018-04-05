#ifndef IRBT_H
#define IRBT_H

typedef struct irbt_node irbt_node;
typedef irbt_node* irbt_ptr;

irbt_ptr irbt_insert(irbt_ptr root, int key, int n, const int * const values);
void irbt_print_tree(irbt_ptr root, const int n);
void irbt_free(irbt_ptr root);
irbt_ptr irbt_merge_trees(irbt_ptr dst, irbt_ptr src, const int n);
int irbt_size(irbt_ptr root);
int irbt_key(irbt_ptr root);
int* irbt_values_ptr(irbt_ptr root);
irbt_ptr irbt_left_child(irbt_ptr root);
irbt_ptr irbt_right_child(irbt_ptr root);
irbt_ptr* make_ptr_to_irbt(const int n);
#endif
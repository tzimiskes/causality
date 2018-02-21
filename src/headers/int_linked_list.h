#ifndef INT_LL_H
#define INT_LL_H

typedef struct int_ll* int_ll_ptr;
typedef struct int_ll int_ll;
int_ll_ptr int_ll_insert(int_ll_ptr root, int key, int value);
int_ll_ptr int_ll_insert_by_value(int_ll_ptr root, int key, int value);
int_ll_ptr int_ll_delete(int_ll_ptr root, int key);
int_ll_ptr int_ll_next(int_ll_ptr root);
int int_ll_key(int_ll_ptr root);
int int_ll_value(int_ll_ptr root);
void int_ll_set_value(int_ll_ptr root, int value);
void int_ll_free(int_ll_ptr root);
int int_ll_size(int_ll_ptr root);

#endif
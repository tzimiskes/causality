#ifndef _INT_LL_
#define _INT_LL_

typedef struct int_ll* int_ll_ptr;
typedef struct int_ll int_ll;
int_ll_ptr* make_int_ll_hash_table(const int n);
int_ll_ptr int_ll_insert(int_ll_ptr root, int key, int value);
void ill_insert(int_ll_ptr* root, int key, int value,
                       int* index, int_ll_ptr nodes);
int_ll_ptr int_ll_insert_by_value(int_ll_ptr root, int key, int value);
int_ll_ptr int_ll_delete(int_ll_ptr root, int key);
int_ll_ptr int_ll_next(int_ll_ptr root);
int_ll_ptr int_ll_make_nodes(const int n);
int_ll_ptr create_ill_ptr(const int n);

void       int_ll_print(int_ll_ptr root);
void       int_ll_set_value(int_ll_ptr root, int value);
void       int_ll_free(int_ll_ptr root);

int        int_ll_key(int_ll_ptr root);
int        int_ll_value(int_ll_ptr root);
int        int_ll_size(int_ll_ptr root);
#endif

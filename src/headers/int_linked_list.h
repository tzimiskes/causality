#ifndef _ILL_H
#define _ILL_H

typedef struct ill* ill_ptr;
// definition of each node in a linked list
typedef struct ill {
  ill_ptr next;
  int key;
  int value;
} ill;

ill_ptr* create_ptr_to_ill_ptr(const int n);
ill_ptr  create_ill_ptr(const int n);

void ill_free(ill_ptr root);

// insertion functions
ill_ptr ill_insert(ill_ptr root, int key, int value);
void ill_insert2(ill_ptr* root, int key, int value, int i, ill_ptr nodes);
void ill_insert_by_value(ill_ptr* root, int key, int value, int i, ill_ptr nodes);

ill_ptr ill_search(ill_ptr root, const int key);
void ill_set_next(ill_ptr root, ill_ptr next);
ill_ptr ill_next(ill_ptr root);

int        ill_key(ill_ptr root);
void       ill_set_value(ill_ptr root, int value);
int        ill_value(ill_ptr root);

void       ill_print(ill_ptr root);
#endif

#ifndef _ILL_H
#define _ILL_H

typedef struct ill* ill_ptr;
// definition of each node in a linked list
typedef struct ill {
  ill_ptr next;
  int key;
  int value;
} ill;

struct ill ** create_ptr_to_ill_ptr(const int n);
struct ill *  create_ill_ptr(const int n);
void ill_free(ill_ptr root);
// insertion functions
struct ill * ill_insert(ill_ptr root, int key, int value);
struct ill * ill_insert_front(ill_ptr root, int key, int value);
struct ill * copy_ill(ill_ptr root);

struct ill *  ill_search(ill_ptr root, const int key);
void          ill_set_next(ill_ptr root, ill_ptr next);
struct ill *  ill_next(ill_ptr root);
void          ill_set_key(ill_ptr root, int new_key);
int           ill_key(ill_ptr root);
void          ill_set_value(ill_ptr root, int value);
int           ill_value(ill_ptr root);
void          ill_delete(ill_ptr* root, int key);
void          ill_print(ill_ptr root);
int           ill_size(ill_ptr root);
#endif

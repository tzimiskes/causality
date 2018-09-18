#ifndef _ILL_H
#define _ILL_H

typedef struct ill* ill_ptr;
// definition of each node in a linked list
struct ill {
  struct ill *next;
  int key;
  int value;
};

struct ill ** create_ptr_to_ill_ptr(int n);
struct ill  * create_ill_ptr(int n);
void          ill_free(struct ill *root);
struct ill  * ill_insert(struct ill *root, int key, int value);
struct ill  * ill_insert_front(struct ill *root, int key, int value);
struct ill  * copy_ill(struct ill *root);
struct ill  * ill_search(struct ill *root, int key);
void          ill_delete(ill_ptr* root, int key);
void          ill_print(struct ill *root);
int           ill_size(struct ill *root);
#endif

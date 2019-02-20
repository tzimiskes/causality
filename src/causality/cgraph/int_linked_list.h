#ifndef ILL_H
#define ILL_H

/* definition of each node in a linked list */
struct ill {
    int         node;
    short       edge;
    short       tag;
    struct ill *next;
};

struct ill ** create_ptr_to_ill_ptr(int n);
struct ill  * create_ill_ptr(int n);
void          ill_free(struct ill *root);
void insert_ell(struct ill **root, int key, int value);
struct ill  * ill_insert_front(struct ill *root, int key, int value);
struct ill  * copy_ill(struct ill *root);
struct ill  * ill_search(struct ill *root, int key);
void          ill_delete(struct ill **root, int key);
void          ill_print(struct ill *root);
int           ill_size(struct ill *root);
#endif

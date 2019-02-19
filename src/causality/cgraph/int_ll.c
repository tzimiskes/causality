#include <stdlib.h>

#include <causality.h>
#include <cgraph/int_linked_list.h>

static struct ill * ill_instantiate(int key, int value)
{
    struct ill *p = malloc(sizeof(struct ill));
    if (p == NULL)
        CAUSALITY_ERROR("Failed to instaniate linked list!\n");
    p->node  =  key;
    p->edge = value;
    p->tag  = 0;
    p->next = NULL;
    return p;
}

struct ill * ill_insert(struct ill *root, int key, int value)
{
    if (root == NULL)
        return(ill_instantiate(key, value));
    else {
        struct ill *p = root;
        while (p->next)
            p = p->next;
        p->next = ill_instantiate(key, value);
        return(root);
    }
}

struct ill * ill_insert_front(struct ill *root, int key, int value)
{
    struct ill *tmp = ill_instantiate(key, value);
    if (!root)
        return tmp;
    else {
        tmp->next = root;
        return tmp;
    }
}


struct ill * copy_ill(struct ill *root)
{
    struct ill *copy = NULL;
    while (root) {
        copy = ill_insert(copy, root->node, root->edge);
        root = root->next;
    }
    return copy;
}

void ill_free(struct ill *root)
{
    while (root != NULL) {
        struct ill *next = root->next;
        free(root);
        root = next;
    }
}

struct ill * ill_search(struct ill *root, int key) {
    while (root) {
        if (root->node == key)
            return root;
        root = root->next;
    }
    return NULL; /* root is NULL */
}

struct ill ** create_ptr_to_ill_ptr(int n)
{
    struct ill ** array = malloc(n * sizeof(struct ill *));
    if (array == NULL)
        CAUSALITY_ERROR("Failed to allocate pointer for ill_ptr*\n");
    for(int i = 0; i < n; ++i)
        array[i] = NULL;
    return(array);
}

struct ill * create_ill_ptr(int n)
{
    struct ill *p = calloc(n, sizeof(struct ill));
    if (p == NULL)
        CAUSALITY_ERROR("Failed to allocate pointer for ill_ptr\n");
    return(p);
}

void ill_print(struct ill *root)
{
    while (root) {
        CAUSALITY_PRINT("Key: %i Value: %i\n", root->node, root->edge);
        root = root->next;
    }
}

int ill_size(struct ill *root)
{
    int n = 0;
    while (root) {
        n++;
        root = root->next;
    }
    return n;
}

void ill_delete(struct ill ** root, int key)
{
    struct ill *p = *root;
    if (p->node == key) {
        *root = (*root)->next;
        free(p);
        return;
    }
    while (p->next) {
        if (p->next->node == key) {
            struct ill *p2 = p->next;
            p->next    = p->next->next;
            free(p2);
            return;
        }
        p = p->next;
    }
}

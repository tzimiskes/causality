#include <stdlib.h>

#include <causality.h>
#include <cgraph/edge_list.h>

static struct edge_list * instantiate(int node, short edge, int tag)
{
    struct edge_list *e = malloc(sizeof(struct edge_list));
    if (e == NULL)
        CAUSALITY_ERROR("Failed to instaniate linked list!\n");
    e->node = node;
    e->edge = edge;
    e->tag  = tag;
    e->next = NULL;
    return e;
}

void insert_edge_list(struct edge_list **root, int node, short edge, short tag)
{
    struct edge_list *e = instantiate(node, edge, tag);
    e->next = *root;
    *root = e;
}

struct edge_list * copy_edge_list(struct edge_list *root)
{
    struct edge_list *copy = NULL;
    while (root) {
        insert_edge_list(&copy, root->node, root->edge, root->tag);
        root = root->next;
    }
    return copy;
}

void free_edge_list(struct edge_list *root)
{
    while (root != NULL) {
        struct edge_list *next = root->next;
        free(root);
        root = next;
    }
}

struct edge_list * search_edge_list(struct edge_list *root, int node)
{
    while (root) {
        if (root->node == node)
            return root;
        root = root->next;
    }
    CAUSALITY_ERROR("Cannot find edge in search_edge_list, returning NULL\n");
    return NULL; /* root is NULL */
}

void print_edge_list(struct edge_list *root)
{
    while (root) {
        CAUSALITY_PRINT("Key: %i Value: %i\n", root->node, root->edge);
        root = root->next;
    }
}

int size_edge_list(struct edge_list *root)
{
    int n = 0;
    while (root) {
        n++;
        root = root->next;
    }
    return n;
}

void delete_edge_list(struct edge_list **root, int node)
{
    struct edge_list *prev = *root;
    struct edge_list *e = (*root)->next;
    if ((*root)->node == node) {
        free(*root);
        *root = e;
        return;
    }
    while (e) {
        if (e->node == node) {
            prev->next = e->next;
            free(e);
            return;
        }
        prev = e;
        e = e->next;
    }
}

int identical_edge_lists(struct edge_list *e1, struct edge_list *e2)
{
    struct edge_list *t1 = e1;
    struct edge_list *t2 = e2;
    while (t1) {
        t2 = e2;
        while (t2) {
            if(t2->node == t1->node)
                goto T2_NEXT;
            t2 = t2->next;
        }
        return 0;
        T2_NEXT:
        t1 = t1->next;
    }
    t1 = e1;
    t2 = e2;
    while (t2) {
        t1 = e1;
        while (t1) {
            if(t1->node == t2->node)
                goto T1_NEXT;
            t1 = t1->next;
        }
        return 0;
        T1_NEXT:
        t2 = t2->next;
    }
    return 1;
}

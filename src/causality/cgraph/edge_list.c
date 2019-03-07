/* Author: Alexander Rix
 * Date  : 3/6/2019
 * Description: edge_list.c implements the edge_list structure, a core part of
 * cgraphs and the causality library in general. edge_lists are implemented as
 * listed lists, because in general we don't expect edge_lists to hold more
 * than 10 elements
 */

#include <stdlib.h>

#include <causality.h>
#include <cgraph/edge_list.h>

/*
 * instantiate a new edge to be inserted into the edge list. by default edges
 * are untagged. edge corresponds to one of the edge defined in causality.h
 */
static struct edge_list * instantiate(int node, short edge, short tag)
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

/* insert_edge adds an edge to the edge list by adding in a stack like manner */
void insert_edge(struct edge_list **root, int node, short edge, short tag)
{
    struct edge_list *e = instantiate(node, edge, tag);
    e->next = *root;
    *root = e;
}

/*
 * copy_edge_list returns a deep copy of the input. Technically the copy
 * reversed, but edge_lists are unordered so this isn't a problem.
 */
struct edge_list * copy_edge_list(struct edge_list *root)
{
    struct edge_list *copy = NULL;
    while (root) {
        insert_edge(&copy, root->node, root->edge, root->tag);
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
    return NULL;
}

void print_edge_list(struct edge_list *root)
{
    while (root) {
        CAUSALITY_PRINT("node: %i edge: %i\n", root->node, root->edge);
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

void remove_edge(struct edge_list **root, int node)
{
    struct edge_list *prev = *root;
    struct edge_list *e    = (*root)->next;
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

/*
 * identical_edge_lists tests whether or not two edge lists are identical.
 * O(n(e1)^2) since edge_lists are unsorted
 */
int identical_edge_lists(struct edge_list *e1, struct edge_list *e2)
{
    if (size_edge_list(e1) != size_edge_list(e2))
        return 0;
    /* if e1 and e2 are the same size we only need to  check e1 \subset e2 */
    while (e1) {
        struct edge_list *t = e2;
        while (t) {
            if( t->node == e1->node)
                goto NEXT;
            t = t->next;
        }
        return 0;
        NEXT:
        e1 = e1->next;
    }
    return 1;
}

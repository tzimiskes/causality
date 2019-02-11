#include <stdlib.h>

#include <causality.h>
#include <aggregate/tree.h>

#define BLACK 1
#define RED 0

static void single_rotation(struct tree **root, int dir)
{
    struct tree *t  = (*root)->children[!dir];
    (*root)->children[!dir] = t->children[dir];
    t->children[dir]        = *root;
    (*root)->color = RED;
    t->color = BLACK;
    *root = t;
}

static void double_rotation(struct tree **root, int dir)
{
    single_rotation(&(*root)->children[!dir], !dir);
    single_rotation(root, dir);
}

static void insert_recursive(struct tree **root, int node, int edge,
                                                     double weight)
{
    if (*root == NULL) {
        *root = malloc(sizeof(struct tree));
        if (*root == NULL)
            CAUSALITY_ERROR("failed to allocate memory for root\n");
        (*root)->node = node;
        (*root)->color = RED;
        (*root)->children[LEFT]  = NULL;
        (*root)->children[RIGHT] = NULL;
        (*root)->edges = calloc(NUM_CAG_EDGETYPES, sizeof(double));
        (*root)->edges[edge] = weight;
    }
    else if ((*root)->node == node) {
        (*root)->edges[edge] += weight;
    }
    else {
        int dir = node < (*root)->node;
        insert_recursive(&(*root)->children[dir], node, edge, weight);
        if ((*root)->children[dir]->color == RED) {
            if ((*root)->children[!dir] && (*root)->children[!dir]->color == RED) {
                (*root)->color = RED;
                (*root)->children[LEFT]->color = BLACK;
                (*root)->children[RIGHT]->color = BLACK;
            }
            else {
                if ((*root)->children[dir]->children[dir] && (*root)->children[dir]->children[dir]->color == RED)
                    single_rotation(root, !dir);
                else if ((*root)->children[dir]->children[!dir])
                    double_rotation(root, !dir);
            }
        }
    }
}

void insert_tree(struct tree **root, int node, int edge, double weight)
{
    insert_recursive(root, node, edge, weight);
    (*root)->color = BLACK;
}

int tree_size(struct tree *root)
{
    if (root != NULL)
        return 1 + tree_size(root->children[LEFT]) +
            tree_size(root->children[RIGHT]);
    else
        return 0;
}

void free_tree(struct tree *root)
{
    if (root != NULL) {
        free_tree(root->children[LEFT]);
        free_tree(root->children[RIGHT]);
        free(root->edges);
        free(root);
    }
}

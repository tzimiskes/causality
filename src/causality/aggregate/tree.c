/* Author: Alexander Rix
 * Date  : 2/19/2019
 * Description: tree.c contains a (partial -- no function to delete a node)
 * implementation of a red black tree inspired by Eternally Confuzzled. It is
 * used exclusively by aggregate_graphs.c
 */

#include <setjmp.h> /* for error handling */
#include <stdlib.h>

#include <causality.h>
#include <aggregate/tree.h>

#define BLACK 1
#define RED 0

/*
 * If there's a problem in insert_recursive, we need to long jump to unwind the
 * stack and to free the tree properly
 */
static jmp_buf FAIL_STATE;

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
        /* Malloc failed. long jump to unwind stack and deallocate tree */
        if (*root == NULL)
            longjmp(FAIL_STATE, 1);
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
            struct tree *t = (*root)->children[!dir];
            if (t && t->color == RED) {
                (*root)->color = RED;
                (*root)->children[LEFT]->color = BLACK;
                (*root)->children[RIGHT]->color = BLACK;
            }
            else {
                struct tree *t = (*root)->children[dir]->children[dir];
                if (t && t->color == RED)
                    single_rotation(root, !dir);
                else if ((*root)->children[dir]->children[!dir]) {
                    double_rotation(root, !dir);
                }
            }
        }
    }
}

void insert_tree(struct tree **root, int node, int edge, double weight)
{
    if (!setjmp(FAIL_STATE)) {
        insert_recursive(root, node, edge, weight);
        (*root)->color = BLACK;
    }
    /* If we get here, malloc failed */
    else {
        CAUSALITY_ERROR("Failed to malloc memory in tree.\n");
        free_tree(*root);
        *root = NULL;
    }
}

int tree_size(struct tree *root)
{
    if (root == NULL)
        return 0;
    else
        return 1 + tree_size(root->children[LEFT]) +
                   tree_size(root->children[RIGHT]);
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

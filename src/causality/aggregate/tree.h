#ifndef CAUSALITY_TREE_H
#define CAUSALITY_TREE_H

#define LEFT 1
#define RIGHT 0

struct tree {
    struct tree *children[2];
    double *edges;
    int node;
    int color;
};

void insert_tree(struct tree **root, int node, int edge, double weight);
int tree_size(struct tree *root);
void free_tree(struct tree *root);


static inline struct tree * left_child(struct tree *root)
{
    return root->children[LEFT];
}

static inline struct tree * right_child(struct tree *root)
{
    return root->children[RIGHT];
}

static inline int tree_node(struct tree *root)
{
    return root->node;
}

static inline double * tree_edges(struct tree *root)
{
    return root->edges;
}

#endif

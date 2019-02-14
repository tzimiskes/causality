/* Author : Alexander Rix
 * Date   : 11/30/18
 * Description:
 * sort.c implements a function to perform a topological sort on a graph.
 * ccf_sort returns a NULL pointer, or a pointer to an integer containing the
 * ordering of the nodes of the graph.
 */

#include <setjmp.h> /* for error handling */
#include <stdlib.h>

#include <causality.h>
#include <cgraph/cgraph.h>
#include <cgraph/int_linked_list.h>

#define UNMARKED   0
#define MARKED     1
#define TEMPORARY -1

static void visit(int node, int *marked, int *stack_index,
                            struct ill **children, int *sort);

/*
 * In order to account for the possibility that the input may contain a cycle,
 * we need to use longjmp/setjmp to implement error handling that will
 * unwind the call stack created by visit() in a safe way.
 */
static jmp_buf FAIL_STATE;

/*
 * ccf_sort implements a topological sort by using a breadth first search as
 * described in CLRS. This function takes in a C level representation
 * (currently an integer linked list) of the edge list of an causality.graph,
 * and returns a pointer to the sorted (C level representation) of the nodes
 */
int * causality_sort(struct cgraph *cg)
{
    int n_nodes = cg->n_nodes;
    /* create an array to signify whether or not a node has been marked, */
    int *marked  = calloc(n_nodes, sizeof(int));
    if (marked == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory for marked in ccf_sort\n");
        return NULL;
    }

    int *sort = malloc(n_nodes * sizeof(int));
    if (sort == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory for sort in ccf_sort\n");
        free(marked);
        return NULL;
    }
    /*
     * setjmp(FAIL_STATE) == 0 the first time this is called. longjmping here
     * from visit will have setjmp(FAIL_STATE) == 1
     */
    if (!setjmp(FAIL_STATE)) {
        /*
         * Pick an unmarked node and do a breadth first search on it. If
         * the node is marked, go to the next node and try again
         */
        struct ill **children    = cg->children;
        int          stack_index = n_nodes;
        for (int i = 0; i < n_nodes; ++i) {
            if (!marked[i])
                visit(i, marked, &stack_index, children, sort);
        }
    }
    else {
        /*
         * FAIL_STATE: Graph contains a cycle, so visiting activated a longjmp
         * here, unwinding the call stack. Now, we can deallocate the memory
         * allocated to sort and then set sort to NULL. (NULL implies that a
         * valid topological sort of the input doesn't exist).
         */
        free(sort);
        sort = NULL;
    }
    free(marked);
    return sort;
}

/*
 * visit: recursively look through the children of the node, looking for cycles
 * if a cycle is found, a longjmp is performed back to ccf_sort, where cleanup
 * occurs. Otherwise, once it has been determined that there are no cylces, the
 * node is permentantly marked, the node is pushed onto the topological sort,
 * stack_index is decremented, and the function terminates.
 */
static void visit(int node, int *marked, int *stack_index,
                            struct ill **children, int *sort)
{
    /* Cycle exists; perform longjmp to so we can terminate the sort */
    if (marked[node] == TEMPORARY)
        longjmp(FAIL_STATE, 1);
    else if (marked[node] == UNMARKED) {
        marked[node] = TEMPORARY;
        struct ill *child = children[node];
        while (child != NULL) {
            visit(child->node, marked, stack_index, children, sort);
            child = child->next;
        }
        marked[node]       = MARKED;
        *stack_index      -= 1;
        sort[*stack_index] = node;
    }
}

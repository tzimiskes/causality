#include<R.h>
#include<Rinternals.h>
#include<stdlib.h>
// this implementation of a stack uses an array as the underlying data structure
// we know the max size of the stack, we should be good
typedef struct int_a_stack {
  int* stack;
  int n;
  int top;
} int_a_stack;

typedef int_a_stack* int_a_stack_ptr;


int_a_stack_ptr int_a_stack_instantiate(int n) {
  int_a_stack_ptr stack_ptr =  malloc(sizeof(int_a_stack));
  if (stack_ptr == NULL)
    error("Failed to allocate pointer for stack_ptr");
stack_ptr->n = n;
stack_ptr->top = n;
stack_ptr->stack = malloc(n*sizeof(int));
if(stack_ptr->stack == NULL)
  error("Failed to allocate pointer for stack");
  return(stack_ptr);
}


void int_a_stack_push(int_a_stack_ptr stack_ptr, int foo) {
  if (stack_ptr->top > 0) {
    stack_ptr->top -= 1;
    stack_ptr->stack[stack_ptr->top] = foo;
  }
  else
    error("attempt to push element on a full stack");
}

int int_a_stack_peek(int_a_stack_ptr stack_ptr) {
  int top = stack_ptr->top;
  if (top < stack_ptr->n)
    return(stack_ptr->stack[top]);
  else
    // stack is empty
    return(-1);
}

void int_a_stack_pop(int_a_stack_ptr stack_ptr) {
  if(stack_ptr->top < stack_ptr->n)
    (stack_ptr->top)++;
  else
    error("attempt to pop an empty stack");
}

int* int_a_stack_get_stack(int_a_stack_ptr stack_ptr) {
  return(stack_ptr->stack);
}

void int_a_stack_free(int_a_stack_ptr stack_ptr) {
  free(stack_ptr->stack);
}
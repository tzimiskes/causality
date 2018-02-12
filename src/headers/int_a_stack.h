#ifndef INT_STACK_H
#define INT_STACK_H

typedef struct int_a_stack int_a_stack;
typedef int_a_stack* int_a_stack_ptr;

int_a_stack_ptr int_a_stack_instantiate(int n);
void int_a_stack_push(int_a_stack_ptr stack_ptr, int foo);
int int_a_stack_peek(int_a_stack_ptr stack_ptr);
void int_a_stack_pop(int_a_stack_ptr stack_ptr);
int* int_a_stack_get_stack(int_a_stack_ptr stack_ptr);

#endif
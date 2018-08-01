#include"headers/causality.h"
#include"headers/int_linked_list.h"


static inline ill_ptr ill_instantiate(int key, int value) {
  ill_ptr tmp = malloc(sizeof(ill));
  if(tmp == NULL)
    error("Failed to instaniate linked list!\n");
  tmp->key =  key;
  tmp->value = value;
  tmp->next = NULL;
  return(tmp);
}

ill_ptr ill_insert(ill_ptr root, int key, int value) {
  if(root == NULL)
    return(ill_instantiate(key, value));
  else {
    ill_ptr tmp = root;
    while(tmp->next != NULL)
      tmp = tmp->next;
    tmp->next = ill_instantiate(key, value);
    return(root);
  }
}

void ill_insert2(ill_ptr* root, int key, int value, int i, ill_ptr nodes) {
    nodes[i].next = *root;
    nodes[i].key   = key;
    nodes[i].value = value;
    *root          = &nodes[i];
}

/*
 * TODO
 */
void ill_insert_by_value(ill_ptr* root, int key, int value,
                          int i, ill_ptr nodes)
{
  nodes[i].key   = key;
  nodes[i].value = value;
  // if root is null, instantiate
  if(*root == NULL) {
    *root          = &nodes[i];
    return;
  }
  if((*root)->value < value) {
    nodes[i].next = *root;
    *root          = &nodes[i];
    return;
  }
  // else loop through the nodes
  ill_ptr tmp = *root;
  while(tmp->next != NULL) {
    if(tmp->next->value < value) {
      nodes[i].next = tmp->next;
      tmp->next     = &nodes[i];
      return;
    }
    tmp = tmp->next;
  }
  // if next is NULL instantiate it
  tmp->next = &nodes[i];
}

void ill_set_next(ill_ptr root, ill_ptr next) {
  root->next = next;
}

ill_ptr ill_next(ill_ptr root) {
  return(root->next);
}

int ill_key(ill_ptr root) {
  return(root->key);
}

int ill_value(ill_ptr root) {
  return(root->value);
}

void ill_free(ill_ptr root) {
  while(root != NULL) {
    ill_ptr next = root->next;
    free(root);
    root = next;
  }
}

void ill_set_key(ill_ptr root, int new_key) {
  if(root != NULL)
    root->key = new_key;
  else
    error("Cannot assign key to a NULL pointer!\n");

}

void ill_set_value(ill_ptr root, int new_value) {
  if(root != NULL)
    root->value = new_value;
  else
    error("Cannot assign value to a NULL pointer!\n");
}

ill_ptr ill_search(ill_ptr root, const int key) {
  while(root != NULL) {
    if(root-> key == key)
      return root;
    else
      root = root->next;
  }
  return root; /* root is NULL */
}

ill_ptr* create_ptr_to_ill_ptr(const int n) {
  ill_ptr* hash_table = malloc(n*sizeof(ill_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for ill_ptr*\n");
  for(int i = 0; i < n; ++i)
    hash_table[i] = NULL;
  return(hash_table);
}

ill_ptr create_ill_ptr(const int n) {
  ill_ptr ptr = calloc(n, sizeof(ill));
  if(ptr == NULL)
    error("Failed to allocate pointer for ill_ptr\n");
  return(ptr);
}

void ill_print(ill_ptr root) {
  while(root != NULL) {
    Rprintf("Key: %i Value: %i\n", root->key, root->value);
    root = root->next;
  }
}
/* DO NOT USE THIS IN CONJUCTION WITH CMPCT_CG !!!!!! */
void ill_delete(ill_ptr* root, int key) {
  ill_ptr tmp = *root; /* should probably check to see if this is not null */
  if(tmp != NULL) {
    if(tmp->key == key) {
      *root = (*root)->next;
      free(tmp);
      return;
    }
    while (tmp->next != NULL) {
      if(tmp->next->key == key) {
        ill_ptr tmp2 = tmp->next;
        tmp->next = tmp2->next;
        free(tmp2);
        return;
      }
      else
        tmp = tmp->next;
    }
  }
  error("Failed to find key in ill_delete!\n");
}

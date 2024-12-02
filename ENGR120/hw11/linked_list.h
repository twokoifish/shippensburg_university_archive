/* A recursive structure */
struct node {
  int number; /* payload */
  struct node *next;
};

/* You can use "node_t" instead of "struct node" */
typedef struct node node_t;

/* Make a single node with c as payload */
extern node_t *make_node(int c);

/* Make a list of intacters with a string input and size */
extern node_t *make_list(int numbers[], int size);

/* Print the list. See an example output in the test sheet. */
extern void print_list(node_t *head);

/* Return number of nodes */
extern int size(node_t *head); 

/* Add a node with c as payload before the current head. Return the new head. */
extern node_t *prepend(node_t *head, int c); 

/* Add a node at the end of list with payload c. */
extern node_t *append(node_t *head, int c); 

/* Prints out the payload of a node in a single line */
extern void print_node(node_t n);

/* Return a pointer to the ith element (0-indexed) */
extern node_t *get_ith(node_t *head, int pos);

/* Make and insert a node after the ith node (0-indexed) */
extern node_t *insert_at(node_t *head, int c, int pos);

/* Make and insert a node after a node */
extern void insert_after(node_t *node, int c);

/* Remove a node at the ith position (0-indexed) and free the space of the node */
extern node_t *remove_ith(node_t *head, int i);

/* Free the space of the nodes and set the head to NULL. */
extern void delete_list(node_t **head);

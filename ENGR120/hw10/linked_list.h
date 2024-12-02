/* A recursive structure */
struct node {
  char letter; /* payload */
  struct node *next;
};

typedef struct node node_t;

/* Make a single node with c as payload */
node_t *make_node(char c);

/* Make a list of characters with a string input and size */
node_t *make_list(char letters[], int size);

/* Print the list. See an example output in the test sheet. */
void print_list(node_t *head);

/* Return number of nodes */
int size(node_t *head);

/* Add a node with c as payload before the current head. Return the new head. */
node_t *prepend(node_t *head, char c);

/* Add a node at the end of list with payload c. */
node_t *append(node_t *head, char c);

/* Prints out the payload of a node in a single line */
void print_node(node_t n);

/* Return a pointer to the ith element (0-indexed) */
node_t *get_ith(node_t *head, int pos);

/* Make and insert a node after the ith node (0-indexed) */
node_t *insert_at(node_t *head, char c, int pos);

/* Make and insert a node after a node */
void insert_after(node_t *node, char c);

/* Remove a node at the ith position (0-indexed) and free the space of the node */
node_t *remove_ith(node_t *head, int i);

/* Free the space of the nodes and set the head to NULL. */
void delete_list(node_t **head);

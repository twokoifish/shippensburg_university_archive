#define MAX 100

typedef struct {
  char arr[MAX];
  int size;
} array_list_t;

array_list_t *make_list(char letters[], int n);

void append(array_list_t *lst, char c);

void prepend(array_list_t *lst, char c);

void insert_at(array_list_t *lst, char c, int pos);

int size(array_list_t *lst);

void remove_ith(array_list_t *lst, int pos);

void delete_list(array_list_t *lst);

void print_list(array_list_t *lst);

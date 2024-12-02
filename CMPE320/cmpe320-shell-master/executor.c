#include "executor.h"

int list_size(struct list_head *list) {

    int size = 0;
    for (struct list_head * tok = list->next; tok != list; tok = tok->next)
    {
        // struct node *l_entry = list_entry(tok, struct node, list);
        size++;
    }
    return size;
}


int get_nodes_in_command(struct list_head *token_list, int start_node) {

    int size = 0;
    int i = 0;
    for (struct list_head * tok = token_list->next; tok != token_list; tok = tok->next)
    {
        struct cmd *l_entry = list_entry(tok, struct cmd, list);
        
        if (i >= start_node) {
            if (l_entry->state == NULL_TERM) break;
            size++;
        } 
        i++;
    }
    
    return size;
}

char** change_list_to_array(struct list_head *token_list, int start_node, int command_line_size) {


    // struct list_head *curr;
    // // struct list_head *t_list = &(command->token_list);

    char** array_of_tokens = calloc(sizeof(char *), command_line_size);

    int i = 0;
    int j = 0;
    for (struct list_head *tok = token_list->next; /*tok != token_list;*/ i < start_node + command_line_size; tok = tok->next)
    {
        struct cmd *l_entry = list_entry(tok, struct cmd, list);
        if (i >= start_node) {
            array_of_tokens[j] = l_entry->word;
            j++;
        }
        i++;
    }

    return array_of_tokens;
}

void execute_single_command(char **command_nodes_array, char **envp, int *status) {

    pid_t pid = fork();

    if (pid == 0)
    {
        char* command_name = command_nodes_array[0];//might not be. open file if redir

        *status = execvpe(command_name, command_nodes_array, envp);
    } else {
        waitpid(pid, status, 0);
    }
    
    

    //print error_msgs.h  based off status code ????????????
    // exit(-1);
}

int move_start_position_and_check_pipe(struct list_head *token_list, int start_position, int command_line_length, int *pipe_next) {
    //TODO: move start position past redirect 
    //printf("start position: %d\n", start_position);


    int i = 0;
    for (struct list_head *tok = token_list->next; tok != token_list; tok = tok->next)
    {
        struct cmd *l_entry = list_entry(tok, struct cmd, list);

        if (i <= start_position) {
            i++;
            if (l_entry->state == NULL_TERM) { //wont work with redirs
                break;
            }
        }
    }


    int j = 0;//separate other part out??
    int pipe = 0;
    for (struct list_head *tok = token_list->next; tok != token_list; tok = tok->next)
    {
        struct cmd *l_entry = list_entry(tok, struct cmd, list);
        //printf("w: %s\n", l_entry->word);

        if (j == (start_position + command_line_length) + 1) {
            //printf("word: %s\n", l_entry->word);
            if (l_entry->state == PIPE) {
                    *pipe_next = 1;
                    pipe = 1;
            }
        }
        j++;
    }


    return i + pipe;
}

void fork_and_execute(struct list_head *list_nodes, struct list_head sushenvp, char **command_nodes_array, char **envp, int new_start_position, int r_level) {
    int status;
    int pipes[2];   
    int rc = pipe(pipes);
    int pid = fork();
    if (pid == 0) 
    {
        close(STDOUT_FILENO); 
        // int e = dup2(STDOUT_FILENO, pipes[1]);
        int e = dup(pipes[1]);  
    
        execute_single_command(command_nodes_array, envp, &status);
        // char* test1[3];
        // test1[0] = "ps";
        // test1[1] = "-a";
        // test1[2] = NULL;
        waitpid(pid, &status, 0);

        // execvp("ps", test1);
    }
    else
    {
        close(STDIN_FILENO); 
        int e = dup(pipes[0]); 
        // int e = dup2(pipes[0], STDIN_FILENO);
        
        execute_commands(list_nodes, sushenvp, new_start_position, r_level+1, 0);

        waitpid(pid, &status, 0);

        // char* test[4];
        // test[0] = "head";
        // test[1] = "-n";
        // test[2] = "1";
        // test[3] = NULL;
        // execvp("head", test);

    }
}

void execute_commands(struct list_head *list_nodes, struct list_head sushenvp, int start_node, int recursive_level, int pipe) {//pipe may not be needed

    if (recursive_level == list_size(list_nodes)) return;

    int start_position = start_node;
    //get number of nodes until null
    int command_line_size = get_nodes_in_command(list_nodes, start_position);
    //printf("command line size: %d\n", command_line_size);

    //check for redirects and do appropriate action
    //TODO: start_position = set_up_redirection(list_nodes, start_position, command_line_size);

    //make into array from that amount
    char **command_nodes_array = change_list_to_array(list_nodes, start_position, command_line_size);

    int environ_var_size = env_list_count_entries(sushenvp);
    char *envp[environ_var_size + 1];
    env_convert_to_list(sushenvp, envp);

    int pipe_next = 0;
    start_position = move_start_position_and_check_pipe(list_nodes, start_position, command_line_size, &pipe_next);
    
    if (pipe_next == 1) 
    {
        fork_and_execute(list_nodes, sushenvp, command_nodes_array, envp, start_position, recursive_level);     
    } 
    else 
    {
        int status;

        if (handle_internal_command(list_nodes, &sushenvp) != 0) {

        } else {
            execute_single_command(command_nodes_array, envp, &status);
        }
        
        
    }

}

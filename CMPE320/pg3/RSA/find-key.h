#ifndef FIND_KEY_H
#define FIND_KEY_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <pthread.h>
#include <time.h>
#include <math.h>
#include <tgmath.h>
#include <stdint.h>
#include <gmp.h>
#include <stdbool.h>

// My RSA library - don't use for NSA work : Dr. Briggs
#include "rsa.h"

// Set the maximum number of characters
// the user can input (in bytes).
#define MAX_INPUT_SIZE 32

// Set the maximum number of characters 
// in a message (in bytes).
#define BLOCK_LEN 32

/**
 * @brief 
 * 
 */
typedef struct {
	int id;
	int found;
	int key_length;
	uint64_t start, end;
	char *encrypted;
	char *decrypted;
	int bytes;
	int inc;
	rsa_keys_t keys;
	bool foundKey;
} check_key_t;

 #include "prime_factorization.h"
//#include "brent-factorization.h"

/**
 * @brief Get an int response from the provided prompt.
 * 
 * @param prompt: the prompt to show the user.
 * @return int - the response from the user.
 */
int prompt_user_int(const char* prompt);


#endif // FIND_KEY_H
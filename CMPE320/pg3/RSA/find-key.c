/* ***********************************************
* Author: T. Briggs (c) 2019
* Date: 2019-02-25
* 
* Brute-force attach against an RSA key.
*
* Reads the public key files and iterates through
* all of the odd numbers from 3 to 2^key_len
************************************************ */ 

#include "find-key.h"

void print_fancy_title() {
	printf("****************\n");
	printf("*  KEY FINDER  *\n");
	printf("****************\n");
}

/**
 * @brief prompt the user for an integer.
 * 
 * @return int, the int provided by the user.
 */
int prompt_user_int(const char* prompt) {
	int int_buffer = 0;
	printf("%s", prompt);
	char buffer[MAX_INPUT_SIZE];
	fgets(buffer, MAX_INPUT_SIZE, stdin);
	int_buffer = atoi(buffer);
	return int_buffer;
}

/**
 * @brief Get the key length from the user.
 * 
 * @return int, the key length.
 */
int get_key_len() {
	int KEY_LEN = 0;
	do {
		KEY_LEN = prompt_user_int("Enter key length (bytes): ");
		if (KEY_LEN < 32) {
			printf("Invalid key size. Key must be >= 32.\n");
		}
		if (KEY_LEN % 2 != 0) {
			printf("Invalid key size. Key must be EVEN.\n");
		}
	} while (KEY_LEN < 32 || KEY_LEN % 2 != 0);
	return KEY_LEN;
}

/**
 * @brief Get the thread count from the user.
 * 
 * @return int, the thread count.
 */
int get_thread_count() {
	int THREAD_COUNT = 0;
	do {
		THREAD_COUNT = prompt_user_int("Enter thread count (int): ");
		if (THREAD_COUNT < 1) {
			printf("Invalid thread count. Count must be >= 1.\n");
		} 
	} while (THREAD_COUNT < 1);
	return THREAD_COUNT;
}

/**
 * @brief 
 * 
 * @param args 
 * @return void* 
 */
void *check_keys(void *args) {
	check_key_t *data = (check_key_t *) args;

	clock_t start_time = clock();
	uint64_t i = data->start;


	//uint64_t p = prime_factorization(mpz_get_ui(data->keys.n), data);

	mpz_t p;
	mpz_init(p);
	rho(p, data->keys.n);
	mpz_t q;
	mpz_init(q);
	mpz_div(q, data->keys.n, p);
	mpz_t phi_n;

	mpz_t p_inv;
	mpz_init(p_inv);
	mpz_sub_ui(p_inv, p, 1);

	mpz_t q_inv;
	mpz_init(q_inv);
	mpz_sub_ui(q_inv, q, 1);

	mpz_mul(phi_n, p_inv, q_inv);

	mpz_t phi;
	mpz_init_set(phi, phi_n);

	mpz_invert(data->keys.d, data->keys.e, phi);

	while (i < data->end) {
		rsa_decrypt(data->encrypted, data->decrypted, data->bytes, &data->keys);

		if (!strncmp(data->decrypted,"<h1>",4)) {
				clock_t end = clock();
				double time_spent = (((double) (end - start_time)) / CLOCKS_PER_SEC) / 10;
				printf("Thread %d found key: %lu %lx\nMessage: %s\nTook %lf s\n", data->id, mpz_get_ui(data->keys.d), mpz_get_ui(data->keys.d), data->decrypted, time_spent);
				return NULL;
		}
		i += (data->inc);
	}
	return NULL;
}

int main(int argc, char **argv)
{
	int KEY_LEN;
	int THREAD_COUNT;

	if (argc == 3) {
		KEY_LEN = atoi(argv[1]);
		THREAD_COUNT = atoi(argv[2]);
	} else {
		print_fancy_title();
		KEY_LEN = get_key_len();
		THREAD_COUNT = get_thread_count();
	}

	check_key_t args[THREAD_COUNT];
	pthread_t threads[THREAD_COUNT];
	pthread_attr_t attr[THREAD_COUNT];

	uint64_t base_size = ((1L << KEY_LEN) - 3);
	uint64_t chunk = (base_size + THREAD_COUNT - 1) / THREAD_COUNT;

	rsa_keys_t keys;

	char *encrypted = malloc(1024*2);

	char *fname = malloc(1024);
	sprintf(fname,"../intercepted_messages/encrypted-%d.dat", KEY_LEN); 
	FILE *fp = fopen(fname,"r+");
	if (fp == NULL) {
		perror("could not open encrypted text");
		exit(-1);
	}

	int bytes = fread(encrypted, 1, BLOCK_LEN*(KEY_LEN/8), fp);
	fclose(fp);

	int s = 3;
	for(int i = 0; i < THREAD_COUNT; i++) {
		int id = (i + 1);
		pthread_attr_init(&attr[i]);
		args[i].id = id;
		args[i].key_length = KEY_LEN;
		uint64_t start = s;
		args[i].foundKey = false;
		uint64_t end = base_size;
		args[i].inc = 2 * THREAD_COUNT;
		args[i].start = start;
		args[i].found = 0;
		args[i].end = end;
		char *fname = malloc(1024);
		sprintf(fname,"../intercepted_messages/public-%d.txt", KEY_LEN);
		rsa_read_public_keys(&args[i].keys, fname);
		args[i].encrypted = encrypted;
		args[i].decrypted = malloc(1024 * 2);
		args[i].bytes = bytes;
		free(fname);
		s+=2;
	}

	for (int i = 0; i < THREAD_COUNT; i++) {
		printf("Creating thread %d...\n", args[i].id);
		pthread_create(&threads[i], NULL, check_keys, (void *) &args[i]);
	}

	for (int i = 0; i < THREAD_COUNT; i++) {
		pthread_join(threads[i], NULL);
		printf("Joining thread %d...\n", args[i].id);
		free(args[i].decrypted);
	
		mpz_clear(args[i].keys.d);
		mpz_clear(args[i].keys.n);
		mpz_clear(args[i].keys.e);
		mpz_clear(args[i].keys.p);
		mpz_clear(args[i].keys.q);
	}

	
}

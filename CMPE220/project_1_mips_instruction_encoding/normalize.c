#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "normalize.h"

char *normalize(const char *string);

/**
 * \brief Normalize an instruction into one that has no extra spaces,
 * and converts the space between the instruction and the registers
 * into a comma.  The strig must be free()'d when you are done with it.
 *
 * Example 1:
 *    call : char *norm = normalize("     add   $t0,     $t2, $t3    ")  
 *    returns:  "add,$t0,$t2,$t3"
 *
 * Example 2:
 *    call : char *norm = normalize("  lw $t0,300($t1)")
 *    returns:  "lw,$t0,300($t1)"
 *
 * It does *NOT* attempt to verify that the instruction is valid...
 * It simply eats up extra spaces and reorganizes things into strings
 * separated by commas.  You should know how to handle that from 
 * ENGR120 or CSC111.
*/
#undef DEBUG
#define DBLEVEL 9

#if defined(DEBUG) && defined(DBLEVEL)
#define DBPRINTF(lvl,fmt,args...) \
	{ if (lvl<=DBLEVEL) fprintf(stderr,"%s:%d:%s(): " fmt, \
		__FILE__, __LINE__, __func__, ##args); }
#else
#define DBPRINTF(...) do { } while(0);
#endif

// states for a finite state machine, starting in WS1
enum norm_state { WS1, INSTR, WS2, REG };
	
char *normalize(const char *string)
{
	DBPRINTF(9,"trace: normalize(%s)\n", string);
	enum norm_state state = WS1;
	
	// the destination buffer to hold a copy of the input string
	char *copy = malloc(strlen(string+1));
	
	// make the copy all nulls....
	memset(copy, 0, strlen(string+1));
	
	DBPRINTF(8,"   allocated %ld bytes for copy %p\n", strlen(string), copy);
	
	// use two pointers to walk through the input and output strings
	char *dest = copy;
	const char *curr = string;
	
	
	// sentinnel loop looking for the end of the string
	while ((*curr != 0) && (*curr != '\n')) {
		DBPRINTF(6,"State: %d curr: (%c) last_dest: (%c)\n",
			state, *curr, (state != WS1) ? *(dest-1) : 0x00);
			
		switch(state) {
		// eat up the spaces (if any) at the beginning of the line
		case WS1:
			DBPRINTF(4,"    WS1 %c\n", *curr);
			if (isspace(*curr)) state = WS1;
			else {
				// dref the pointers for the assignment, use pointer
				// arithmetic to advnce the output pointer
				*dest++ = *curr;
				state = INSTR;
			}
			break;
			
		// copy the characters to the output until space
		case INSTR:
			DBPRINTF(4,"    INSTR %c\n", *curr);
			if (isspace(*curr)) {
				*dest++ = ',';
				state = WS2;
			}
			else {
				*dest++ = *curr;
			}
			break;
			
		// skip spaces between fields
		case WS2: 
			DBPRINTF(4,"    WS2 %c\n", *curr);
			if (isspace(*curr)) state = WS2;
			else {
				*dest++ = *curr;
				state = REG;
			}
			break;
		
		// copy the characters to the output until a space
		case REG: 
			DBPRINTF(4,"    REG %c\n", *curr);
			if (isspace(*curr)) state = WS2;
			else {
				*dest++ = *curr;
				state = REG;
			}
			break;
		default:
			DBPRINTF(0,"    default %c\n", *curr);
			return NULL;
		}
		
		// go to next input character
		curr++;
	}
	
	// the last character might be a ',' but it should be a null
	if (*dest == ',') *dest = 0x00;
	
	// remember dest is pointing into copy's memory....
	return copy;
}

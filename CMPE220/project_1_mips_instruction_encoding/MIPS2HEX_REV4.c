#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "normalize.c"
#define MAX_INSTRUCTIONS 100
#define MAX_LINE_SIZE 50
#define SENTINEL_VALUE "\n"
#define ERROR_INSTRUCTION "ERROR - INVALID INSTRUCTION\n"
#define ERROR_FORMAT "ERROR - INVALID FORMAT\n"
#define ERROR_REGISTER "ERROR - INVALID REGISTER\n"

// Define the structure for a _coreInstruction type.
// Type will either be:
// 'i-type'
// 'r-type'
// 'j-type'
// 'i-mem-type'
// 'r-mem-type'
// 'load-type'
// 'branch'
typedef struct _coreInstruction {
    char* instruction_name;
    char* instruction_type;
    int op_code;
    int function_code;
} core_instruction;

// All the functions we need for this program to work. Before refactor.
int fetchUserInput(char buffer[MAX_INSTRUCTIONS][MAX_LINE_SIZE]);
void determineType(char* instruction_line);
char* getType(char* possible_instruction);
char getFormatType(char* deconstructed);
int getRegister(char* input_register);
int getOperationCode(int index);
int getFunctionCode(int index);
int checkInstruction(char instruction[10]);
void convertI(char instruction_deconstructed[4][10]);
void convertR(char instruction_deconstructed[4][10]);
void convertJ(char instruction_deconstructed[4][10]);
void convertIMemory(char instruction_deconstructed[4][10]);
void convertRMemory(char instruction_deconstructed[4][10]);
void convertLoadImmediate(char instruction_deconstructed[4][10]);
void convertBranch(char instruction_deconstructed[4][10]);

int main(void) {
    char buffer[MAX_INSTRUCTIONS][MAX_LINE_SIZE];
    int numOfInstructions = fetchUserInput(buffer);
    for (int i = 0; i < numOfInstructions; i++) {
        determineType(buffer[i]);
    }
    return 0;
}

int fetchUserInput(char buffer[MAX_INSTRUCTIONS][MAX_LINE_SIZE]) {
    int instructions = 0;
    while (fgets(buffer[instructions], sizeof(buffer[instructions]), stdin) && instructions < MAX_INSTRUCTIONS - 1) {
        if (strcmp(buffer[instructions], SENTINEL_VALUE) == 0) {
            break;
        }
        for(int i = 0; i < MAX_LINE_SIZE; i++) {
            char character = buffer[instructions][i];
            character = tolower(character);
            buffer[instructions][i] = character;
        }
        strcpy(buffer[instructions], normalize(buffer[instructions]));
        char* token = strtok(buffer[instructions], SENTINEL_VALUE);
        strcpy(buffer[instructions], token);
        ++instructions;
    }
    return instructions;
}

void determineType(char* instruction_line) {
    char* copy = malloc(MAX_LINE_SIZE * sizeof(char));
    strcpy(copy, instruction_line);
    char* nospace = strtok(copy, ",");
    if(strcmp(nospace, instruction_line) == 0) {
            printf("%s", ERROR_FORMAT);
            return;
    }
    char deconstructed[4][10];
    int portion = 0;
    char *token = strtok(instruction_line, ",");
    while (token != NULL) {
        if (portion == 4) {
            printf("%s", ERROR_FORMAT);
            return;
        }
        strcpy(deconstructed[portion], token);
        ++portion;
        token = strtok(NULL, ",");
    }
    char* type = getType(deconstructed[0]);
    if (!strcmp(type, "r-type")) {
        convertR(deconstructed);
    } else if (!strcmp(type, "i-type")) {
        convertI(deconstructed);
    } else if (!strcmp(type, "j-type")) {
        convertJ(deconstructed);
    } else if (!strcmp(type, "i-mem-type")) {
        convertIMemory(deconstructed);
    } else if (!strcmp(type, "r-mem-type")) {
        convertRMemory(deconstructed);
    } else if (!strcmp(type, "load-type")) {
        convertLoadImmediate(deconstructed);
    } else if (!strcmp(type, "branch")) {
        convertBranch(deconstructed);
    } else {
        printf("%s", ERROR_INSTRUCTION);
    }
    return;
}

// Allows the user to get an format type from a string.
char* getType(char* possible_instruction) {
    const core_instruction arithmetic_core_instruction_set[] = {{"add", "r-type", 0x0, 0x20}, {"addi", "i-type", 0x8, 0x8}, {"addiu", "i-type", 0x9, 0x9}, {"addu", "r-type", 0x0, 0x21}, {"and", "r-type", 0x0, 0x24}, {"andi", "i-type", 0xC, 0xC}, {"beq", "branch", 0x4, 0x4}, {"bne", "branch", 0x5, 0x5}, {"j", "j-type", 0x2, 0x2}, {"jal", "j-type", 0x3, 0x3}, {"jr", "r-type", 0x0, 0x08}, {"lbu", "i-mem-type", 0x24, 0x24}, {"lhu", "i-mem-type", 0x25, 0x25}, {"ll", "i-mem-type", 0x30, 0x30}, {"lui", "i-mem-type", 0xF, 0xF}, {"lw", "i-mem-type", 0x23, 0x23}, {"nor", "r-type", 0x0, 0x27}, {"or", "r-type", 0x0, 0x25}, {"ori", "i-type", 0xD, 0xD}, {"slt", "r-type", 0x0, 0x2A}, {"slti", "i-type", 0xA, 0xA}, {"sltiu", "i-type", 0xB, 0xB}, {"sltu", "r-type", 0x0, 0x2B}, {"sll", "r-mem-type", 0x0, 0x00}, {"srl", "r-mem-type", 0x0, 0x02}, {"sb", "i-mem-type", 0x28, 0x28}, {"sc", "i-mem-type", 0x38, 0x38}, {"sh", "i-mem-type", 0x29, 0x29}, {"sw", "i-mem-type", 0x2B, 0x2B}, {"sub", "r-type", 0x0, 0x22}, {"subu", "r-type", 0x0, 0x23}, {"li", "load-type", 0xffff, 0xffffffff}};
    for (int i = 0; i < 32; i++) {
        if (!strcmp(possible_instruction, arithmetic_core_instruction_set[i].instruction_name)) {
            return arithmetic_core_instruction_set[i].instruction_type;
        }
    }
    return "error";
}

// Allows the user to get an register index from a string.
int getRegister(char* input_register) {
    const char valid_registers[][7] = {"$zero", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3", "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7", "$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra"};
    for(int i = 0; i < 32; i++) {
        if(strcmp(input_register, valid_registers[i]) == 0) {
            return i;
        }
    }
    return -1;
}

// Allows the user to pull data from a dictionary by using an index.
int getOperationCode(int index) {
    const core_instruction arithmetic_core_instruction_set[] = {{"add", "r-type", 0x0, 0x20}, {"addi", "i-type", 0x8, 0x8}, {"addiu", "i-type", 0x9, 0x9}, {"addu", "r-type", 0x0, 0x21}, {"and", "r-type", 0x0, 0x24}, {"andi", "i-type", 0xC, 0xC}, {"beq", "branch", 0x4, 0x4}, {"bne", "branch", 0x5, 0x5}, {"j", "j-type", 0x2, 0x2}, {"jal", "j-type", 0x3, 0x3}, {"jr", "r-type", 0x0, 0x08}, {"lbu", "i-mem-type", 0x24, 0x24}, {"lhu", "i-mem-type", 0x25, 0x25}, {"ll", "i-mem-type", 0x30, 0x30}, {"lui", "i-mem-type", 0xF, 0xF}, {"lw", "i-mem-type", 0x23, 0x23}, {"nor", "r-type", 0x0, 0x27}, {"or", "r-type", 0x0, 0x25}, {"ori", "i-type", 0xD, 0xD}, {"slt", "r-type", 0x0, 0x2A}, {"slti", "i-type", 0xA, 0xA}, {"sltiu", "i-type", 0xB, 0xB}, {"sltu", "r-type", 0x0, 0x2B}, {"sll", "r-mem-type", 0x0, 0x00}, {"srl", "r-mem-type", 0x0, 0x02}, {"sb", "i-mem-type", 0x28, 0x28}, {"sc", "i-mem-type", 0x38, 0x38}, {"sh", "i-mem-type", 0x29, 0x29}, {"sw", "i-mem-type", 0x2B, 0x2B}, {"sub", "r-type", 0x0, 0x22}, {"subu", "r-type", 0x0, 0x23}, {"li", "load-type", 0xffff, 0xffffffff}};
    return arithmetic_core_instruction_set[index].op_code;
}

// Allows the user to pull data from a dictionary by using an index.
int getFunctionCode(int index) {
    const core_instruction arithmetic_core_instruction_set[] = {{"add", "r-type", 0x0, 0x20}, {"addi", "i-type", 0x8, 0x8}, {"addiu", "i-type", 0x9, 0x9}, {"addu", "r-type", 0x0, 0x21}, {"and", "r-type", 0x0, 0x24}, {"andi", "i-type", 0xC, 0xC}, {"beq", "branch", 0x4, 0x4}, {"bne", "branch", 0x5, 0x5}, {"j", "j-type", 0x2, 0x2}, {"jal", "j-type", 0x3, 0x3}, {"jr", "r-type", 0x0, 0x08}, {"lbu", "i-mem-type", 0x24, 0x24}, {"lhu", "i-mem-type", 0x25, 0x25}, {"ll", "i-mem-type", 0x30, 0x30}, {"lui", "i-mem-type", 0xF, 0xF}, {"lw", "i-mem-type", 0x23, 0x23}, {"nor", "r-type", 0x0, 0x27}, {"or", "r-type", 0x0, 0x25}, {"ori", "i-type", 0xD, 0xD}, {"slt", "r-type", 0x0, 0x2A}, {"slti", "i-type", 0xA, 0xA}, {"sltiu", "i-type", 0xB, 0xB}, {"sltu", "r-type", 0x0, 0x2B}, {"sll", "r-mem-type", 0x0, 0x00}, {"srl", "r-mem-type", 0x0, 0x02}, {"sb", "i-mem-type", 0x28, 0x28}, {"sc", "i-mem-type", 0x38, 0x38}, {"sh", "i-mem-type", 0x29, 0x29}, {"sw", "i-mem-type", 0x2B, 0x2B}, {"sub", "r-type", 0x0, 0x22}, {"subu", "r-type", 0x0, 0x23}, {"li", "load-type", 0xffff, 0xffffffff}};
    return arithmetic_core_instruction_set[index].function_code;
}

// Allows the user to get an instruction index from a string.
int getInstruction(char* input_instruction) {
    const core_instruction arithmetic_core_instruction_set[] = {{"add", "r-type", 0x0, 0x20}, {"addi", "i-type", 0x8, 0x8}, {"addiu", "i-type", 0x9, 0x9}, {"addu", "r-type", 0x0, 0x21}, {"and", "r-type", 0x0, 0x24}, {"andi", "i-type", 0xC, 0xC}, {"beq", "branch", 0x4, 0x4}, {"bne", "branch", 0x5, 0x5}, {"j", "j-type", 0x2, 0x2}, {"jal", "j-type", 0x3, 0x3}, {"jr", "r-type", 0x0, 0x08}, {"lbu", "i-mem-type", 0x24, 0x24}, {"lhu", "i-mem-type", 0x25, 0x25}, {"ll", "i-mem-type", 0x30, 0x30}, {"lui", "i-mem-type", 0xF, 0xF}, {"lw", "i-mem-type", 0x23, 0x23}, {"nor", "r-type", 0x0, 0x27}, {"or", "r-type", 0x0, 0x25}, {"ori", "i-type", 0xD, 0xD}, {"slt", "r-type", 0x0, 0x2A}, {"slti", "i-type", 0xA, 0xA}, {"sltiu", "i-type", 0xB, 0xB}, {"sltu", "r-type", 0x0, 0x2B}, {"sll", "r-mem-type", 0x0, 0x00}, {"srl", "r-mem-type", 0x0, 0x02}, {"sb", "i-mem-type", 0x28, 0x28}, {"sc", "i-mem-type", 0x38, 0x38}, {"sh", "i-mem-type", 0x29, 0x29}, {"sw", "i-mem-type", 0x2B, 0x2B}, {"sub", "r-type", 0x0, 0x22}, {"subu", "r-type", 0x0, 0x23}, {"li", "load-type", 0xffff, 0xffffffff}};
    for (int i = 0; i < 39; i++) {
        if(strcmp(input_instruction, arithmetic_core_instruction_set[i].instruction_name) == 0) {
            return i;
        }
    }
    return -1;
}

// Convert all non memory R types.
void convertR(char instruction[4][10]) {
    int op_code, rs, rd, rt, shamt, funct;
    op_code = getOperationCode(getInstruction(instruction[0]));
    if((isdigit(*instruction[1]) != 0 || isdigit(*instruction[2]) != 0 || isdigit(*instruction[3]) != 0)  || (isalpha(*instruction[1]) != 0 || isalpha(*instruction[2]) != 0 || isalpha(*instruction[3]) != 0)) {
        printf("%s", ERROR_FORMAT);
        return;
    } else {
        if (getRegister(instruction[1]) == -1 || getRegister(instruction[2]) == -1 || getRegister(instruction[3]) == -1) {
            printf("%s", ERROR_REGISTER);
            return;
        } else {  
            rd = getRegister(instruction[1]);
            rs = getRegister(instruction[2]);
            rt = getRegister(instruction[3]);
        }
    }
    funct = getFunctionCode(getInstruction(instruction[0]));
    printf("%08x\n", op_code << 26 | rs << 21 | rt << 16 | rd << 11 | 0x0 << 6 | funct);
}

// Convert all non memory I types.
void convertI(char instruction[4][10]) {
    if (strcmp("addi", instruction[0]) == 0 && strcmp(instruction[3], "$t0") == 0) {
        printf("%s", ERROR_FORMAT);
        return;
    }
    if (getRegister(instruction[1]) != -1 && getRegister(instruction[2]) != -1) {
        int rt = getRegister(instruction[1]);
        int rs = getRegister(instruction[2]);
        int op_code = getOperationCode(getInstruction(instruction[0]));
        int immediate = abs(strtol(instruction[3], NULL, 0));
        printf("%08x\n", op_code << 26 | rs << 21 | rt << 16 | immediate);
    } else {
        printf("%s", ERROR_REGISTER);
        return;
    }
}

// Handles all J types.
void convertJ(char instruction[4][10]) {
    int op_code, immediate;
    if((getRegister(instruction[2]) != -1 || getRegister(instruction[3]) != -1) || (isdigit(*instruction[2]) != 0 || isdigit(*instruction[3]) != 0 )) {
        printf("%s", ERROR_FORMAT);
        return;
    }
    op_code = getOperationCode(getInstruction(instruction[0]));
    if(getRegister(instruction[1]) == -1 && isdigit(*instruction[1]) != 0) {
        immediate = abs((int)strtol(instruction[1], NULL, 0));
    } else {
        printf("%s", ERROR_FORMAT);
        return;
    }
    printf("%08x\n", op_code << 26 | immediate);
}

// Handles all non memory I types.
void convertIMemory(char instruction[4][10]) {
    if(getRegister(instruction[3]) != -1 || isdigit(*instruction[3]) != 0 || isalpha(*instruction[3]) != 0) {
        printf("%s", ERROR_FORMAT);
        return;
    }
    if (strcmp("lui", instruction[0]) == 0) {
        int rt = getRegister(instruction[1]);
        int rs = 0;
        int op_code = getOperationCode(getInstruction(instruction[0]));
        int immediate = (int)strtol(instruction[2], NULL, 0);
        printf("%08x\n", op_code << 26 | rs << 21 | rt << 16 | immediate);
        return;
    }
     char temp[20];
        int immediate;
        sscanf(instruction[2], " %d ( %s )", &immediate, temp);
        char* newRS = strtok(temp, ")");
        int rt = getRegister(instruction[1]);
        int rs = getRegister(newRS);
        int op_code = getOperationCode(getInstruction(instruction[0]));
        int clean = abs(immediate);
        printf("%08x\n", op_code << 26 | rs << 21 | rt << 16 | abs(clean));
        return;
}

// Handles SLL and SRL.
void convertRMemory(char instruction[4][10]) {
    int op_code, rd, rt, shamt, funct;
    op_code = getOperationCode(getInstruction(instruction[0]));
    if ((isdigit(*instruction[1]) != 0 || isdigit(*instruction[2]) != 0) || (isalpha(*instruction[1]) != 0 || isalpha(*instruction[2]) != 0 || isalpha(*instruction[3]) != 0)) {
        printf("%s", ERROR_FORMAT);
        return;
    } else {
        if ((getRegister(instruction[1]) == -1 || getRegister(instruction[2]) == -1) || (isalpha(*instruction[1]) != 0 || isalpha(*instruction[2]) != 0 || isalpha(*instruction[3]) != 0)) {
            printf("%s", ERROR_REGISTER);
            return;
        } else {  
            rd = getRegister(instruction[1]);
            rt = getRegister(instruction[2]);
        }
    }
    if(getRegister(instruction[3]) == -1 && isdigit(*instruction[3]) != 0) {
        shamt = abs(strtol(instruction[3], NULL, 0));
    } else {
        printf("%s", ERROR_FORMAT);
        return;
    }
    funct = getFunctionCode(getInstruction(instruction[0]));
    printf("%08x\n", op_code << 26 | 0x0 << 21 | rt << 16 | rd << 11 | shamt << 6 | funct);
}

void convertLoadImmediate(char instruction[4][10]) {
    int immediate;
    char newInstruction[4][10];
    int fifteenBit = getOperationCode(getInstruction(instruction[0]));
    int thirtyOneBit = getFunctionCode(getInstruction(instruction[0]));
    if ((isalpha(*instruction[1]) || isalpha(*instruction[2])) || (isdigit(*instruction[1]) || (getRegister(instruction[2]) != -1 || (getRegister(instruction[2]) == -1 && isdigit(*instruction[2]) == 0)))) {
        printf("%s", ERROR_FORMAT);
        return;
    } else {
        if (getRegister(instruction[1]) == -1) {
            printf("%s", ERROR_REGISTER);
            return;
        } else {
            immediate = abs(strtol(instruction[2], NULL, 0));
            if (immediate <= fifteenBit) {
                printf("is addi\n");
                strcpy(newInstruction[0], "addi");
                strcpy(newInstruction[1], instruction[1]);
                strcpy(newInstruction[2], "$zero");
                sprintf(newInstruction[3], "%d", immediate);
                convertI(newInstruction);
            } else if ((immediate > fifteenBit) && (immediate <= thirtyOneBit)) {
                printf("is ori\n");
            } else {
                printf("is lui\n");
            }
        }
    }
}

void convertBranch(char instruction[4][10]) {
    int op_code, rs, rt, immediate;
    op_code = getOperationCode(getInstruction(instruction[0]));
    if((isdigit(*instruction[1]) != 0 || isdigit(*instruction[2]) != 0 || getRegister(instruction[3]) != -1) || (isalpha(*instruction[1]) != 0 || isalpha(*instruction[2]) != 0 || isalpha(*instruction[3]) != 0)) {
        printf("%s", ERROR_FORMAT);
        return;
    } else {
        if (getRegister(instruction[1]) == -1 || getRegister(instruction[2]) == -1) {
            printf("%s", ERROR_REGISTER);
            return;
        } else {
            if (isdigit(*instruction[3]) != 0) {
                rt = getRegister(instruction[1]);
                rs = getRegister(instruction[2]);
                immediate = abs((int)strtol(instruction[3], NULL, 0));
            } else {
                printf("%s", ERROR_FORMAT);
                return;
            }
        }
    }
    printf("%08x\n", op_code << 26 | rs << 21 | rt << 16 | immediate);
}
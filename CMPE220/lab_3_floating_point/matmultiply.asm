		.data
prompt_row_A:	.asciiz		"Enter the number of rows in A: "
prompt_col_A:	.asciiz		"Enter the number of cols in A: "
prompt_row_B:	.asciiz		"Enter the number of rows in B: "
prompt_col_B:	.asciiz		"Enter the number of cols in B: "
prompt_float_A:	.asciiz		"Enter floating-point value for A: "
prompt_float_B:	.asciiz		"Enter floating-point value for B: "
prompt_matrix:	.asciiz		"Size of the matrix C: "
newline_char:	.asciiz		"\n"
		.text
main:
# [$t0, $t1] for A; [$t2, $t3] for B
# $t4 is the number of floats in A, $t5 is the number of floats in B
# $t6 is the size of C
li $t9, 0
li $v0, 4
la $a0, prompt_row_A
syscall
li $v0, 5
syscall
move $t0, $v0
li $v0, 4
la $a0, prompt_col_A
syscall
mul $t4, $t0, $t1

B:
li $v0, 5
syscall
move $t1, $v0
li $v0, 4
la $a0, newline_char
syscall
li $v0, 4
la $a0, prompt_row_B
syscall
li $v0, 5
syscall
move $t2, $v0
li $v0, 4
la $a0, prompt_col_B
syscall
li $v0, 5
syscall
move $t3, $v0
mul $t4, $t0, $t1
mul $t5, $t2, $t3
mul $t6, $t4, $t5
li $v0, 4
la $a0, newline_char
syscall
li $v0, 4
la $a0, prompt_matrix
syscall
li $v0, 1
move $a0, $t6
syscall

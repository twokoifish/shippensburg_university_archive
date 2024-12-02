		.data
array:		.word		5, 1, 4, 2, 8
nl:		.asciiz		"\n"
s:		.asciiz		"Called swap\n"

		.text
main:
la $a1, array
li $a2, 5
addi $sp, $sp, -4
addi $sp, $sp, 4
jal sort


loop:
li $v0, 1
lw $a0, array($t0)
syscall
li $v0, 4
la $a0, nl
syscall
addi $t0, $t0, 4
blt $t0, 20, loop
j end

end:
jal swap
li $v0, 10
syscall


sort:
li $t1, 0
j o_loop
	o_loop:
	bge $t1, 5, e_sort
	addi $t2, $t1, 0
	j i_loop
	
		i_loop:
		addi $sp, $sp, -4
		addi $sp, $sp, 4
		bge $t2, 4, e_i_loop
		
		lw $t3, 0($a1)
		lw $t4, 4($a1)
		lw $t5, 8($a1)
		lw $t6, 12($a1)
		lw $t7, 16($a1)
		
		ble $t3, $t4, n1
		j swap1
		n1:
		ble $t4, $t5, n2
		j swap2
		n2:
		ble $t5, $t6, n3
		j swap3
		n3:
		ble $t6, $t7, skip
		j swap4
		
		skip:
		addi $t2, $t2, 1
		j i_loop

		e_i_loop:
		addi $t1, $t1, 1
		j o_loop
e_sort:
jr $ra

swap1:
move $t8, $t3
sw $t4, 0($a1)
sw $t3, 4($a1)
j n1

swap2:
move $t8, $t4
sw $t5, 4($a1)
sw $t4, 8($a1)
j n2

swap3:
move $t8, $t5
sw $t6, 8($a1)
sw $t5, 12($a1)
j n3

swap4:
move $t8, $t6
sw $t7, 12($a1)
sw $t6, 16($a1)
j skip


swap:
add $sp, $sp -16
sw $ra, 0($sp)
lw $ra, 0($sp)
add $sp, $sp, 16
jr $ra



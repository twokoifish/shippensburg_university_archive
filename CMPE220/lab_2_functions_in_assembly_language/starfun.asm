		.data
prompt:		.asciiz		"How many stars? "
star:		.asciiz		"*"
newline:	.asciiz		"\n"

		.text
main:
	# Load and print 'prompt'.
	la $a0, prompt
	li $v0, 4
	syscall
	
	# Load and store input into '$s0'.
	li $v0, 5
	syscall
	add $s0, $0, $v0
	
	# Store '$s0' inn active memory.
	sw $s0, 0($sp)
	
	# Call and run 'print' with '$s0'.
	jal print
	
	# End of program.
	end:
	li $v0, 10
	syscall
	
	# The function to print stars
	print:
	lw $t0, 0($sp)
	
	li $t1, 1
	
	# The outer loop of the function.
	outer_loop:
	bgt $t1, $t0, end
	
	li $t2, 0
	
	jal inner_loop
	
	la $a0, newline
	li $v0, 4
	syscall
	
	addi $t1, $t1, 1
	
	j outer_loop
	
	# The inner loop of the function.
	inner_loop:
	beq $t2, $t1,return
	
	la $a0, star
	li $v0, 4
	syscall
	
	addi $t2, $t2, 1
	
	j inner_loop
	
	# Return to previous position.
	return: 
	jr $ra

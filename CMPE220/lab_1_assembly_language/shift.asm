		.data
prompt:		.asciiz		"Enter a number: "
newline:	.asciiz		"\n"

		.text
main:
	la $a0, prompt
	li $v0, 4
	syscall
	
	li $v0, 5
	syscall
	move $t1, $v0
	
	sll $t2,$t1,1
	srl $t3,$t1,1
	
	li $v0, 1
	move $a0, $t2
	syscall
	
	la $a0, newline
	li $v0, 4
	syscall
	
	li $v0, 1
	move $a0, $t3
	syscall
	
	
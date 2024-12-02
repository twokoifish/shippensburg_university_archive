		.data
prompt:		.asciiz		"Enter a number: "
newline:	.asciiz		"\n"
array:		.word		1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010

		.text
main:
	la $a0, prompt		# Load address 'prompt'
	li $v0, 4		# Load immediate string value of $v0
	syscall
	
	li $v0, 5		# Load immediate integer value of $v0
	syscall
	move $t1, $v0		# Move the value of $v0 to $t1
	
	la $a1, array		# Load address 'array'
	sub $t1,$t1,1		# Subtract 1 from $t1 and store the new value into $t1
	
	sll $t2,$t1,2		# Shift the value of $t1 to the left by 2
	
	lw $t3, array($t2)      # Load the word at the address of 'array($t2)' and store it into $t3
	
	li $v0, 1		# Load the immediate integer value of $v0
	move $a0, $t3		# Move the value of $t3 to $a0
	syscall
	
	
	
	
	
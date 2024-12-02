# Create new data section
		.data
prompt:		.asciiz		"Enter two numbers: "
newline:	.asciiz		"\n"
less:		.asciiz		"A is less than B\n"
equal:		.asciiz 	"A is equal to B\n"
greater:	.asciiz		"A is greater than B\n"

# Start new text section
		.text
main:
	la $a0, prompt     # Load address of the label 'prompt'
	li $v0, 4	   # Load immediate string value of $v0
	syscall		   # System call
	
	li $v0, 5	   # Load immediate integer value of $v0
	syscall		   # System call
	move $t1, $v0	   # Store $v0 into $t1
	
	li $v0, 5	   # Load immediate integer value of $v0
	syscall		   # System call
	move $t2, $v0	   # Store $v0 into $t2
	
	bgt $t1,$t2,EQ    # If $t1 > $t2, move on to 'EQ'
	beq $t1,$t2,EQ    # If $t1 == $t2, move on to 'EQ'
	la $a0, less      # Load address of the label 'less'
	li $v0, 4	  # Load immediate string value of $v0
	syscall
	j SKIP		  # Jump to 'SKIP'
	EQ: 
	bne $t1,$t2,GT    # If $t1 != $t2, move on to 'GT'
	la $a0, equal     # Load address of the label 'equal'
	li $v0, 4	  # Load immediate string value of $v0	
	syscall
	j SKIP		  # Jump to 'SKIP'
	GT:
	la $a0, greater   # Load address of the label 'greater'
	li $v0, 4	  # Load immediate string value of $v0	
	syscall
	j SKIP		  # Jump to 'SKIP'
	SKIP:
	li $v0, 10         # Exit program
	syscall		   # System call
	
		
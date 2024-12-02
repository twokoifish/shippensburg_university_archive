# Create new data section
		.data
prompt:		.asciiz		"Enter two numbers: "
newline:	.asciiz		"\n"

# Start new text section
		.text
main:
	la $a0, prompt     # Load prompt address
	li $v0, 4	   # Print data from prompt address
	syscall		   # System call
	
	li $v0, 5	   # Take in integer from command line
	syscall		   # System call
	move $t1, $v0	   # Store $v0 into $t1
	
	li $v0, 5	   # Take in integer from command line
	syscall		   # System call
	move $t2, $v0	   # Store $v0 into $t1
	
	and $t3,$t1,$t2
	or $t4,$t1,$t2
	xor $t5,$t1,$t2
	
	li $v0, 1	   # Print sum to command line
	move $a0, $t3      # Store $t3 into $a0
	syscall		   # System call
	
	la $a0, newline    # Load newline address
	li $v0, 4	   # Print data from newline address
	syscall		   # System call
	
	li $v0, 1	   # Print sum to command line
	move $a0, $t4      # Store $t4 into $a0
	syscall		   # System call
	
	la $a0, newline    # Load newline address
	li $v0, 4	   # Print data from newline address
	syscall		   # System call
	
	li $v0, 1	   # Print sum to command line
	move $a0, $t5      # Store $t5 into $a0
	syscall		   # System call
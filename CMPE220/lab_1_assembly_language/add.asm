# Create new data section for prompt
		.data
prompt:		.asciiz		"Enter two numbers: "

# Create new text section for input and return
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
	
	add $t3,$t1,$t2    # Add the two integers together
	
	li $v0, 1	   # Print sum to command line
	move $a0, $t3      # Store $t3 into $a0
	syscall		   # System call
	
	li $v0, 10         # Exit program
	syscall		   # System call
	
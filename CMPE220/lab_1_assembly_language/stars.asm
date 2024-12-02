# Data Section
		.data
prompt:		.asciiz		"My God! Its full of how many stars?: "
star:		.asciiz		"*"
newline:	.asciiz		"\n"

# Text Section
		.text
main:
	la $a0, prompt		# Load address 'prompt'
	li $v0, 4		# Load immediate string value of $v0
	syscall		        # System call
	
	li $v0, 5		# Load immediate integer value of $v0
	syscall		        # System call
	move $t1, $v0		# Move the value of $v0 to $t1
	
	add $t0,$zero,$zero     # Add zero to $t0
	
	top:
		bge $t0,$t1,end # If $t0 > $t1, branch to 'end'
		la $a0, star    # Load address 'star'
		li $v0, 4	# Load immediate string value of $v0
		syscall		# System call
		addi $t0,$t0,1  # Add Immediate, add 1 to $t0
		j top
	end:
		la $a0, newline # Load address 'newline'
		li $v0, 4       # Load immediate string value of $v0
		syscall		# System call
		li $v0, 10      # Exit program
		syscall		# System call
		
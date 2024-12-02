# new data section
	.data
prompt:		.asciiz		"Enter your name:"         # creates a label for prompt
hello:		.asciiz		"hello "                   # creates a label for hello
meet:		.asciiz		", pleased to meet you!\n" # creates a label for meet
name:		.space 		32		   	   # creates a label for name

# text section
	.text
main:
	la	$a0, prompt # Load address for label 'prompt'
	li	$v0, 4      # Load immediate string value for $v0
	syscall             # Execute system call for $v0
	la	$a0, name   # Load address for label 'name'
	li	$a1, 32	    # Load immediate string value upto 32 bytes for $a1
	li	$v0, 8      # Load immediate string value from console
	syscall             # Execute system call for $v0
	loop:
		lb $a3, name($a2)  
    		addi $a2, $a2, 1
    		bnez $a3, loop      # If not null, loop again
   		beq $a1, $a2, end
    		subi $a2, $a2, 2    # Remove the null \n
    		sb $0, name($a2)    # Replace it with \0
	end:
		la	$a0, hello  # Load address for label 'hello'
		li	$v0, 4      # Load immediate string value for $v0
		syscall             # Execute system call for $v0
		la	$a0, name   # Load address for label 'name'
		li	$v0, 4      # Load immediate string value for $v0
		syscall             # Execute system call for $v0
		la	$a0, meet   # Load address for label 'meet'
		li	$v0, 4      # Load immediate string value for $v0
		syscall             # Execute system call for $v0
		li $v0, 10          # Exit program
		syscall		    # Execute system call for $v0
	
	
	

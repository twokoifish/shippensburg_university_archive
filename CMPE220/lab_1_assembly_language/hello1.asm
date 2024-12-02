# new data section
	.data
prompt:		.asciiz		"Enter your name:"         # creates a label for prompt
hello:		.asciiz		"hello "                   # creates a label for hello
meet:		.asciiz		", pleased to meet you!\n" # creates a label for meet
name:		.space 		32		   	   # creates a label for name

# text section
	.text
main:
	# prompt the user for their name
	la	$a0, prompt # load the prompt into memory
	li	$v0, 4      # print the prompt
	syscall             # execute system call for $v0
	
	# get the name from the user
	la	$a0, name   # load the name into memory
	li	$a1, 32	    # set a maximum of 32 charaters to read
	li	$v0, 8      # read the string from console
	syscall             # execute system call for $v0
	
	# print the hello prompt
	la	$a0, hello  # load the hello into memory
	li	$v0, 4      # print the hello prompt
	syscall             # execute system call for $v0
	
	# print the user's name
	la	$a0, name   # load the name into memory
	li	$v0, 4      # print the name
	syscall             # execute system call for $v0
	
	# print the greeting
	la	$a0, meet   # load the meet into memory
	li	$v0, 4      # print the meet
	syscall             # execute system call for $v0
	
	
	
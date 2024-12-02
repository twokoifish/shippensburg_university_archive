# Data Section
		.data
prompt1:	.asciiz		"Enter text: "
prompt2:	.asciiz		"Enter size of box: "
text: 		.space		32 
inner: 		.asciiz		"* "
star: 		.asciiz		"*"
outer: 		.asciiz		" *\n"
newline:	.asciiz		"\n"

# Text Section
		.text
main: 
	la 	$a0, prompt1	# Load prompt1 address into memory
	li	$v0, 4			# Load immediately for printing
	syscall 			# Print 
	
	la 	$a0, text 		# Load text address into memory
	li	$a1, 32			# Load immediately with a max 32 chars 
	li 	$v0, 8			# Load immediately for reading a string
	syscall				# Read 
	
	loop: # Remove new line from string 
		lb 	$s3, text($s2)			# Load the $s2 position of text, store into #$s3
		addi 	$s2, $s2, 1			# Increment our counter to keep position
		bnez 	$s3, loop			# Test if not null, restart loop 
		beq 	$s1, $s2, end		# Test if $s1 and $s2 are equal, meaning there is no newline. 
									# and then skip to end
		subi 	$s2, $s2, 2			# Remove the newline from the string 
		sb 	$0, text($s2) 			# Replace with \0 
	end: # End of new line removal 
		j readint # Skip to readint
	
	readint: # Read in box size
		la 	$a0, prompt2	# Load address of prompt2
		li 	$v0, 4		# Load string immediately for printing string 
		syscall			# Print it out 
	
		li 	$v0, 5		# Load immediately for reading int 
		syscall			# Read 
		move    $a1, $v0	# Move location into $a1 for easy access
		j print			# Continue onwards to print the stars
		
	# The meat
	print: # Print our boxes 
	
		# Set increment counter to 0, jump to stars method and print stars.  
		add	$a2,$zero, 0
		jal stars
		
		# Print newline 
		li 	$v0, 4
		la 	$a0, newline
		syscall
	
		# Jump to print text, print middle line 
		jal printtext	
	
		# Reset increment ocunter, jump to start method to print stars. 
		add 	$a2,$zero, 1
		jal	stars
		
		li 	$v0, 4		# Load immediately for printing
		la 	$t1, newline	# Load address of newline for formatting
		syscall			# Print 
		
		j exit
	
	exit: # Exit the program properly 
		li $v0, 4
		la $a0, newline
		syscall
		
		li $v0, 10		# Unload
		syscall			# Syscall
	
	# $a2 = increment 
	# $a1 = goal 	
	
	
# Method Registry Map
# a0 - temp registry, used for prinitng 
# a1 - goal int, num of stars to print
# a2 - increment variable 

stars:		# "Method" for printing stars
	beq $a2, $a1, endstars 	# If $a0 = $a1, branch to 'end' 
	li	$v0, 4				# Load immediately for printing
	la 	$a0, star			# Load address of star
	syscall					# Print 
	
	addi	$a2, $a2, 1		# Increment $a2
	
	j stars
	
	endstars: 	
		jr 	$ra		# Return
	
printtext:
	# Load inner "* "
	la 	$a0, inner
	li 	$v0, 4		
	syscall			
	
	# Print text 
	la 	$a0, text
	li 	$v0, 4	
	syscall		 
	
	# Print outer " *"
	la	$a0, outer	
	li	$v0, 4		
	syscall
	
	# Return
	jr $ra			

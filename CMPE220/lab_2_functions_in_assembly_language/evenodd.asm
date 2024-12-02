		.data
prompt:		.asciiz		"Enter a number: "
odd_msg:	.asciiz		"It is odd"
even_msg:	.asciiz		"It is even"

		.text
main:
	jal read_number
	move $a0, $v0
	jal print_evenodd
	
end:
	li $v0, 10
	syscall

read_number:
	addi $sp, $sp, -4
	sw $0, 0($sp)
	
	la $a0, prompt
	li $v0, 4
	syscall
	
	li $v0, 5
	syscall
	
	sw $v0, 0($sp)
	lw $v0, 0($sp)
	addi $sp, $sp, 4
	jr $ra
	
print_evenodd:
	addi $sp, $sp, -16
	sw $ra, 0($sp)
	jal isodd
	
	beq $v0, 0, isE
	li $v0, 4
	la $a0, odd_msg
	syscall
	j cont
	
	isE:
	li $v0, 4
	la $a0, even_msg
	syscall	
	
	cont:
	lw $ra, 0($sp)
	addi $sp, $sp, 16
	jr $ra
	
isodd:
	and $v0, $a0, 0x01
	jr $ra
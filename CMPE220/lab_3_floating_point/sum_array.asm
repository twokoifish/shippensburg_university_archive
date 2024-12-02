		.data
		.align  2
array:		.space	160
start_prompt:	.asciiz "Enter the number of elements, up to 20: "
total_prompt:	.asciiz "Total: "
newline_char:	.asciiz "\n"
		.text
main:
li $t0, 0 # int i
li $t2, 0 # int j
li $t3, 0 # shift
la $t4, array

mainloop:
la $a0, start_prompt
li $v0, 4
syscall
li $v0, 5
syscall
move $t0, $v0

bgt $t0, 20, mainloop
blt $t0, 0, mainloop
j main_loop_2
	
main_loop_2:
beq $t2, $t0, print
li $v0, 6
syscall
swc1 $f0, ($t4)
add $t4, $t4, 8
add $t2, $t2, 1
j main_loop_2
																		
print:
la $a1, array
la $a2, ($t0)
jal sum_array
la $a0, total_prompt
li $v0, 4
syscall
li $v0, 2
syscall
la $a0, newline_char
li $v0, 4
syscall
j end

end:
li $v0, 10
syscall

sum_array:
la $t4, ($a1)
la $t5, ($a2)
li $t6, 0
sum_loop:
beq $t6, $t5, exit
lwc1 $f0, ($t4)
add.s $f12, $f12, $f0
add $t4, $t4, 8
add $t6, $t6, 1
j sum_loop
exit:
jr $ra

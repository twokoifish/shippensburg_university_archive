		.data
		.align 2
fp1:		.double 1.0
fp2:		.double 2.0
fp3:		.double 4.0
terms_prompt:	.asciiz		"Enter number of terms: "
pi_approx:	.asciiz		"Pi is: "

		.text
main:
li $t0, 0 #terms

li $v0, 4
la $a0, terms_prompt
syscall

li $v0, 5
syscall
move $t0, $v0

la $a1, ($t0)
jal gregory

li $v0, 3
syscall
j end

end:
li $v0, 10
syscall

gregory:
la $t2, ($a1) # int n
li $t1, 0 #int i = 0
l.d $f0, fp1 # denmon
l.d $f2, fp1 # 1.0
l.d $f4, fp2 # 2.0
l.d $f6, fp3 # 4.0

greg_loop:
beq $t1, $t2, exit
div.d $f8, $f2, $f0
and $t4, $t1, 0x01
beq $t4, 0, even
sub.d $f12, $f12, $f8
j cont
even:
add.d $f12, $f12, $f8
j cont
cont:
add $t1, $t1, 1
add.d $f0, $f0, $f4
j greg_loop

exit:
mul.d $f12, $f12, $f6
jr $ra

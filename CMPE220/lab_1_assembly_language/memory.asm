# new memory segment
	.data
A:	.word	100
B:	.word	200
C:	.word	300

	.text
main:	
	LA $t0, A
	LW $t1, 0($t0)
	LA $t2, B
	LW $t3  0($t2)
	SW $t1, 4($t0)
# example of assembly language data section
	.data
	.align 2
a_char:		.byte		'A'
another_char: 	.byte		65

# an integer and an array of integers
an_int:		.word		32
an_array:	.word		0 1 2 3 4 5
an_array2:	.word		0 : 4

# 64-bit float
a_double:	.double		3.14159265359

# null terminated ASCII string
a_string:	.asciiz		"hello world"
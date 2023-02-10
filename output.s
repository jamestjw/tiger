	.globl	main
	.type	main, @function

L1:
	li t132, 10
	mv a1, t132
	li t133, 0
	mv a2, t133
	call initArray
	mv t131, a0
	mv t130, t131
	j L0
L0:



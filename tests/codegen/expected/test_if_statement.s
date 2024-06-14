	.text
	.globl	tigermain
	.type	tigermain, @function
tigermain:
	addi sp, sp, -32
	sd fp, 24(sp)
	addi fp, sp, 32

L4:
	sd a0, -16(fp)
	mv t142, s11
	mv t141, s10
	mv t140, s9
	mv t139, s8
	mv t138, s7
	mv t137, s6
	mv t136, s5
	mv t135, s4
	mv t134, s3
	mv t133, s2
	mv t132, s1
	mv t131, ra
	li t143, 8
	sd t143, -24(fp)
	li t144, 10
	sd t144, -32(fp)
	ld t145, -24(fp)
	ld t146, -32(fp)
	blt t145, t146, L0
L1:
	li t147, 30
	mv t130, t147
L2:
	mv t148, zero
	mv a0, t148
	mv ra, t131
	mv s1, t132
	mv s2, t133
	mv s3, t134
	mv s4, t135
	mv s5, t136
	mv s6, t137
	mv s7, t138
	mv s8, t139
	mv s9, t140
	mv s10, t141
	mv s11, t142
	j L3
L0:
	li t149, 20
	mv t130, t149
	j L2
L3:
	mv sp, fp
	ld fp, -8(fp)
	jr ra


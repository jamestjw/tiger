	.text
	.globl	tigermain
	.type	tigermain, @function
tigermain:
	addi sp, sp, -24
	sd fp, 16(sp)
	addi fp, sp, 24

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
	mv t143, zero
	sd t143, -24(fp)
	li t144, 10
	mv t130, t144
	ld t145, -24(fp)
	addi t146, t130, 1
	blt t145, t146, L1
L0:
	mv t147, zero
	mv a0, t147
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
L1:
	ld t148, -24(fp)
	mv a0, t148
	call exit
	ld t149, -24(fp)
	bge t149, t130, L0
L2:
	ld t151, -24(fp)
	addi t150, t151, 1
	sd t150, -24(fp)
	j L1
L3:
	mv sp, fp
	ld fp, -8(fp)
	jr ra


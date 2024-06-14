	.text
	.globl	tigermain
	.type	tigermain, @function
tigermain:
	addi sp, sp, -32
	sd fp, 24(sp)
	addi fp, sp, 32

L4:
	sd a0, -16(fp)
	mv t141, s11
	mv t140, s10
	mv t139, s9
	mv t138, s8
	mv t137, s7
	mv t136, s6
	mv t135, s5
	mv t134, s4
	mv t133, s3
	mv t132, s2
	mv t131, s1
	mv t130, ra
	mv t143, zero
	sd t143, -24(fp)
	li t144, 10
	sd t144, -32(fp)
L1:
	ld t145, -24(fp)
	ld t146, -32(fp)
	blt t145, t146, L2
L0:
	mv t147, zero
	mv a0, t147
	mv ra, t130
	mv s1, t131
	mv s2, t132
	mv s3, t133
	mv s4, t134
	mv s5, t135
	mv s6, t136
	mv s7, t137
	mv s8, t138
	mv s9, t139
	mv s10, t140
	mv s11, t141
	j L3
L2:
	ld t149, -24(fp)
	mv a0, t149
	call chr
	mv t148, a0
	mv t142, t148
	mv a0, t142
	call print
	ld t151, -24(fp)
	addi t150, t151, 1
	sd t150, -24(fp)
	j L1
L3:
	mv sp, fp
	ld fp, -8(fp)
	jr ra


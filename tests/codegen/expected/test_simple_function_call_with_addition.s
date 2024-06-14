	.text
	.globl	tigermain
	.type	tigermain, @function
tigermain:
	addi sp, sp, -32
	sd fp, 24(sp)
	addi fp, sp, 32

L2:
	sd a0, -16(fp)
	mv t153, s11
	mv t152, s10
	mv t151, s9
	mv t150, s8
	mv t149, s7
	mv t148, s6
	mv t147, s5
	mv t146, s4
	mv t145, s3
	mv t144, s2
	mv t143, s1
	mv t142, ra
	li t154, 8
	sd t154, -24(fp)
	li t155, 10
	sd t155, -32(fp)
	mv a0, fp
	ld t156, -24(fp)
	mv a1, t156
	ld t157, -32(fp)
	mv a2, t157
	call L0_add
	mv t158, zero
	mv a0, t158
	mv ra, t142
	mv s1, t143
	mv s2, t144
	mv s3, t145
	mv s4, t146
	mv s5, t147
	mv s6, t148
	mv s7, t149
	mv s8, t150
	mv s9, t151
	mv s10, t152
	mv s11, t153
	j L1
L1:
	mv sp, fp
	ld fp, -8(fp)
	jr ra

	.text
	.globl	L0_add
	.type	L0_add, @function
L0_add:
	addi sp, sp, -32
	sd fp, 24(sp)
	addi fp, sp, 32

L4:
	sd a0, -16(fp)
	sd a1, -24(fp)
	sd a2, -32(fp)
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
	ld t160, -32(fp)
	ld t161, -24(fp)
	add t159, t161, t160
	mv a0, t159
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
L3:
	mv sp, fp
	ld fp, -8(fp)
	jr ra


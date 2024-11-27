	.equ sbi_print_code, 0x4442434E

	.macro call_sbi_print
	li	a7, sbi_print_code
	li	a6, 0
	ecall
	.endm

	.global glbfunc

	.data
	.string "Hello world!\n"
	.string "One string.\0Another string."
	.string "\x65\146\x65\146\x65\146\x65\146\x65\146"
	.byte 'a', 'b', 'c', 'd'
	.byte 1, 2, 3, 4
	.half 5, 6, 7, 8
	.zero 99
my_var:	.word 9, 10, 11, 12

	.text
	nop
	nop
	nop
func:	nop
	nop
	nop
	nop

.text
testthing:
upper:
	lui t0, 4
	lui t0, %hi(8000)
	lui t0, %hi(func)
	lui t0, %hi(func + 8000)
	lui t0, %hi(glbfunc)
	auipc t0, 4
	auipc t0, %hi(func)
	auipc t0, %pcrel_hi(8000)
	auipc t0, %pcrel_hi(func)
	auipc t0, %pcrel_hi(func + 8000)
	auipc t0, %pcrel_hi(glbfunc)

jal:
	jal ra, 8
	jal ra, func
	jal ra, func + 8
	jal ra, glbfunc + 8

branch:	
#	beq x25, x26, 8
	beq x25, x26, func
	beq x25, x26, func + 512
#	beq x25, x26, glbfunc + 512

#	bne x25, x26, 8 
	bne x25, x26, func 
	bne x25, x26, func + 512
#	bne x25, x26, glbfunc + 512

#	blt x25, x26, 8
	blt x25, x26, func
	blt x25, x26, func + 512
#	blt x25, x26, glbfunc + 512

#	bge x25, x26, 8
	bge x25, x26, func
	bge x25, x26, func + 512
#	bge x25, x26, glbfunc + 512

#	bltu x25, x26, 8
	bltu x25, x26, func
	bltu x25, x26, func  + 512
#	bltu x25, x26, glbfunc  + 512

#	bgeu x25, x26, 8
	bgeu x25, x26, func
	bgeu x25, x26, func  + 512
#	bgeu x25, x26, glbfunc  + 512
	
store:
	sb x20, -22(x25)
	sh x21, 224(x25)
	sw x22, 500(x25)
	jalr x23, 0x8(x25)

load:	
	lb  x20, -22(x25)
	lh  x21, 224(x25)
	lw  x22, 500(x25)
	lhu x23, 0x8(x25)
	lbu x24, 0b001(x25)

immediate:
	addi x1, x2,  -'0'
	slti x3, x4,  0b01100101
	sltiu x5, x6, 0b01100101
	xori x7, x8,  0xAB
	ori  x9, x10, 324
	andi x11, x12, 999

register_shamt:	
	slli x13, x14, 5
	srli x15, x16, 28
	srai x17, x18, 31

register:
	add zero, ra, sp
	sub gp, tp, t0
	slt s1, a0, a1
	sltu a2, a3, a4
	xor  a5, a6, a7
	sll t1, t2, s0
	srl  s2, s3, s4
	sra  s5, s6, s7
	or   s8, s9, s10
	and  s11, t3, t4

lw_low_model:	
	# Load value from a symbol
	lui  a0, %hi(my_var + 16)
	lw   a0, %lo(my_var + 16)(a0)

sw_low_model:
	# Store value to a symbol
	lui  a0, %hi(my_var + 16)
	sw   a1, %lo(my_var + 16)(a0)

add_low_model:
	# Calculate address
	lui  a0, %hi(my_var + 16)
	addi a0, a0, %lo(my_var + 16)
	
lw_any_model:	
	# Load value from a symbol
0:	auipc	a0, %pcrel_hi(my_var + 16)
	lw	a0, %pcrel_lo(0b)(a0)

sw_any_model:
	# Store value to a symbol
0:	auipc	a0, %pcrel_hi(my_var + 16)
	sw	a1, %pcrel_lo(0b)(a0)

add_any_model:
	# Calculate address
0:	auipc  a0, %pcrel_hi(my_var + 16)
	addi a0, a0, %pcrel_lo(0b)

call:	
	call 8
	call func
	call func + 8 
	call glbfunc + 8 

tail:	
	tail 8
	tail func
	tail func + 8 
	tail glbfunc + 8 

la:	la t0, 8000
	la t0, func
	la t0, func + 8000
	la t0, glbfunc + 8000
	la t0, my_var + 4

pseudo_store:
	sw a1, my_var + 16, a0
pseudo_load:
	lw a0, my_var + 16

li:
	li t0, 8 # lo12
	li t0, -99 #lo12
	li t0, 0xABC0000 # hi20
	li t0, 1555 # hi20(zeros) + hi12(negative)
	li t0, 8000 # hi20 + lo12(negative)
	li t0, 0xF000A # hi20 + lo12

pseudo_branch:
	beqz x25, func
	bnez x25, func
	blez x25, func
	bgez x25, func
	bltz x25, func
	bgtz x25, func

	bgt  x25, x26, func
	ble  x25, x26, func
	bgtu x25, x26, func
	bleu x25, x26, func

pseudo_jumps:	
	j func + 8000
	jr t0
	ret
	
pseudo_other:
	mv s0, s1
	not s0, s1
	neg s0, s1
	seqz s0, s1
	snez s0, s1
	sltz s0, s1
	sgtz s0, s1

muldiv:
	mul s0, s1, s2
	mulh s0, s1, s2
	mulhsu s0, s1, s2
	div s0, s1, s2
	divu s0, s1, s2
	rem s0, s1, s2
	remu s0, s1, s2
fence:
	fence rw, iorw
	sfence.vma s0, s4
	fence.i
csr:	
	csrrw t0, cycle, s0
	csrrc t0, time, s0
	csrrs t0, instret, s0

	csrrw t0, cycleh, s0
	csrrc t0, timeh, s0
	csrrs t0, instreth, s0
syscalls:
	ecall
	ebreak
	sret
	mret
	wfi
	## mnret

pseudo_csr:
	rdinstret t0
	rdinstreth t0
	rdcycle t0
	rdcycleh t0
	rdtime t0
	rdtimeh t0

	csrr t0, 0x0C80
	csrw timeh, t0
	csrs cycle, t0
	csrc 0x0C01, t0

	csrwi 0x0C01, 9
	csrsi time, 3
	csrci cycle, 1

macro_call:
	call_sbi_print

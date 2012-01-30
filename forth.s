#
#
#
		.data

data_stack:
		.space	65536

# Terminal input buffer
tib_buf:
		.space	80

tmp_buf:
		.space	256

nl:
		.asciiz "\n"
space:
		.asciiz " "
done_msg:
		.asciiz	"Done\n"
stack_empty:
		.asciiz	"\n?STACK\n"
digit_to_char:
		.ascii	"0123456789abcdefghijklmnopqrstuvwxyz"

#------------------------------------------------------------------------------
# Forth vocabulary
#
d_forth_system:
		.byte	12
		.ascii	"forth-system"

		.align	2
		.word	d_query
		.word	c_forth

d_query:
		.byte	5
		.ascii	"query"

		.align	2
		.word	d_tib
		.word	c_query

d_tib:
		.byte	3
		.ascii	"tib"

		.align	2
		.word	d_create_hash
		.word	c_tib

d_create_hash:
		.byte	7
		.ascii	"create#"

		.align	2
		.word	d_lit
		.word	c_create_hash

d_lit:
		.byte	3
		.ascii	"lit"
		
		.align	2
		.word	d_expect
		.word	c_lit

d_expect:
		.byte	6
		.ascii	"expect"

		.align	2
		.word	d_interpret
		.word	c_expect

d_interpret:
		.byte	9
		.ascii	"interpret"

		.align	2
		.word	d_dup
		.word	c_interpret

d_dup:
		.byte	3
		.ascii	"dup"

		.align	2
		.word	d_more_r
		.word	c_dup
		
d_more_r:
		.byte	2
		.ascii	">r"

		.align	2
		.word	d_int_expect
		.word	c_more_r

d_int_expect:
		.byte	8
		.ascii	"(expect)"

		.align	2
		.word	d_at
		.word	c_int_expect

d_at:
		.byte	1
		.ascii	"@"

		.align	2
		.word	d_span
		.word	c_at

d_span:
		.byte	4
		.ascii	"span"

		.align	2
		.word	d_set
		.word	c_span

d_set:
		.byte	1
		.ascii	"!"

		.align	2
		.word	d_type
		.word	c_set

d_type:
		.byte	4
		.ascii	"type"

		.align	2
		.word	d_r_more
		.word	c_type

d_r_more:
		.byte	2
		.ascii	"r>"

		.align	2
		.word	d_minus
		.word	c_r_more

d_minus:
		.byte	1
		.ascii	"-"

		.align	2
		.word	d_space
		.word	c_minus

d_space:
		.byte	5
		.ascii	"space"

		.align	2
		.word	d_more_in
		.word	c_space

d_more_in:
		.byte	3
		.ascii	">in"

		.align	2
		.word	d_0_set
		.word	c_more_in

d_0_set:
		.byte	2
		.ascii	"0!"

		.align	2
		.word	d_blk
		.word	c_0_set

d_blk:
		.byte	3
		.ascii	"blk"

		.align	2
		.word	d_hash_tib
		.word	c_blk

d_hash_tib:
		.byte	4
		.ascii	"#tib"

		.align	2
		.word	d_minus_find
		.word	c_hash_tib

d_minus_find:
		.byte	5
		.ascii	"-find"

		.align	2
		.word	d_bl
		.word	c_minus_find

d_bl:
		.byte	2
		.ascii	"bl"

		.align	2
		.word	d_plus
		.word	c_bl

d_plus:
		.byte	1
		.ascii	"+"

		.align	2
		.word	d_swap
		.word	c_plus

d_swap:
		.byte	4
		.ascii	"swap"

		.align	2
		.word	d_enclose
		.word	c_swap

d_enclose:
		.byte	7
		.ascii	"enclose"

		.align	2
		.word	d_plus_set
		.word	c_enclose

d_plus_set:
		.byte	2
		.ascii	"+!"

		.align	2
		.word	d_here
		.word	c_plus_set

d_here:
		.byte	4
		.ascii	"here"

		.align	2
		.word	d_over
		.word	c_here

d_over:
		.byte	4
		.ascii	"over"

		.align	2
		.word	d_alignw
		.word	c_over

d_alignw:
		.byte	6
		.ascii	"alignw"

		.align	2
		.word	d_align
		.word	c_alignw

d_align:
		.byte	5
		.ascii	"align"
		
		.align	2
		.word	d_inc
		.word	c_align

d_inc:
		.byte	2
		.ascii	"1+"

		.align	2
		.word	d_r_at
		.word	c_inc

d_r_at:
		.byte	2
		.ascii	"r@"

		.align	2
		.word	d_cmove
		.word	c_r_at

d_cmove:
		.byte	5
		.ascii	"cmove"

		.align	2
		.word	d_allot
		.word	c_cmove

d_allot:
		.byte	5
		.ascii	"allot"

		.align	2
		.word	d_2_minus
		.word	c_allot

d_2_minus:
		.byte	2
		.ascii	"2-"

		.align	2
		.word	d_c_set
		.word	c_2_minus

d_c_set:
		.byte	2
		.ascii	"c!"

		.align	2
		.word	d_dp_set
		.word	c_c_set

d_dp_set:
		.byte	3
		.ascii	"dp!"

		.align	2
		.word	d_find
		.word	c_dp_set

d_find:
		.byte	4
		.ascii	"find"

		.align	2
		.word	d_int_find
		.word	c_find

d_int_find:
		.byte	6
		.ascii	"(find)"

		.align	2
		.word	d_q_dup
		.word	c_int_find

d_q_dup:
		.byte	4
		.ascii	"?dup"

		.align	2
		.word	d_execute
		.word	c_q_dup

d_execute:
		.byte	7
		.ascii	"execute"

		.align	2
		.word	d_dot
		.word	c_execute

d_dot:
		.byte	1
		.ascii	"."

		.align	2
		.word	d_drop
		.word	c_dot

d_drop:
		.byte	4
		.ascii	"drop"

		.align	2
		.word	d_rot
		.word	c_drop

d_rot:
		.byte	3
		.ascii	"rot"

		.align	2
		.word	d_and
		.word	c_rot
	
d_and:
		.byte	3
		.ascii	"and"

		.align	2
		.word	d_empty
		.word	c_and

d_empty:
		.byte	0
		# no chars

		.align	2
		.word	d_cr
		.word	c_empty

d_cr:
		.byte	2
		.ascii	"cr"

		.align	2
		.word	d_count
		.word	c_cr

d_count:
		.byte	5
		.ascii	"count"

		.align	2
		.word	d_c_at
		.word	c_count

d_c_at:
		.byte	2
		.ascii	"c@"

		.align	2
		.word	d_equal
		.word	c_c_at

d_equal:
		.byte	1
		.ascii	"="

		.align	2
		.word	d_not_equal
		.word	c_equal

d_not_equal:
		.byte	2
		.ascii	"<>"

		.align	2
		.word	d_less
		.word	c_not_equal

d_less:
		.byte	1
		.ascii	"<"

		.align	2
		.word	d_dpl
		.word	c_less

d_dpl:
		.byte	3
		.ascii	"dpl"

		.align	2
		.word	d_base
		.word	c_dpl

d_base:
		.byte	4
		.ascii	"base"

		.align	2
		.word	d_digit
		.word	c_base

d_digit:
		.byte	5
		.ascii	"digit"

		.align	2
		.word	d_int_do
		.word	c_digit

d_int_do:
		.byte	4
		.ascii	"(do)"

		.align	2
		.word	d_i
		.word	c_int_do

d_i:
		.byte	1
		.ascii	"i"

		.align	2
		.word	d_alpha
		.word	c_i

d_alpha:
		.byte	5
		.ascii	"alpha"

		.align	2
		.word	d_int_loop
		.word	c_alpha

d_int_loop:
		.byte	6
		.ascii	"(loop)"

		.align	2
		.word	d_2_drop
		.word	c_int_loop

d_2_drop:
		.byte	5
		.ascii	"2drop"

		.align	2
		.word	d_rdrop
		.word	c_2_drop

d_rdrop:
		.byte	5
		.ascii	"rdrop"

		.align	2
		.word	d_um_star
		.word	c_rdrop

d_um_star:
		.byte	3
		.ascii	"um*"

		.align	2
		.word	d_d_plus
		.word	c_um_star

d_d_plus:
		.byte	2
		.ascii	"d+"

		.align	2
		.word	d_literal
		.word	c_d_plus

d_literal:
		.byte	135
		.ascii	"literal"

		.align	2
		.word	d_state
		.word	c_literal

d_state:
		.byte	5
		.ascii	"state"

		.align	2
		.word	0
		.word	c_state

init_here:
		.space	65536

#==============================================================================
#
		.text
main:
		la		$fp, data_stack
		j		c_forth

next:
		lw		$ra, 0($sp)
		addu	$sp, $sp, 4
		jr		$ra

# : FORTH ( ->) BEGIN QUERY INTERPRET  AGAIN ;
c_forth:
		jal		c_query
		jal		c_interpret
		j		c_forth

# : QUERY ( ->) TIB 80 EXPECT >IN 0! BLK 0! SPAN @ #TIB ! ;
c_query:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_tib
		jal		c_lit
		.word	80
		jal		c_expect
		jal		c_more_in	# >IN
		jal		c_0_set		# 0!
		jal		c_blk
		jal		c_0_set
		jal		c_span
		jal		c_at		# @
		jal		c_hash_tib	# #TIB
		jal		c_set		# !
		j		next

const_does:
		jal		c_at
		j		next
		
# TIB
c_tib:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	const_does
		.word	tib_buf

# CREATE#
#
# This is what is compiled on stack by create
#
c_create_hash:
		# 0($ra)		DOES> code
		# $ra+4			PFA

		move	$t0, $ra
		addu	$t0, $t0, 4

		sw		$t0, 0($fp)
		addu	$fp, $fp, 4

		lw		$t0, ($ra)
		jr		$t0

# LIT
#
# Puts on stack the constant from PFA
#
c_lit:
		move	$t0, $ra
		lw		$t0, 0($ra)

		sw		$t0, 0($fp)
		addu	$fp, $fp, 4

		addu	$ra, $ra, 4
		jr 		$ra

# : EXPECT  ( A,+N->)  DUP >R (EXPECT) DUP SPAN !
#    TYPE R> SPAN @ - IF SPACE THEN ;
c_expect:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_dup
		jal		c_more_r
		jal		c_int_expect
		jal		c_dup
		jal		c_span
		jal		c_set
		jal		c_type
		jal		c_r_more
		jal		c_span
		jal		c_at
		jal		c_minus
		jal		c_if_branch
		.word	next
		jal		c_space
		j	next

# : INTERPRET ( ->) BEGIN -FIND ?DUP IF
#   1+ IF EXECUTE ELSE STATE @ IF , ELSE EXECUTE THEN  THEN
#   ELSE NUMBER DPL @ 1+ IF [COMPILE] 2LITERAL
#    ELSE DROP [COMPILE] LITERAL THEN THEN ?STACK AGAIN ;
#
single_word:
		jal		c_drop			# DROP
		jal		c_literal		# LITERAL

check_stack:
		# jal		c_q_stack
		j		next_word

state_execute:
		jal		c_execute		# EXECUTE
		j		check_stack

normal_word:
		jal		c_state			# STATE
		jal		c_at			# @
		jal		c_if_branch		# IF
		.word	state_execute

		jal		c_comma			# ,
		j		check_stack		# ELSE

maybe_number:
		jal		c_number		# NUMBER
		jal		c_dpl			# DPL
		jal		c_at			# @
		jal		c_inc			# 1+
		jal		c_if_branch		# IF
		.word	single_word

		# double word
		jal		c_2_literal		# 2LITERAL
		j		check_stack

c_interpret:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)
next_word:
		jal		c_minus_find	# -FIND
		jal		c_q_dup			# ?DUP
		jal		c_if_branch		# IF
		.word	maybe_number

		jal		c_inc			# 1+
		jal		c_if_branch		# IF
		.word	normal_word

		# immediate word
		jal		c_execute		# EXECUTE
		j		check_stack		# ELSE

# DUP
c_dup:
		la		$t0, data_stack
		bleu	$fp, $t0, abort_data_stack

		lw		$t0, -4($fp)
		sw		$t0, 0($fp)
		addu	$fp, $fp, 4
		jr		$ra

# >R
c_more_r:
		la		$t0, data_stack
		bleu	$fp, $t0, abort_data_stack

		subu	$fp, $fp, 4
		lw		$t0, 0($fp)

		subu	$sp, $sp, 4
		sw		$t0, 0($sp)
		jr		$ra

# (EXPECT)
c_int_expect:
		la		$t0, data_stack
		addu	$t0, $t0, 8
		bltu	$fp, $t0, abort_data_stack

		lw		$a1, -4($fp)
		lw		$a0, -8($fp)

		li		$v0, 8
		syscall

		# the returned string must be null-terminated
		lw		$t0, -8($fp)

c_int_expect_1:
		lbu		$t1, 0($t0)
		beqz	$t1, c_int_expect_2

		addiu	$t0, $t0, 1
		j c_int_expect_1

c_int_expect_2:
		lw		$t1, -8($fp)
		subu	$t0, $t0, $t1

		sw		$t0, -4($fp)
		jr		$ra

# @
c_at:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lw		$t0, ($t0)
		sw		$t0, -4($fp)
		jr		$ra

# SPAN		
c_span:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	0

# !
c_set:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# A
		lw		$t1, -8($fp)	# N

		sw		$t1, ($t0)
		subu	$fp, $fp, 8
		jr		$ra

# TYPE
c_type:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t1, -4($fp)	# N
		lw		$t0, -8($fp)	# A
		subu	$fp, $fp, 8

		# copy the string to temp buffer to be able
		# to nul-terminate it before printing

		la		$t2, tmp_buf
c_type_2:
		beqz	$t1, c_type_1
		lbu		$a0, ($t0)
		sb		$a0, ($t2)
		addu	$t0, $t0, 1
		addu	$t2, $t2, 1
		subu	$t1, $t1, 1
		j c_type_2
c_type_1:
		sb		$zero, ($t2)	# nul terminate

		la		$a0, tmp_buf
		li		$v0, 4
		syscall
		jr $ra

# r>		
c_r_more:
		lw		$t0, ($sp)
		addu	$sp, $sp, 4

		addu	$fp, $fp, 4
		sw		$t0, -4($fp)
		jr		$ra

# -
c_minus:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# B
		lw		$t1, -8($fp)	# A
		sub		$t1, $t1, $t0

		subu	$fp, $fp, 4
		sw		$t1, -4($fp)
		jr		$ra

# ?BRANCH
c_if_branch:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# bool
		subu	$fp, $fp, 4

		beqz	$t0, c_if_branch_1

		addu	$ra, $ra, 4		# skip jump addr
		jr		$ra
c_if_branch_1:
		lw		$t0, ($ra)
		jr		$t0

# SPACE
c_space:
		la		$a0, space
		li		$v0, 4
		syscall
		jr		$ra

# >IN		
c_more_in:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	0

# 0!
c_0_set:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# bool
		subu	$fp, $fp, 4

		sw		$zero, ($t0)
		jr		$ra

# BLK
c_blk:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	0

# #TIB
c_hash_tib:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	0

# : -FIND ( ->A,N)   BL WORD FIND ;
c_minus_find:
		subu	$sp, $sp, 4
		sw		$ra, ($sp)

		jal		c_bl
		jal		c_word
		jal		c_find
		j		next

# 32 CONSTANT BL
c_bl:
		subu	$sp, $sp, 4
		sw		$ra, ($sp)

		jal		c_create_hash
		.word	const_does
		.word	32

# : WORD   ( C->T )   BLK @ IF BLK @ BLOCK ELSE TIB THEN
#      >IN @ + SWAP ENCLOSE >IN +!
#      HERE >R OVER - >R + ALIGNH HERE 1+ R@ CMOVE
#      HERE R> 1+ ALLOT ALIGNH HERE OVER - ( 2- ) OVER C! R> DP! ;
# 
# : WORD   ( C->T )   BLK @ IF BLK @ BLOCK ELSE TIB THEN
#      >IN @ + SWAP ENCLOSE >IN +!
#	( A,N1,N2 )
#		OVER - >R +
#	( A1 )
#		HERE 1+ R@
#	( A1,H+1,S )
#		CMOVE
#	( )
#	R> HERE C! HERE ;
#	( T )
c_word_1:
		jal		c_tib
c_word_2:
		jal		c_more_in
		jal		c_at
		jal		c_plus
		jal		c_swap
		jal		c_enclose
		jal		c_more_in
		jal		c_plus_set

		jal		c_over
		jal		c_minus
		jal		c_more_r
		jal		c_plus

		jal		c_here
		jal		c_inc
		jal		c_r_at

		jal		c_cmove

		jal		c_r_more
		jal		c_here
		jal		c_c_set
		jal		c_here
		j		next		
	
c_word:
		subu	$sp, $sp, 4
		sw		$ra, ($sp)

		jal		c_blk
		jal		c_at
		jal		c_if_branch
		.word	c_word_1

		jal		c_blk
		jal		c_at
		jal		c_block
		j		c_word_2

# +
c_plus:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# B
		lw		$t1, -8($fp)	# A
		add		$t1, $t1, $t0

		subu	$fp, 4
		sw		$t1, -4($fp)
		jr		$ra

# swap
c_swap:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lw		$t1, -8($fp)
		sw		$t1, -4($fp)
		sw		$t0, -8($fp)
		jr		$ra

# ENCLOSE
c_enclose:
		la		$t1, data_stack	# c_enclose
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# C
		lw		$t1, -8($fp)	# A

		li		$s0, 0			# N1
		li		$s1, 0			# N2
		li		$s2, 0			# N3

		li		$t7, 10			# eol

c_enclose_2:
		lbu		$a0, ($t1)
		beq		$a0, $t7, c_enclose_eof
		bne		$a0, $t0, c_enclose_1
		addu	$t1, 1
		addu	$s0, 1
		addu	$s1, 1
		addu	$s2, 1
		j		c_enclose_2
c_enclose_1:
		addu	$t1, 1
		addu	$s1, 1
		addu	$s2, 1
		lbu		$a0, ($t1)
		beq		$a0, $t7, c_enclose_eof
		bne		$a0, $t0, c_enclose_1
c_enclose_3:
		addu	$t1, 1
		addu	$s2, 1
		lbu		$a0, ($t1)
		beq		$a0, $t7, c_enclose_eof
		beq		$a0, $t0, c_enclose_3
c_enclose_eof:
		addu	$fp, 8
		sw		$s2, -4($fp)
		sw		$s1, -8($fp)
		sw		$s0, -12($fp)
		# -16($fp) is still A
		jr		$ra

# +!
c_plus_set:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# A
		lw		$t1, -8($fp)	# W
		subu	$fp, 8

		lw		$a0, 0($t0)
		add		$a0, $a0, $t1
		sw		$a0, 0($t0)
		jr		$ra

# HERE
c_here:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	const_does
here:
		.word	init_here
	
# OVER
c_over:
		la		$t1, data_stack	# c_over
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -8($fp)
		addu	$fp, 4
		sw		$t0, -4($fp)
		jr		$ra
		
# : ALIGNW	( -> ) 4 ALIGN ;
c_alignw:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_lit
		.word	4
		jal		c_align
		j		next

# ALIGN
c_align:
		la		$t1, data_stack	# c_align
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# boundary
		subu	$fp, 4
		subu	$t0, 1

		lw		$a0, here
		addu	$a0, $a0, $t0
		not		$t0, $t0
		and		$a0, $a0, $t0
		sw		$a0, here
		jr		$ra

# : 1+ ( W1 -> W2 ) 1 + ;
c_inc:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_lit
		.word	1
		jal		c_plus
		j		next

# r@
c_r_at:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, 0($sp)
		addu	$fp, 4
		sw		$t0, -4($fp)
		jr		$ra
				
# cmove
c_cmove:
		la		$t1, data_stack	# c_cmove
		addu	$t1, 12
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# U
		lw		$t1, -8($fp)	# A2
		lw		$t2, -12($fp)	# A1
		subu	$fp, 12
c_cmove_2:
		beqz	$t0, c_cmove_1
		lbu		$a0, ($t2)
		sb		$a0, ($t1)
		addu	$t1, 1
		addu	$t2, 1
		subu	$t0, 1
		j		c_cmove_2
c_cmove_1:
		jr		$ra

# ALLOT
c_allot:
		la		$t1, data_stack	# c_allot
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# N
		subu	$fp, 4

		lw		$a0, here
		addu	$a0, $a0, $t0
		sw		$a0, here
		jr		$ra

# : 2- 2 - ;
c_2_minus:
		subu	$sp, $sp, 4
		sw		$ra, 0($sp)

		jal		c_lit
		.word	2
		jal		c_minus
		j		next	

# C!
c_c_set:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# A
		lw		$t1, -8($fp)	# C
		subu	$fp, 8

		sb		$t1, 0($t0)
		jr		$ra

# DP!
c_dp_set:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		subu	$fp, 4
		sw		$t0, here
		jr		$ra

# : FIND  ( T->A,N)
#    DUP >R -1   LIT [ FORTH# ] @
#    CURRENT @ @ 2DUP = IF DROP THEN
#    CONTEXT @ @ 2DUP = IF DROP THEN
#    R> (FIND)   DUP IF
#    DROP ROT DROP [ &IFLAG ] LITERAL AND IF 1 ELSE -1 THEN
#    THEN ;
#
c_find_2:
		jal		c_lit
		.word	1
		j		next

c_find:
		subu	$sp, $sp, 4	# c_find
		sw		$ra, 0($sp)

		jal		c_dup
		jal		c_more_r
		jal		c_lit
		.word	-1
		jal		c_lit
		.word	d_forth_system

		jal		c_r_more
		jal		c_int_find
		jal		c_dup
		jal		c_if_branch
		.word	next

		jal		c_drop
		jal		c_rot
		jal		c_drop
		jal		c_lit
		.word	128			# immediate flag mask
		jal		c_and
		jal		c_if_branch
		.word	c_find_2

		jal		c_lit
		.word	-1
		j		next

# : (FIND) ( -1,AN,,,A1,T->CFA,C,TF/FF )
c_int_find:
		la		$t1, data_stack	# c_int_find
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$s0, -4($fp)	# T
		subu	$fp, 4

c_int_next_list:
		lw		$t0, -4($fp)	# A
		subu	$fp, 4

		li		$a0, -1
		beq		$t0, $a0, c_int_find_not_found

c_int_find_check_word:
		lbu		$a0, 0($s0)
		lbu		$a1, 0($t0)
		andi	$a1, 31
		bne		$a0, $a1, c_int_find_next_word

		move	$t1, $s0
		move	$t2, $t0
		move	$t3, $a0		# count

c_int_find_1:
		beqz	$t3, c_int_find_found
		addu	$t1, 1
		addu	$t2, 1
		subu	$t3, 1
		lbu		$a0, 0($t1)
		lbu		$a1, 0($t2)
		beq		$a0, $a1, c_int_find_1

c_int_find_next_word:
		lbu		$a0, 0($t0)
		andi	$a0, 31
		addu	$t0, 1
		addu	$t0, $t0, $a0

		addu	$t0, 3
		and		$t0, 0xfffffffc

		lw		$t0, 0($t0)
		bnez	$t0, c_int_find_check_word

		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack
		j		c_int_next_list

c_int_find_not_found:
		addu	$fp, 4
		sw		$zero, -4($fp)
		jr		$ra

c_int_find_found:
		# drop all lists until -1
		li		$t7, -1
c_int_find_drop_more:
		lw		$a0, -4($fp)
		subu	$fp, 4
		bne		$a0, $t7, c_int_find_drop_more

		lbu		$v0, 0($t0)		# length and flags
		andi	$a0, $v0, 31
		
		addu	$t0, 1
		addu	$t0, $t0, $a0

		addu	$t0, 3
		and		$t0, 0xfffffffc

		addu	$t0, 4			# skip next word link
		lw		$v1, 0($t0)		# cfa

		li		$a0, 1			# true

		addu	$fp, 12
		sw		$v1, -12($fp)
		sw		$v0, -8($fp)
		sw		$a0, -4($fp)
		jr		$ra

# ?DUP
c_q_dup:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		beqz	$t0, c_q_dup_1

		addu	$fp, 4
		sw		$t0, -4($fp)
c_q_dup_1:
		jr		$ra

# EXECUTE
c_execute:
		la		$t1, data_stack	# c_execute
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		subu	$fp, 4
		jr		$t0

# .
c_dot:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$a0, -4($fp)
		subu	$fp, 4

		li		$v0, 1
		syscall
		jr		$ra

# DROP
c_drop:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		subu	$fp, 4
		jr		$ra

# ROT
c_rot:
		la		$t1, data_stack
		addu	$t1, 12
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lw		$t1, -8($fp)
		lw		$t2, -12($fp)

		sw		$t2, -4($fp)
		sw		$t0, -8($fp)
		sw		$t1, -12($fp)
		jr		$ra

# AND
c_and:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lw		$t1, -8($fp)
		and		$t0, $t0, $t1

		sub		$fp, 4
		sw		$t0, -4($fp)
		jr		$ra

# ( empty word; exits interpret )
c_empty:
		# frame not created on purpose
		jal		c_cr
		j		next

# CR		
c_cr:
		la		$a0, nl
		li		$v0, 4
		syscall
		jr		$ra

# : NUMBER ( T->WD )
#    0 0 ROT DUP >R COUNT OVER + OVER C@ C" - =
#    DUP >R SWAP >R IF ELSE 1- THEN -1
#      BEGIN DPL ! CONVERT DUP R@ < WHILE DUP C@
#      C" . <> IF RDROP RDROP R> BADWORD THEN 0
#      REPEAT  DROP RDROP R>  IF DNEGATE THEN RDROP ;
c_number_positive:
		jal		c_rdrop
		j		next

c_number_break:
		jal		c_drop
		jal		c_rdrop
		jal		c_r_more
		jal		c_if_branch
		.word	c_number_positive
		jal		c_dnegate
		jal		c_rdrop
		j		next

c_number_1:
		jal		c_dec
		j		c_number_2

c_number_3:
		jal		c_lit
		.word	0
		j		c_number_loop

c_number:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_lit
		.word	0
		jal		c_lit
		.word	0
		jal		c_rot
		jal		c_dup
		jal		c_more_r
		jal		c_count
		jal		c_over
		jal		c_plus
		jal		c_over
		jal		c_c_at
		jal		c_lit
		.word	45			# -
		jal		c_equal

		jal		c_dup
		jal		c_more_r
		jal		c_swap
		jal		c_more_r
		jal		c_if_branch
		.word	c_number_1
c_number_2:
		jal		c_lit
		.word	-1
c_number_loop:
		jal		c_dpl
		jal		c_set
		jal		c_convert
		jal		c_dup
		jal		c_r_at
		jal		c_less
		jal		c_if_branch
		.word	c_number_break
		jal		c_dup
		jal		c_c_at

		jal		c_lit
		.word	46			# .
		jal		c_not_equal
		jal		c_if_branch
		.word	c_number_3

		jal		c_rdrop
		jal		c_rdrop
		jal		c_r_more
		jal		c_badword

# : COUNT ( T -> A,N ) DUP 1+ SWAP C@ ;
c_count:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_dup
		jal		c_inc
		jal		c_swap
		jal		c_c_at
		j		next

# C@
c_c_at:
		la		$t1, data_stack
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lb		$t0, 0($t0)
		sw		$t0, -4($fp)
		jr		$ra

# =
c_equal:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# B
		lw		$t1, -8($fp)	# A
		subu	$fp, 4

		seq		$a0, $t1, $t0
		sw		$a0, -4($fp)
		jr		$ra

# <>
c_not_equal:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# B
		lw		$t1, -8($fp)	# A
		subu	$fp, 4

		sne		$a0, $t1, $t0
		sw		$a0, -4($fp)
		jr		$ra

# <
c_less:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# B
		lw		$t1, -8($fp)	# A
		subu	$fp, 4

		slt		$a0, $t1, $t0
		sw		$a0, -4($fp)
		jr		$ra

# : 1- 1 - ;
c_dec:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_lit
		.word	1
		jal		c_minus
		j		next
		
# VARIABLE DPL
c_dpl:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	0

# : CONVERT  ( WD1,A1->WD2,A2)
#    BEGIN 1+ DUP >R C@ BASE @ DIGIT WHILE
#      SWAP BASE @ UM* DROP ROT BASE @ UM* D+
#      DPL @ 1+ IF DPL 1+! THEN R> REPEAT R> ;
c_convert_break:
		jal		c_r_more
		j		next

c_convert_1:
		jal		c_r_more
		j		c_convert_loop

c_convert:
		subu	$sp, 4		# c_convert
		sw		$ra, 0($sp)

c_convert_loop:
		jal		c_inc
		jal		c_dup
		jal		c_more_r
		jal		c_c_at
		jal		c_base
		jal		c_at
		jal		c_digit
		jal		c_if_branch
		.word	c_convert_break

		jal		c_swap
		jal		c_base
		jal		c_at
		jal		c_um_star
		jal		c_drop
		jal		c_rot
		jal		c_base
		jal		c_at
		jal		c_um_star
		jal		c_d_plus

		jal		c_dpl
		jal		c_at
		jal		c_inc
		jal		c_if_branch
		.word	c_convert_1

		jal		c_dpl
		jal		c_inc_set
		jal		c_r_more
		j		c_convert_loop

# VARIABLE BASE
c_base:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	10

# : DIGIT ( C,N1->N2,TF/FF)  0 ROT ROT 0
#   DO I ALPHA OVER = IF 2DROP I -1 0 LEAVE THEN LOOP DROP ;

c_digit_0:
		j		c_digit_1

c_digit:
		subu	$sp, 4		# c_digit
		sw		$ra, 0($sp)

		jal		c_lit
		.word	0
		jal		c_rot
		jal		c_rot
		jal		c_lit
		.word	0
		jal		c_int_do
c_digit_loop:
		jal		c_i
		jal		c_alpha
		jal		c_over
		jal		c_equal
		jal		c_if_branch
		.word	c_digit_0

		jal		c_2_drop
		jal		c_i
		jal		c_lit
		.word	-1
		jal		c_lit
		.word	0

#
# XXX: how leave is implemented?
#
		jal		c_rdrop
		jal		c_rdrop
		j		c_digit_break

c_digit_1:
		jal		c_int_loop
		.word	c_digit_loop
c_digit_break:
		jal		c_drop
		j		next

# (DO)
c_int_do:
		la		$t1, data_stack
		add		$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# index
		lw		$t1, -8($fp)	# limit
		subu	$fp, 8

		subu	$sp, 8
		sw		$t0, 0($sp)
		sw		$t1, 4($sp)
		jr		$ra

# I
c_i:
		j		c_r_at

# ALPHA
c_alpha:
		la		$t1, data_stack		# c_alpha
		bleu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lbu		$t0, digit_to_char($t0)
		sw		$t0, -4($fp)
		jr		$ra

# (LOOP)
c_int_loop:
		lw		$t0, 0($sp)		# index
		lw		$t1, 4($sp)		# limit

		add		$t0, 1
		bge		$t0, $t1, c_int_loop_break

		sw		$t0, 0($sp)
		
		lw		$s0, 0($ra)
		jr		$s0

c_int_loop_break:
		addu	$sp, 8
		jr		$ra

# : 2DROP DROP DROP ;
c_2_drop:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_drop
		jal		c_drop
		j		next

# RDROP
c_rdrop:
		addu	$sp, 4
		jr		$ra

# UM*
c_um_star:
		la		$t1, data_stack
		addu	$t1, 8
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)
		lw		$t1, -8($fp)
		multu	$t0, $t1
		mfhi	$t0		
		mflo	$t1
		sw		$t0, -4($fp)
		sw		$t1, -8($fp)
		jr		$ra

# D+
c_d_plus:
		la		$t1, data_stack		# D+
		addu	$t1, 16
		bltu	$fp, $t1, abort_data_stack

		lw		$t0, -4($fp)	# BH
		lw		$t1, -8($fp)	# BL
		lw		$t2, -12($fp)	# AH
		lw		$t3, -16($fp)	# AL
		subu	$fp, 8

#
# XXX: integer overflow ignored
#
		add		$t1, $t1, $t3
		add		$t0, $t0, $t2

		sw		$t0, -4($fp)
		sw		$t1, -8($fp)
		jr		$ra

# : LITERAL   ( W->) STATE @ IF COMPILE LIT , THEN ; IMMEDIATE
c_literal:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_state
		jal		c_at
		jal		c_if_branch
		.word	next

		jal		c_compile
		.word	c_lit
		jal		c_comma
		j		next	

# VARIABLE STATE
c_state:
		subu	$sp, 4
		sw		$ra, 0($sp)

		jal		c_create_hash
		.word	next
		.word	0

#------------------------------------------------------------------------------
#
abort:
		# print data stack and exit
		move	$t0, $fp
		la		$t1, data_stack
loop1:
		bleu	$t0, $t1, cont1 
		subu	$t0, 4
		lw		$a0, 0($t0)

		li		$v0, 1
		syscall

		la		$a0, nl
		li		$v0, 4
		syscall

		j		loop1

cont1:
		la		$a0, done_msg
		li		$v0, 4
		syscall

		li		$v0, 10
		syscall

abort_data_stack:
		la		$a0, stack_empty
		li		$v0, 4
		syscall

		li		$v0, 10
		syscall
		
#EOF

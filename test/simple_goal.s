	.text
	.file	"simple_goal.ll"
	.globl	i64ToString                     # -- Begin function i64ToString
	.p2align	4, 0x90
	.type	i64ToString,@function
i64ToString:                            # @i64ToString
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%r13
	.cfi_def_cfa_offset 40
	pushq	%r12
	.cfi_def_cfa_offset 48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	subq	$40, %rsp
	.cfi_def_cfa_offset 96
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	movabsq	$7810763617093968238, %rax      # imm = 0x6C656820656E616E
	movq	%rax, 22(%rsp)
	movabsq	$7017206772232710740, %rax      # imm = 0x616220616E656A54
	movq	%rax, 14(%rsp)
	movw	$10, 34(%rsp)
	movl	$1634955116, 30(%rsp)           # imm = 0x61736F6C
	movl	$0, 36(%rsp)
	leaq	14(%rsp), %rax
	testq	%rsi, %rsi
	js	.LBB0_1
# %bb.2:                                # %negative_check_false
	movb	$43, (%rax)
	jmp	.LBB0_3
.LBB0_1:                                # %negative_check_true
	movb	$45, (%rax)
.LBB0_3:                                # %negative_check_done
	movabsq	$7378697629483820647, %r13      # imm = 0x6666666666666667
	leaq	14(%rsp), %r15
	.p2align	4, 0x90
.LBB0_4:                                # %while_point
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rax
	imulq	%r13
	movq	%rdx, %rbx
	movq	%rdx, %rbp
	shrq	$63, %rbp
	sarq	$2, %rbx
	movq	%r15, %rdi
	callq	puts@PLT
	addq	%rbp, %rbx
	jne	.LBB0_4
# %bb.5:                                # %while_break
	movq	14(%rsp), %rax
	movq	22(%rsp), %rcx
	movl	30(%rsp), %edx
	movzwl	34(%rsp), %esi
	movw	%si, 20(%r14)
	movl	%edx, 16(%r14)
	movq	%rcx, 8(%r14)
	movq	%rax, (%r14)
	movq	%r14, %rax
	addq	$40, %rsp
	.cfi_def_cfa_offset 56
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%r12
	.cfi_def_cfa_offset 40
	popq	%r13
	.cfi_def_cfa_offset 32
	popq	%r14
	.cfi_def_cfa_offset 24
	popq	%r15
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	i64ToString, .Lfunc_end0-i64ToString
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	leaq	12(%rsp), %rdi
	movl	$200, %esi
	callq	i64ToString@PLT
	movzwl	32(%rsp), %eax
	movl	28(%rsp), %ecx
	movq	12(%rsp), %rdx
	movq	20(%rsp), %rsi
	movq	%rdx, 34(%rsp)
	movq	%rsi, 42(%rsp)
	movl	%ecx, 50(%rsp)
	movw	%ax, 54(%rsp)
	leaq	34(%rsp), %rdi
	callq	puts@PLT
	xorl	%eax, %eax
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits

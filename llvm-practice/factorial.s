	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15
	.globl	_factorial                      ## -- Begin function factorial
	.p2align	4, 0x90
_factorial:                             ## @factorial
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	testl	%edi, %edi
	je	LBB0_2
## %bb.1:                               ## %recurse
	movl	%edi, %ebx
	leal	-1(%rbx), %edi
	callq	_factorial
	imull	%ebx, %eax
	popq	%rbx
	retq
LBB0_2:                                 ## %done
	movl	$1, %eax
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$11, %edi
	callq	_factorial
	movl	%eax, %edi
	callq	_print_int
	leaq	_.nl(%rip), %rdi
	callq	_print_string
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__const
	.globl	_.nl                            ## @.nl
_.nl:
	.asciz	"\n"

.subsections_via_symbols

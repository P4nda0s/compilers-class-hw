	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15
	.globl	_add1                           ## -- Begin function add1
	.p2align	4, 0x90
_add1:                                  ## @add1
	.cfi_startproc
## %bb.0:                               ## %entry
                                        ## kill: def $esi killed $esi def $rsi
                                        ## kill: def $edi killed $edi def $rdi
	leal	(%rdi,%rsi), %eax
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
	movl	$3, %edi
	movl	$4, %esi
	callq	_add1
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

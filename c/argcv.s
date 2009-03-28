	.file	"argcv.c"
	.text
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	addl	$1, (%ecx)
	addl	$4, 4(%ecx)
	movl	$0, %eax
	popl	%ecx
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
	.size	main, .-main
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu10) 4.3.2"
	.section	.note.GNU-stack,"",@progbits

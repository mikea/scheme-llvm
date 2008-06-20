

	.text
	.align	4,0x90
	.globl	_main
_main:
	subl	$12, %esp
	movl	$_.LC0, (%esp)
	call	L_puts$stub
	xorl	%eax, %eax
	addl	$12, %esp
	ret
	.cstring
_.LC0:				# .LC0
	.asciz	"hello world\n"
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_puts$stub:
	.indirect_symbol _puts
	hlt ; hlt ; hlt ; hlt ; hlt

	.subsections_via_symbols


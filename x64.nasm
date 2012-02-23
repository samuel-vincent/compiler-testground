
extern printf
segment .data
	
	a dd	100					
	msg db "Result: %i", 0xA
	
segment .bss
		
segment .text
	global main
		
main:
	enter 16,0 ; return address and one local qword
	
	mov rax, [a]
	push rax
	pop rsi
	mov rdi, msg
		
	call printf
	
	leave
	ret

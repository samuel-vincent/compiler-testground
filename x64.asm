
extern printf
segment .data
	
	a dd	100
	msg db "Result: %i", 0xA
		
segment .bss
		
segment .text
	global main
		
main:
	push rbp
	mov rbp, rsp

	mov rax, [a]
	push rax
	pop rsi
	mov rdi, msg
	
	call printf
		
	add rsp, 8
	xor rax, rax
	leave
	ret

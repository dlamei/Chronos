GLOBAL main
EXTERN printf
EXTERN alloc_heap
EXTERN heap_alloc_int
section .data
int_format DB "%d", 0xa, 0x0, 
hex_format DB "%#06x", 0xa, 0x0, 
section .bss
heap_ptr: RESB 0x4
section .text
main:
PUSH EBP
MOV EBP, ESP
SUB ESP, 16
CALL alloc_heap
MOV DWORD [heap_ptr], EAX
PUSH DWORD [heap_ptr]
MOV EAX, 3
PUSH EAX
MOV EAX, 3
PUSH EAX
MOV EAX, 2
POP ECX
MUL ECX
PUSH EAX
MOV EAX, 1
POP ECX
ADD EAX, ECX
POP ECX
MOV EDX, 0
DIV ECX
PUSH EAX
PUSH DWORD [heap_ptr]
CALL heap_alloc_int
ADD ESP, 8
MOV DWORD [EBP-4], EAX
MOV EAX, DWORD [EAX+4]
PUSH EAX
PUSH int_format
CALL printf
MOV EAX, 2
PUSH EAX
MOV EAX, DWORD [EBP-4]
MOV EAX, DWORD [EAX+4]
POP ECX
ADD EAX, ECX
PUSH EAX
PUSH DWORD [heap_ptr]
CALL heap_alloc_int
ADD ESP, 8
MOV DWORD [EBP-8], EAX
MOV EAX, DWORD [EAX+4]
PUSH EAX
PUSH int_format
CALL printf
MOV EAX, DWORD [EBP-4]
PUSH EAX
MOV EAX, DWORD [EBP-8]
MOV EAX, DWORD [EAX+4]
POP ECX
MOV ECX, DWORD [ECX+4]
ADD EAX, ECX
PUSH EAX
PUSH DWORD [heap_ptr]
CALL heap_alloc_int
ADD ESP, 8
MOV DWORD [EBP-12], EAX
MOV EAX, DWORD [EAX+4]
PUSH EAX
PUSH int_format
CALL printf
MOV ESP, EBP
POP EBP
MOV EAX, 1
MOV EBX, 1
INT 128


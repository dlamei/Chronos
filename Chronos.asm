GLOBAL main
EXTERN printf
EXTERN print_float
EXTERN alloc_heap
EXTERN heap_alloc_int
section .data
int_format DB "%d", 0xa, 0x0, 
hex_format DB "%#06x", 0xa, 0x0, 
double_format DB "%f", 0xa, 0x0, 
section .bss
heap_ptr: RESB 0x4
section .text
main:
AND ESP, 0xfffffff8
PUSH EBP
MOV EBP, ESP
SUB ESP, 0xc
CALL alloc_heap
MOV DWORD [heap_ptr], EAX
PUSH 0x2
MOV EAX, DWORD [ESP]
MOV DWORD [EBP-4], EAX
PUSH int_format
CALL printf
PUSH 0x3
MOV EAX, DWORD [ESP]
MOV DWORD [EBP-8], EAX
PUSH int_format
CALL printf
PUSH 0x4
MOV EAX, DWORD [ESP]
MOV DWORD [EBP-12], EAX
PUSH int_format
CALL printf
MOV ESP, EBP
POP EBP
MOV EAX, 0x1
MOV EBX, 0x1
INT 0x80


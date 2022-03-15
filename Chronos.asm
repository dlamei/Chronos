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
PUSH DWORD [heap_ptr]
PUSH 0x2
MOV EAX, DWORD [ESP]
MOV DWORD [EBP-4], EAX
PUSH __float32__(1.100000)
CVTSI2SS XMM0, DWORD [ESP+4]
MOVSS DWORD [ESP+4], XMM0
MOVSS XMM1, DWORD [ESP]
MOVSS XMM0, DWORD [ESP+4]
ADD ESP, 0x8
ADDSS XMM0, XMM1
MOVSS DWORD [ESP-4], XMM0
SUB ESP, 0x4
MOV EAX, DWORD [ESP]
MOV DWORD [EBP-8], EAX
CALL print_float
PUSH DWORD [EBP-8]
CALL print_float
PUSH DWORD [EBP-4]
PUSH int_format
CALL printf
MOV ESP, EBP
POP EBP
MOV EAX, 0x1
MOV EBX, 0x1
INT 0x80


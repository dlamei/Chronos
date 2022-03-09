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
SUB ESP, 0x4
CALL alloc_heap
MOV DWORD [heap_ptr], EAX
PUSH DWORD [heap_ptr]
PUSH __float32__(1.000000)
PUSH __float32__(2.000000)
PUSH __float32__(3.000000)
MOVSS XMM1, DWORD [ESP]
ADD ESP, 0x4
MOVSS XMM0, DWORD [ESP]
ADD ESP, 0x4
MULSS XMM0, XMM1
MOVSS DWORD [ESP-4], XMM0
SUB ESP, 0x4
MOVSS XMM1, DWORD [ESP]
ADD ESP, 0x4
MOVSS XMM0, DWORD [ESP]
ADD ESP, 0x4
ADDSS XMM0, XMM1
MOVSS DWORD [ESP-4], XMM0
SUB ESP, 0x4
PUSH __float32__(2.000000)
MOVSS XMM1, DWORD [ESP]
ADD ESP, 0x4
MOVSS XMM0, DWORD [ESP]
ADD ESP, 0x4
DIVSS XMM0, XMM1
MOVSS DWORD [ESP-4], XMM0
SUB ESP, 0x4
CALL print_float
MOV ESP, EBP
POP EBP
MOV EAX, 0x1
MOV EBX, 0x1
INT 0x80


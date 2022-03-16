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
PUSH 0x1
POP ECX
TEST ECX, ECX
JE .L0
PUSH __float32__(0.000000)
PXOR XMM0, XMM0
ADD ESP, 0x4
UCOMISS XMM0, DWORD [ESP-4]
JE .L0
MOV EAX, 0x1
JMP .L1
.L0:
MOV EAX, 0x0
.L1:
PUSH EAX
PUSH int_format
CALL printf
PUSH __float32__(0.000000)
PXOR XMM0, XMM0
ADD ESP, 0x4
UCOMISS XMM0, DWORD [ESP-4]
JNE .L2
PUSH 0x1
POP ECX
TEST ECX, ECX
JE .L3
.L2:
MOV EAX, 0x1
JMP .L4
.L3:
MOV EAX, 0x0
.L4:
PUSH EAX
PUSH int_format
CALL printf
PUSH __float32__(0.000000)
PXOR XMM0, XMM0
ADD ESP, 0x4
UCOMISS XMM0, DWORD [ESP-4]
JNE .L5
PUSH 0x1
POP ECX
TEST ECX, ECX
JE .L6
.L5:
MOV EAX, 0x1
JMP .L7
.L6:
MOV EAX, 0x0
.L7:
PUSH EAX
POP ECX
TEST ECX, ECX
JE .L8
PUSH __float32__(1.230000)
PXOR XMM0, XMM0
ADD ESP, 0x4
UCOMISS XMM0, DWORD [ESP-4]
JNE .L8
PUSH __float32__(0.001000)
PXOR XMM0, XMM0
ADD ESP, 0x4
UCOMISS XMM0, DWORD [ESP-4]
JE .L9
.L8:
MOV EAX, 0x1
JMP .L10
.L9:
MOV EAX, 0x0
.L10:
PUSH EAX
POP ECX
TEST ECX, ECX
JE .L11
MOV EAX, 0x1
JMP .L12
.L11:
MOV EAX, 0x0
.L12:
PUSH EAX
PUSH int_format
CALL printf
MOV ESP, EBP
POP EBP
MOV EAX, 0x1
MOV EBX, 0x1
INT 0x80


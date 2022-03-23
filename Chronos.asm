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
SUB ESP, 0x0
CALL alloc_heap
MOV DWORD [heap_ptr], EAX

PUSH __float32__(1.3)
PUSH __float32__(1.4)

fld dword [esp+4]
fld dword [esp]
fcomip st0, st1
;fstp st0
setge al
movzx eax, al
push eax

PUSH int_format
CALL printf

MOV ESP, EBP
POP EBP
MOV EAX, 0x1
MOV EBX, 0x1
INT 0x80


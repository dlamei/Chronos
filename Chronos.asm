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
PUSH 0x1
PUSH 0x1
MOV EAX, DWORD [ESP+4]
CMP EAX, DWORD [ESP]
SETE AL
MOVZX EAX, AL
ADD ESP, 0x8
PUSH EAX
PUSH int_format
CALL printf
PUSH __float32__(1.000000)
PUSH __float32__(1.000000)
FLD DWORD [ESP]
FLD DWORD [ESP+4]
FCOMIP ST0, ST1
SETE AL
MOVZX EAX, AL
ADD ESP, 0x8
PUSH EAX
PUSH int_format
CALL printf
PUSH __float32__(1.000000)
PUSH __float32__(2.000000)
FLD DWORD [ESP]
FLD DWORD [ESP+4]
FCOMIP ST0, ST1
SETE AL
MOVZX EAX, AL
ADD ESP, 0x8
PUSH EAX
PUSH int_format
CALL printf
PUSH __float32__(0.990000)
PUSH 0x1
CVTSI2SS XMM0, DWORD [ESP]
MOVSS DWORD [ESP], XMM0
FLD DWORD [ESP]
FLD DWORD [ESP+4]
FCOMIP ST0, ST1
SETE AL
MOVZX EAX, AL
ADD ESP, 0x8
PUSH EAX
PUSH int_format
CALL printf
PUSH __float32__(0.990000)
PUSH __float32__(0.990000)
FLD DWORD [ESP]
FLD DWORD [ESP+4]
FCOMIP ST0, ST1
SETE AL
MOVZX EAX, AL
ADD ESP, 0x8
PUSH EAX
PUSH int_format
CALL printf
MOV ESP, EBP
POP EBP
MOV EAX, 0x1
MOV EBX, 0x1
INT 0x80


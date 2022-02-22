GLOBAL main
EXTERN printf
EXTERN alloc_heap
EXTERN heap_alloc_int
SECTION .data
int_format db "%d", 0xa, 0x0
hex_format db "%#06x", 0xa, 0x0
SECTION .bss
heap_ptr: resb 4
SECTION .text
main:
PUSH EBP
MOV EBP, ESP
SUB ESP, 16
CALL alloc_heap
MOV [heap_ptr], EAX
;begin code:
;
PUSH int_format
CALL printf
;
;end code
MOV ESP, EBP
POP EBP
MOV EAX, 1
MOV EBX, 0
INT 0x80


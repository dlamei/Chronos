GLOBAL main
EXTERN printf
SECTION .data
int_format db "%d", 0xa, 0x0
SECTION .text
main:
PUSH EBP
MOV EBP, ESP
SUB ESP, 16
;begin code:
;
PUSH 3
PUSH 2
POP EAX
POP EBX
MUL EBX
PUSH EAX
PUSH 1
POP EAX
POP EBX
ADD EAX, EBX
PUSH EAX
PUSH int_format
CALL printf
;
;end code
MOV ESP, EBP
POP EBP
MOV EAX, 1
MOV EBX, 0
INT 0x80


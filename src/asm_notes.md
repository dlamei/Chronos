# x86 assmebly notes

## compiling:
nasm -f efl32 main.asm -o main.o\
gcc -m32 main.o -o main

## sections:
**.data** defining constant variables\
**.text** read only data

## datatypes:
db writing a byte\
dw writing a word\
dd writing a 4 bytes

## system exit:

	mov eax, 1
	mov ebx, 0
	int 0x80

## writing to stdout:

	mov eax, 4 		(sys_write system call)
	mov ebx, 1 		(stdout file desc)
	mov ecx, esp 	(pointer to bytes to write)
	mov edx, 4		(num of bytes to write)
	int 0x80		(syscall)

## stack pointer:
**esp** is the stack pointer (goes from low to high)\
**ebp** is the base pointer (holds the value of esp)

## alloc 2 bytes on the stack and write to it:

	sub esp, 2
	mov [esp], byte 'H'
	mov [esp+1], byte 'i'

## calling functions:
push eip to stack\
jump\
ret (pop location and jump to it)


## nested func calls:
push ebp to the stack,\
pop ebp from the stack at the end

	push ebp
	mov ebp, esp
	...
	mov esp, ebp
	pop ebp
	ret
this is necessary because you don't know from where this function is called

## passing arguments:
push arguments to the stack.\
arguments start after epb+8, then in 4 bit steps:

	times2:
		push ebp
		mov ebp, esp

		mov eax, [epb+8]
		add eax, eax

		mov esp, ebp
		pop ebp
		ret

## interaction with c:
### call c function:
	global main
	extern printf
push parameters in reverse order\
you also should remove the arguments after calling the function
### call asm function from c:
	global add42
	add42:
		push ebp
		mov ebp, esp
		mov eax, [ebp+8]
		add eax, 42
		mov esp, ebp
		pop ebp
		ret
and a header file:

	int add42(int x);
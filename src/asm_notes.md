# x86 assmebly notes

## compiling:
nasm -f efl32 main.asm -o main.o\
gcc -m32 main.o -o main

## sections:
**.data** defining constant variables\
**.text** read only data

## registers:
| purpose      | 32-bit | low 16-bits | high / low 8-bits |
|--------------|--------|-------------|-------------------|
|general       |eax     |ax           |ah / al            |
|general       |ecx     |cx           |ch / cl            |
|general       |edx     |dx           |dh / dl            |
|general       |ebx     |bx           |bh / bl            |
|stack ptr     |esp     |sp           |                   |
|frame ptr     |ebp     |bp           |                   |
|src index ptr |esi     |si           |                   |
|dest indx     |edi     |di           |                   |

**eip** location of the current instruction\
**esp** is the stack pointer (goes from low to high)\
**ebp** is the base pointer (holds the value of esp)

## Syscall table (linux):
https://chromium.googlesource.com/chromiumos/docs/+/HEAD/constants/syscalls.md#Calling-Conventions

## Instruction set:
https://en.wikipedia.org/wiki/X86_instruction_listings

## writing data:
db writing a byte\
dw writing a word\
dd writing 4 bytes

## writing to stdout:

	mov eax, 4 		(sys_write system call)
	mov ebx, 1 		(stdout file desc)
	mov ecx, esp 	(pointer to bytes to write)
	mov edx, 4		(num of bytes to write)
	int 0x80		(syscall)

## alloc 2 bytes on the stack and write to it:

	sub esp, 2
	mov [esp], byte 'H'
	mov [esp+1], byte 'i'

## calling functions:
### call:
push eip to the stack\
jump to the label
### ret:
pop location from the stack (should be the location after the call instruction)\
jump to it


## nested func calls:
we need to perserve the stack, because eip was pushed to it\
push ebp to the stack,\
pop from the stack at the end

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
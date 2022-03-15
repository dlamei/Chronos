#include <fstream>
#include <unordered_map>
#include <string>
#include <stack>

#include "Parser.h"
#include "Debug.h"

#define HEADER_SIZE 4
#define PTR_SIZE 4

namespace Chronos
{

	enum class ValueType
	{
		INT_VAL,
		FLOAT_VAL,
		UNKNWN_PTR,
	};

	namespace x86ASM
	{

		enum Section : uint8_t
		{
			DATA = 0,
			BSS,
			TEXT,
			NO_SECTION,
		};

		enum InstType : uint16_t
		{
			PUSH = 0,
			POP,
			NOP,
			CALL,
			MOV,
			ADD,
			SUB,
			MUL,
			DIV,
			AND,
			OR,
			XOR,
			FLD,
			FLID,
			FSTP,
			FISTP,
			FISTTP,
			FADD,
			FSUB,
			FMUL,
			FDIV,
			MOVSS,
			ADDSS,
			SUBSS,
			MULSS,
			DIVSS,
			CVTSI2SD,
			CVTSI2SS,
			CVTSS2SD,
			INT,
			GLOBAL,
			EXTERN,
			NO_INST,
		};

		enum class Reg : uint8_t
		{
			EAX = 0, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
			AX, CX, DX, BX, SP, BP, SI, DI,
			AH, AL, CH, CL, DH, DL, BH, BL,
			ST1, ST0,
			XMM0, XMM1, XMM2, XMM3,
			NO_REG,
		};

		enum DerefSize : uint8_t
		{
			BYTE = 0,
			WORD,
			DWORD,
			QWORD,
			NO_DEREF,
		};

		enum ReserveSize : uint8_t
		{
			RESB = 0,
			RESW,
			RESQ,
			NO_RESERVE,
		};

		enum DefineSize : uint8_t
		{
			DB = 0,
			DW,
			DQ,
			NO_DEFINE,
		};

		static const int LABEL_ADR = 0;
		static const int REGISTER = 1;
		using MemAdress = std::variant<const char*, Reg>;

		struct MemAccess
		{
			MemAdress adress = Reg::NO_REG;
			int offset = 0;
			DerefSize size = NO_DEREF;

			MemAccess(const char* adr)
				: adress(adr) {}
			MemAccess(Reg reg)
				: adress(reg) {}
			MemAccess(Reg reg, int off)
				: adress(reg), offset(off) {}
			MemAccess(const char* adr, int off)
				: adress(adr), offset(off) {}
			MemAccess(Reg reg, int off, DerefSize s)
				: adress(reg), offset(off), size(s) {}
			MemAccess(const char* reg, int off, DerefSize s)
				: adress(reg), offset(off), size(s) {}
		};

		struct ReserveMem
		{
			const char* name;
			ReserveSize size = NO_RESERVE;
			int count;
		};


		struct DefineMem
		{
			const char* name;
			DefineSize size = NO_DEFINE;
			std::vector<std::variant<const char*, int>> bytes;
		};


		static const int MEM_ACCESS = 0;
		static const int INT_VALUE = 1;
		static const int FLOAT_VALUE = 2;
		static const int NO_ADR = 3;

		struct BasicInst
		{
			InstType type;
			std::variant<MemAccess, int, float, bool> adresses[2] = { false, false };

		};

		static const int BASIC_INST = 0;
		static const int RESERVE_MEM = 1;
		static const int DEFINE_MEM = 2;
		static const int SECTION = 3;

		using Instruction = std::variant<BasicInst, ReserveMem, DefineMem, Section>;

		using Label = const char*;
	}

	std::string to_string(std::unordered_map<x86ASM::Label, std::vector<x86ASM::Instruction>>& m_Code);
	using ASMCode = std::unordered_map<x86ASM::Label, std::vector<x86ASM::Instruction>>;

	struct StackVal
	{
		int offset;
		ValueType type;
	};

	class Compiler
	{
	private:
		const char* m_Name = "";

		x86ASM::Label m_CurrentLabel = "";
		ASMCode m_Code;

		std::unordered_map<std::string, StackVal> m_VarTable; //<name, offset>
		int m_BPOffset = 4;

		uint32_t m_StackMemAllocAdr;

		std::ofstream m_Output;

		void set_label(x86ASM::Label l) { m_CurrentLabel = l; }
		void write(x86ASM::Instruction i);
		void write(x86ASM::InstType t, x86ASM::MemAccess a);
		void write(x86ASM::InstType t, x86ASM::MemAccess a, x86ASM::MemAccess b);
		void write(x86ASM::InstType t, x86ASM::MemAccess a, int b);
		void write(x86ASM::InstType t, x86ASM::MemAccess a, float b);
		void write(x86ASM::InstType t, int a);
		void write(x86ASM::InstType t, float a);
		void write_section(x86ASM::Section s);
		void write_mem_def(const char* var, x86ASM::DefineSize size, std::vector<std::variant<const char*, int>> bytes);
		void write_mem_res(const char* var, x86ASM::ReserveSize size, int count);
		void set_stack_mem();

		void print_top();
		void print_chint();
		void print_value(ValueType type);

		ValueType eval_num(Token& token);
		void int_int_binop(TokenType type);
		void float_float_binop(TokenType type);
		ValueType eval_binop(Node* node);
		ValueType eval_assing(NodeValues::AssignOp& op);
		ValueType eval_expr(Node* node);
		ValueType eval_access(std::string var);


	public:
		void compile(const char* name, std::vector<Node*> node);
		void close();

		~Compiler()
		{
			close();
		}
	};


}

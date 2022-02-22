#include <fstream>
#include <unordered_map>
#include <string>

#include "Parser.h"
#include "common.h"

namespace Chronos
{
	enum class ASMExprType
	{
		INSTRUCTION = 0,
 		LABEL,
		COMMENT,
	};

	struct ASMExpr
	{
		ASMExprType type;

		ASMExpr(ASMExprType t)
			: type(t) {}

		virtual std::string get_string()
		{
			return "";
		}
	};


	struct Label : ASMExpr
	{
		const char* name;

		Label(const char* n)
			: ASMExpr(ASMExprType::LABEL), name(n) {}

		virtual std::string get_string()
		{
			return { name };
		}
	};

	struct Comment : ASMExpr
	{
		std::string msg = ";";

		Comment(const char* m)
			: ASMExpr(ASMExprType::LABEL)
		{
			msg += m;
		}

		virtual std::string get_string()
		{
			return { msg };
		}
	};

	enum class InstType
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
		INT,
		GLOBAL,
		EXTERN,
		SECTION,
		NONE,
	};

	enum class Register
	{
		EAX = 0, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
		AX, CX, DX, BX, SP, BP, SI, DI,
		AH, AL, CH, CL, DH, DL, BH, BL,
		NONE,
	};

	enum class ArgType
	{
		REGISTER = 0,
		REGISTER_OFFSET,
		REGISTER_DEREF,
		INT,
		FLOAT,
		HEX,
		LABEL,
		NONE,
	};

	enum class ASMSize
	{
		DWORD = 0,
	};



	struct ASMArg
	{
		ArgType type;

		union ArgValue
		{
			Register reg;

			struct Offset
			{
				Register reg;
				int offset;
			} reg_offset;

			struct
			{
				Register reg;
				int offset;
				ASMSize size;
			} deref_value;

			int int_value;
			float float_value;
			int hex_value;
			const char* label;

			~ArgValue() {}
			ArgValue()
				: int_value(0) {}
		} value;

		ASMArg()
			: type(ArgType::NONE) {}

		ASMArg(ArgType t)
			: type(t) {}

		ASMArg(const char* l)
			: type(ArgType::LABEL)
		{
			value.label = l;
		}

		ASMArg(Register reg)
			: type(ArgType::REGISTER)
		{
			value.reg = reg;
		}

		ASMArg(Register reg, int offset)
			: type(ArgType::REGISTER_OFFSET)
		{
			value.reg_offset.reg = reg;
			value.reg_offset.offset = offset;
		}

		ASMArg(ASMSize size, Register reg, int offset)
			: type(ArgType::REGISTER_DEREF)
		{
			value.deref_value.reg = reg;
			value.deref_value.offset = offset;
			value.deref_value.size = size;
		}

		ASMArg(float f)
			: type(ArgType::FLOAT)
		{
			value.float_value = f;
		}

		ASMArg(ArgType t, int val)
			: type(t)
		{
			switch (type)
			{
			case ArgType::INT:
				value.int_value = val;
				return;

			case ArgType::HEX:
				value.hex_value = val;
				return;
			}

			ASSERT(false, "type has no int value");
		}
	};

	std::string to_string(ASMArg& arg);
	std::string to_string(InstType& type);

	struct Instruction : ASMExpr
	{
		InstType type = InstType::NONE;
		std::vector<ASMArg> args;

		Instruction(InstType t, std::vector<ASMArg> a)
			: ASMExpr(ASMExprType::INSTRUCTION), args(a), type(t) {}

		virtual std::string get_string()
		{
			std::string s = to_string(type);
			s += " ";

			size_t i = 0;
			for (; i < args.size() - 1; i++)
			{
				s += to_string(args[i]);
				s += ", ";
			}

			s += to_string(args[i]);
			return s;
		}
	};


	std::string to_string(Register reg);
	std::string to_string(ASMSize size);
	std::string to_string(Instruction& instruction);
	std::string to_string(std::deque<ASMExpr*>& code);

	class Compiler
	{
	private:
		const char* m_Name = "";

		int m_MainLblPtr = 0;

		std::deque<ASMExpr*> m_Code;

		std::ofstream m_Output;

		void write_inst(InstType t, std::vector<ASMArg>&& a);
		void write_label(const char* label);
		void write_comment(const char* msg);
		void print_eax();
		void print_top();

		void eval_num(Token& token);
		void eval_binop(Node* node);
		void eval_expr(Node* node);


	public:
		void compile(const char* name, Node* node, int exit_code = 0);
		void close();

		~Compiler()
		{
			close();
		}
	};


}

#include <fstream>
#include <unordered_map>
#include <vector>
#include <string>

#include "Parser.h"
#include "common.h"

namespace Chronos
{
	struct ASMExpr
	{
		ASMExpr() {}
		virtual std::string get_string()
		{
			return "";
		}
	};


	struct Label : ASMExpr
	{
		const char* name;

		Label(const char* n)
			: name(n) {}

		virtual std::string get_string()
		{
			return { name };
		}
	};

	enum class InstructionType
	{
		PUSH,
		NOP,
		MOV,
		ADD,
		SUB,
		INT,
		GLOBAL,
		EXTERN,
		SECTION,

		NONE,
	};

	enum class Register
	{
		EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
		AX, CX, DX, BX, SP, BP, SI, DI,
		AH, AL, CH, CL, DH, DL, BH, BL,
		NONE,
	};

	enum class ArgType
	{
		REGISTER,
		REGISTER_OFFSET,
		INT_VALUE,
		HEX,
		LABEL,
		NONE,
	};


	union ArgValue
	{
		Register reg;

		struct 
		{
			Register reg;
			int offset;
		} reg_offset;

		int int_value;
		int hex_value;
		Label label;

		~ArgValue() {}
		ArgValue()
			: int_value(0) {}
	};

	struct Argument
	{
		ArgType type;
		ArgValue value;

		Argument()
			: type(ArgType::NONE) {}

		Argument& operator=(const Argument& other)
		{
			type = other.type;
			switch (type)
			{
			case ArgType::HEX:
				value.hex_value = other.value.hex_value;
				break;
			case ArgType::INT_VALUE:
				value.int_value = other.value.int_value;
				break;
			case ArgType::LABEL:
				value.label = other.value.label;
				break;
			case ArgType::REGISTER:
				value.reg = other.value.reg;
				break;
			case ArgType::REGISTER_OFFSET:
				value.reg_offset = other.value.reg_offset;
				break;
			}

			return *this;
		}

		Argument(const Argument& other)
			: type(other.type)
		{
			switch (type)
			{
			case ArgType::HEX:
				value.hex_value = other.value.hex_value;
				break;
			case ArgType::INT_VALUE:
				value.int_value = other.value.int_value;
				break;
			case ArgType::LABEL:
				value.label = other.value.label;
				break;
			case ArgType::REGISTER:
				value.reg = other.value.reg;
				break;
			case ArgType::REGISTER_OFFSET:
				value.reg_offset = other.value.reg_offset;
				break;
			}
		}

		Argument(Label l)
			: type(ArgType::LABEL)
		{
			value.label = l;
		}

		Argument(Register reg)
			: type(ArgType::REGISTER)
		{
			value.reg = reg;
		}

		Argument(Register reg, int offset)
			: type(ArgType::REGISTER_OFFSET)
		{
			value.reg_offset.reg = reg;
			value.reg_offset.offset = offset;
		}

		Argument(ArgType t, int val)
			: type(t)
		{
			switch (type)
			{
			case ArgType::INT_VALUE:
				value.int_value = val;
				return;

			case ArgType::HEX:
				value.hex_value = val;
				return;
			}

			ASSERT(false, "type has no int value");
		}
	};

	std::string to_string(Argument& arg);
	std::string to_string(InstructionType& type);

	struct Instruction : ASMExpr
	{
		InstructionType type = InstructionType::NONE;
		std::vector<Argument> args;

		Instruction(InstructionType t, std::vector<Argument> a)
			: args(a), type(t) {}

		virtual std::string get_string()
		{
			std::string s = to_string(type);
			s += " ";

			int i = 0;
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
	std::string to_string(Instruction& instruction);
	std::string to_string(std::vector<ASMExpr*>& code);

	class Compiler
	{
	private:
		Node* m_Root;
		const char* m_Name;
		std::vector<ASMExpr*> m_Code;

		std::ofstream m_Output;

		void eval_num(Token& token);
		void eval_expr(Node* node);

	public:
		void compile(const char* name, Node* node);
	};


}

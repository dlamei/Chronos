#include <iostream>
#include <bitset>
#include <sstream>

#include "Compiler.h"

namespace Chronos
{

	std::string to_string(Register reg)
	{
		switch (reg)
		{
		case Register::EAX: return "EAX";
		case Register::ECX: return "ECX";
		case Register::EDX: return "EDX";
		case Register::EBX: return "EBX";
		case Register::ESP: return "ESP";
		case Register::EBP: return "EBP";
		case Register::ESI: return "ESI";
		case Register::EDI: return "EDI";
		case Register::AX : return  "AX";
		case Register::CX : return  "CX";
		case Register::DX : return  "DX";
		case Register::BX : return  "BX";
		case Register::SP : return  "SP";
		case Register::BP : return  "BP";
		case Register::SI : return  "SI";
		case Register::DI : return  "DI";
		case Register::AH : return  "AH";
		case Register::AL : return  "AL";
		case Register::CH : return  "CH";
		case Register::CL : return  "CL";
		case Register::DH : return  "DH";
		case Register::DL : return  "DL";
		case Register::BH : return  "BH";
		case Register::BL : return  "BL";
		}

		ASSERT(false, "to_string for register not defined");
		exit(-1);
	}

	std::string to_string(Argument& arg)
	{
		switch (arg.type)
		{
		case ArgType::INT_VALUE: return std::to_string(arg.value.int_value);
		case ArgType::REGISTER: return to_string(arg.value.reg);
		case ArgType::LABEL: return arg.value.label.name;

		case ArgType::REGISTER_OFFSET:
		{
			std::string s = "[";
			s += to_string(arg.value.reg_offset.reg);
			if (arg.value.reg_offset.offset > 0) s += "+";
			s += std::to_string(arg.value.reg_offset.offset);
			s += "]";
			return s;
		}

		case ArgType::HEX:
		{
			std::stringstream s;
			s << "0x";
			s << std::hex << arg.value.hex_value;
			return s.str();
		}
		}

		ASSERT(false, "to_string for arg not defined");
		exit(-1);
	}

	std::string to_string(InstructionType& type)
	{
		switch (type)
		{
			case InstructionType::INT: return "INT";
			case InstructionType::MOV: return "MOV";
			case InstructionType::NOP: return "NOP";
			case InstructionType::PUSH: return "PUSH";
			case InstructionType::ADD: return "ADD";
			case InstructionType::SUB: return "SUB";
			case InstructionType::GLOBAL: return "GLOBAL";
			case InstructionType::EXTERN: return "EXTERN";
			case InstructionType::SECTION: return "SECTION";
		}

		ASSERT(false, "case for int_type not implemented!");
		exit(-1);
	}

	std::string to_string(Instruction& inst)
	{
		std::string s = to_string(inst.type);
		s += " ";

		if (inst.args.size() == 1)
		{
			s += to_string(inst.args[0]);
			return s;
		}

		for (Argument& arg : inst.args)
		{
			s += to_string(arg);
			s += ", ";
		}

		return s;
	}

	std::string to_string(std::vector<ASMExpr*>& code)
	{
		std::string s = "";
		for (auto expr : code)
		{
			s += expr->get_string();
			s += "\n";
		}

		return s;
	}

	void Compiler::eval_num(Token& t)
	{
		switch (t.type)
		{
		case TokenType::INT:
			m_Output << std::to_string(t.value.ival);
			break;

		case TokenType::FLOAT:
			ASSERT(false, "float not implemented");
			break;

		default:
			ASSERT(false, "num node should not have this token" + to_string(t));
		}
	}

	void Compiler::eval_expr(Node* node)
	{
		if (!node) return;

		switch (node->type)
		{
		case NodeType::NUM: 
			eval_num(node->value.token);
			break;

		default:
			ASSERT(false, "not implemented yet");
			break;
		}
	}

	void Compiler::compile(const char* name, Node* node)
	{
		m_Root = node;
		m_Name = name;

		std::string file_name = name;
		file_name += ".asm";
		m_Output = std::ofstream(file_name.c_str());

		const char* setup =
			R"(
global main
extern printf
			
section .data
section .text

main:

	push ebp
	mov ebp, esp
	sub esp, 16

	mov eax, 1
	mov ebx, 32
	int 0x80
)";

		m_Code.push_back(new Instruction(InstructionType::GLOBAL, { {"main"} }));
		m_Code.push_back(new Instruction(InstructionType::EXTERN, { {"printf"} }));
		m_Code.push_back(new Instruction(InstructionType::SECTION, { {".data"} }));
		m_Code.push_back(new Instruction(InstructionType::SECTION, { {".text"} }));
		m_Code.push_back(new Label("main:"));
		m_Code.push_back(new Instruction(InstructionType::PUSH, { {Register::EBP} }));
		m_Code.push_back(new Instruction(InstructionType::MOV, { {Register::EBP, Register::ESP} }));
		m_Code.push_back(new Instruction(InstructionType::SUB, { {{Register::ESP}, {ArgType::INT_VALUE, 16} } }));
		m_Code.push_back(new Instruction(InstructionType::MOV, { {{Register::EAX}, {ArgType::INT_VALUE, 1} } }));
		m_Code.push_back(new Instruction(InstructionType::MOV, { {{Register::EBX}, {ArgType::INT_VALUE, 32} } }));
		m_Code.push_back(new Instruction(InstructionType::INT, { {ArgType::HEX, 0x80 } }));

		//m_Code.push_back({ Label("main") });

		m_Output << to_string(m_Code);

		//m_Output << setup << std::endl;

		//m_Output << "	mov DWORD [ebp-4], ";
		//eval_expr(node);

		m_Output << std::endl;
		m_Output.close();
	}
}

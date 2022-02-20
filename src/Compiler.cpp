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

	std::string to_string(ASMArg& arg)
	{
		switch (arg.type)
		{
		case ArgType::INT: return std::to_string(arg.value.int_value);
		case ArgType::FLOAT: return std::to_string(arg.value.float_value);
		case ArgType::REGISTER: return to_string(arg.value.reg);
		case ArgType::LABEL: return arg.value.label;

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

	std::string to_string(InstType& type)
	{
		switch (type)
		{
			case InstType::INT: return "INT";
			case InstType::MOV: return "MOV";
			case InstType::NOP: return "NOP";
			case InstType::PUSH: return "PUSH";
			case InstType::POP: return "POP";
			case InstType::CALL: return "CALL";
			case InstType::ADD: return "ADD";
			case InstType::SUB: return "SUB";
			case InstType::MUL: return "MUL";
			case InstType::DIV: return "DIV";
			case InstType::GLOBAL: return "GLOBAL";
			case InstType::EXTERN: return "EXTERN";
			case InstType::SECTION: return "SECTION";
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

		for (ASMArg& arg : inst.args)
		{
			s += to_string(arg);
			s += ", ";
		}

		return s;
	}

	std::string to_string(std::deque<ASMExpr*>& code)
	{
		std::string s = "";
		for (auto expr : code)
		{
			s += expr->get_string();
			s += "\n";
		}

		return s;
	}


	void Compiler::write_inst(InstType t, std::vector<ASMArg>&& a)
	{
		m_Code.push_back(new Instruction(std::move(t), std::move(a)));
	}

	void Compiler::write_label(const char* label)
	{
		m_Code.push_back(new Label(label));
	}

	void Compiler::write_comment(const char* msg)
	{
#ifdef DEBUG
		m_Code.push_back(new Comment(msg));
#endif
	}

	void Compiler::eval_num(Token& t)
	{
		switch (t.type)
		{
		case TokenType::INT:
			//m_Code.push_back(new Instruction(InstType::MOV, { {Register::EAX, {ArgType::INT, t.value.ival}} }));
			m_Code.push_back(new Instruction(InstType::PUSH, { ASMArg(ArgType::INT, t.value.ival)} ));
			break;

		case TokenType::FLOAT:
			ASSERT(false, "float eval not implemented");
			break;

		default:
			ASSERT(false, "num node should not have this token" + to_string(t));
		}
	}

	void Compiler::eval_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");

		eval_expr(node->value.binop_value.right);
		eval_expr(node->value.binop_value.left);

		write_inst(InstType::POP, { ASMArg(Register::EAX) });
		write_inst(InstType::POP, { ASMArg(Register::EBX) });

		switch (node->value.binop_value.type)
		{
		case TokenType::ADD:
			write_inst(InstType::ADD, { ASMArg(Register::EAX), ASMArg(Register::EBX) });
			break;
		case TokenType::SUB:
			write_inst(InstType::SUB, { ASMArg(Register::EAX), ASMArg(Register::EBX) });
			break;
		case TokenType::MULT:
			write_inst(InstType::MUL, { ASMArg(Register::EBX) });
			break;
		case TokenType::DIV:
			write_inst(InstType::DIV, { ASMArg(Register::EBX) });
			break;

		default:
			ASSERT(false, "op type not defined");
			exit(-1);
		}

		write_inst(InstType::PUSH, { ASMArg(Register::EAX) });


	}

	void Compiler::eval_expr(Node* node)
	{
		if (!node) return;

		switch (node->type)
		{
		case NodeType::NUM: 
			eval_num(node->value.token);
			break;

		case NodeType::BINOP:
			eval_binop(node);
			break;

		default:
			ASSERT(false, "not implemented yet");
			break;
		}
	}

	void Compiler::close()
	{
		m_Output << to_string(m_Code);
		m_Output << std::endl;
		m_Output.close();

		m_Name = "";

		for (auto* expr : m_Code)
		{
			delete expr;
		}
		m_Code.clear();
	}

	void Compiler::print_eax()
	{
		write_inst(InstType::PUSH, { ASMArg(Register::EAX) });
		write_inst(InstType::PUSH, { ASMArg("int_format") });
		write_inst(InstType::CALL, { ASMArg("printf") });
	}
	
	void Compiler::print_top()
	{
		write_inst(InstType::PUSH, { ASMArg("int_format") });
		write_inst(InstType::CALL, { ASMArg("printf") });
	}

	void Compiler::compile(const char* name, Node* node, int exit_code)
	{
		m_Name = name;

		std::string file_name = name;
		file_name += ".asm";
		m_Output = std::ofstream(file_name.c_str());

		write_inst(InstType::GLOBAL, { ASMArg("main") });
		write_inst(InstType::EXTERN, { ASMArg("printf") });
		write_inst(InstType::SECTION, { ASMArg(".data") });
		write_label("int_format db \"%d\", 0xa, 0x0");
		write_inst(InstType::SECTION, { ASMArg(".text") });
		write_label("main:");
		write_inst(InstType::PUSH, { ASMArg(Register::EBP) });
		write_inst(InstType::MOV, { ASMArg(Register::EBP), ASMArg(Register::ESP) });
		write_inst(InstType::SUB, { ASMArg(Register::ESP), ASMArg(ArgType::INT, 16) });
		write_comment("begin code:");
		write_comment("");

		eval_expr(node);

		print_top();

		write_comment("");
		write_comment("end code");
		write_inst(InstType::MOV, { ASMArg(Register::ESP), ASMArg(Register::EBP) });
		write_inst(InstType::POP, { ASMArg(Register::EBP) });
		write_inst(InstType::MOV, { ASMArg(Register::EAX), ASMArg(ArgType::INT, 1) });
		write_inst(InstType::MOV, { ASMArg(Register::EBX), ASMArg(ArgType::INT, exit_code) });
		write_inst(InstType::INT, { ASMArg(ArgType::HEX, 0x80) });
	}
}

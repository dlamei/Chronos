#include <iostream>
#include <bitset>
#include <sstream>

#include "Compiler.h"


namespace Chronos
{
	using namespace NodeValues;
	using namespace x86ASM;

	std::string to_string(Reg reg)
	{
		switch (reg)
		{
		case Reg::EAX: return "EAX";
		case Reg::ECX: return "ECX";
		case Reg::EDX: return "EDX";
		case Reg::EBX: return "EBX";
		case Reg::ESP: return "ESP";
		case Reg::EBP: return "EBP";
		case Reg::ESI: return "ESI";
		case Reg::EDI: return "EDI";
		case Reg::AX: return  "AX";
		case Reg::CX: return  "CX";
		case Reg::DX: return  "DX";
		case Reg::BX: return  "BX";
		case Reg::SP: return  "SP";
		case Reg::BP: return  "BP";
		case Reg::SI: return  "SI";
		case Reg::DI: return  "DI";
		case Reg::AH: return  "AH";
		case Reg::AL: return  "AL";
		case Reg::CH: return  "CH";
		case Reg::CL: return  "CL";
		case Reg::DH: return  "DH";
		case Reg::DL: return  "DL";
		case Reg::BH: return  "BH";
		case Reg::BL: return  "BL";

		case Reg::ST1: return "ST1";
		case Reg::ST0: return "ST0";
		case Reg::XMM0: return "XMM0";
		case Reg::XMM1: return "XMM1";
		case Reg::XMM2: return "XMM2";
		case Reg::XMM3: return "XMM3";
		}

		ASSERT(false, "to_string for register not defined");
		exit(-1);
	}

	std::string to_string(DerefSize size)
	{
		switch (size)
		{
		case BYTE: return "BYTE";
		case WORD: return "WORD";
		case DWORD: return "DWORD";
		}

		ASSERT(false, "to_string for ASMSize not defined");
		exit(-1);
	}

	std::string to_string(const InstType& type)
	{
		switch (type)
		{
		case INT: return "INT";
		case AND: return "AND";
		case MOV: return "MOV";
		case NOP: return "NOP";
		case PUSH: return "PUSH";
		case POP: return "POP";
		case CALL: return "CALL";
		case ADD: return "ADD";
		case FLD: return "FLD";
		case FLID: return "FLID";
		case FSTP: return "FSTP";
		case FISTP: return "FISTP";
		case FISTTP: return "FISTTP";
		case FADD: return "FADD";
		case FSUB: return "FSUB";
		case FMUL: return "FMUL";
		case FDIV: return "FDIV";
		case MOVD: return "MOVD";
		case MOVSS: return "MOVSS";
		case ADDSS: return "ADDSS";
		case SUBSS: return "SUBSS";
		case MULSS: return "MULSS";
		case DIVSS: return "DIVSS";
		case CVTSI2SD: return "CVTSI2SD";
		case CVTSI2SS: return "CVTSI2SS";
		case CVTSS2SD: return "CVTSS2SD";
		case SUB: return "SUB";
		case MUL: return "MUL";
		case DIV: return "DIV";
		case NEG: return "NEG";
		case CMP: return "CMP";
		case JE: return "JE";
		case JNE: return "JNE";
		case JP: return "JP";
		case UCOMISS: return "UCOMISS";
		case PXOR: return "PXOR";
		case TEST: return "TEST";
		case JZ: return "JZ";
		case JMP: return "JMP";
		case GLOBAL: return "GLOBAL";
		case EXTERN: return "EXTERN";
		}

		ASSERT(false, "case for int_type not implemented!");
		return "";
	}

	std::string to_string(const Section sec)
	{
		switch (sec)
		{
		case DATA: return "section .data";
		case BSS: return "section .bss";
		case TEXT: return "section .text";
		}

		ASSERT(false, "to_string for Section not implemented");
		return "";
	}

	std::string int_to_hex(int i)
	{
		std::stringstream hex;
		hex << "0x";
		hex << std::hex << i;
		return hex.str();
	}

	std::string to_string(const ReserveMem res)
	{
		std::string s = res.name;
		s += ": ";

		switch (res.size)
		{
		case RESB:
			s += "RESB";
			break;
		case RESW:
			s += "RESW";
			break;
		case RESQ:
			s += "RESQ";
			break;
		default:
			ASSERT(false, "reserve size not implemented");
			break;
		}

		s += " " + int_to_hex(res.count);

		return s;
	}

	std::string to_string(const DefineMem def)
	{
		std::string s = def.name;
		s += " ";

		switch (def.size)
		{
		case DB:
			s += "DB";
			break;
		case DW:
			s += "DW";
			break;
		case DQ:
			s += "DQ";
			break;
		default:
			ASSERT(false, "DefineSize not defined");
			break;
		}

		s += " ";

		for (auto& data : def.bytes)
		{
			if (data.index() == 0) s += std::get<const char*>(data);
			else if (data.index() == 1) s += int_to_hex(std::get<int>(data));
			else
			{
				ASSERT(false, "bytes value not defined");
			}

			s += ", ";
		}

		return s;
	}

	std::string to_string(SubLabel l)
	{
		return std::string(".L") + std::to_string(l.count);
	}

	std::string to_string(const MemAdress& adr)
	{
		switch (adr.index())
		{
		case LABEL_ADR:
			return std::get<const char*>(adr);
		case REGISTER:
			return to_string(std::get<Reg>(adr));
		case SUB_LABEL_ADR:
			return to_string(std::get<SubLabel>(adr));
		default:
			ASSERT(false, "undefined address");
			return "";
		}
	}

	std::string to_string(const MemAccess& acc)
	{
		std::string s = "";
		if (acc.size != DerefSize::NO_DEREF)
		{
			s += to_string(acc.size);
			s += " [";
		}
		s += to_string(acc.adress);
		if (acc.offset > 0) s += "+" + std::to_string(acc.offset);
		else if (acc.offset < 0) s += std::to_string(acc.offset);
		if (acc.size != DerefSize::NO_DEREF) s += "]";
		return s;
	}

	std::string to_string(const std::variant<MemAccess, int, float, bool>& acc)
	{
		if (acc.index() == MEM_ACCESS) return to_string(std::get<MemAccess>(acc));
		else if (acc.index() == INT_VALUE) return int_to_hex(std::get<int>(acc));
		else if (acc.index() == FLOAT_VALUE) return "__float32__(" + std::to_string(std::get<float>(acc)) + ")";
		ASSERT(false, "acces type not defined");
		return "";
	}

	std::string to_string(const BasicInst& inst)
	{
		std::string s = to_string(inst.type) + " ";

		s += to_string(inst.adresses[0]);
		if (inst.adresses[1].index() != NO_ADR) s += ", " + to_string(inst.adresses[1]);
		return s;
	}

	std::string to_string(const Instruction& inst)
	{
		switch (inst.index())
		{
		case BASIC_INST:
			return to_string(std::get<BasicInst>(inst));
		case RESERVE_MEM:
			return to_string(std::get<ReserveMem>(inst));
		case DEFINE_MEM:
			return to_string(std::get<DefineMem>(inst));
		case SECTION:
			return to_string(std::get<Section>(inst));
		case SUB_LABEL:
			return to_string(std::get<SubLabel>(inst)) + ":";
		}

		ASSERT(false, "undefined Instruction");
		return "";
	}

	std::string to_string(const Label& l)
	{
		return l;
	}

	std::string to_string(std::unordered_map<Label, std::vector<Instruction>>& m_Code)
	{
		std::string res = "";

		Label null_label = "";
		Label& current_label = null_label;

		if (m_Code.find("") != m_Code.end())
		{
			for (auto& inst : m_Code.at("")) res += to_string(inst) + "\n";
		}

		for (auto& pair : m_Code)
		{
			if (pair.first == "") continue;

			if (pair.first != current_label)
			{
				current_label = pair.first;
				res += to_string(current_label) + ":\n";
			}

			for (auto& inst : pair.second) res += to_string(inst) + "\n";
		}

		return res;
	}

	void Compiler::close()
	{
		m_Output << to_string(m_Code);
		m_Output << std::endl;
		m_Output.close();

		m_Name = "";

		m_Code.clear();
	}

	SubLabel Compiler::sub_label()
	{
		return SubLabel{ m_CurrentSubLabel };
	}

	SubLabel Compiler::sub_label(uint32_t offset)
	{
		return SubLabel{ m_CurrentSubLabel + offset };
	}

	void Compiler::offset_sub_label(uint32_t offset)
	{
		m_CurrentSubLabel += offset;
	}

	void Compiler::write(Instruction i)
	{
		if (m_Code.find(m_CurrentLabel) == m_Code.end())
		{
			m_Code.insert({ m_CurrentLabel, { i } });
		}
		else
		{
			m_Code.at(m_CurrentLabel).push_back(i);
		}
	}

	void Compiler::write(InstType t, MemAccess a)
	{
		write(Instruction{ BasicInst{ t, { a, false } } });
	}

	void Compiler::write(InstType t, MemAccess a, MemAccess b)
	{
		write(Instruction{ BasicInst{ t, { a, b } } });
	}

	void Compiler::write(InstType t, MemAccess a, int b)
	{
		write(Instruction{ BasicInst{ t, { a, b } } });
	}

	void Compiler::write(InstType t, MemAccess a, float b)
	{
		write(Instruction{ BasicInst{ t, { a, b } } });
	}

	void Compiler::write(InstType t, int a)
	{
		write(Instruction{ BasicInst{ t, { a, false } } });
	}

	void Compiler::write(InstType t, float a)
	{
		write(Instruction{ BasicInst{ t, { a, false } } });
	}

	void Compiler::write_section(Section s)
	{
		write({ Instruction { s } });
	}

	void Compiler::write_mem_def(const char* var, DefineSize size, std::vector<std::variant<const char*, int>> bytes)
	{
		write(Instruction{ DefineMem { var, size, std::move(bytes) } });
	}

	void Compiler::write_mem_res(const char* var, ReserveSize size, int count)
	{
		write(Instruction{ ReserveMem { var, size, count } });
	}

	void Compiler::print_top()
	{
		write(PUSH, "int_format");
		write(CALL, "printf");
	}

	void Compiler::print_chint()
	{
		write(PUSH, { Reg::EAX, HEADER_SIZE, DWORD });
		write(PUSH, "int_format");
		write(CALL, "printf");
	}

	void Compiler::print_value(ValueType type)
	{
		switch (type)
		{
		case ValueType::INT:
			write(PUSH, "int_format");
			write(CALL, "printf");
			break;
		case ValueType::FLOAT:
			write(CALL, "print_float");
			break;
		default:
			write(PUSH, "hex_format");
			write(CALL, "printf");
		}

	}

	void Compiler::zero_cmp_float()
	{
		write(PXOR, Reg::XMM0, Reg::XMM0);
		write(ADD, Reg::ESP, 4);
		write(UCOMISS, Reg::XMM0, { Reg::ESP, -4, DWORD });
	}

	void Compiler::zero_cmp_int()
	{
		write(POP, Reg::ECX);
		write(TEST, Reg::ECX, Reg::ECX);
	}

	void Compiler::eval_num(Token& t)
	{
		switch (t.type)
		{
		case TokenType::INT:
			write(PUSH, std::get<int>(t.value));
			break;

		case TokenType::FLOAT:
			write(PUSH, std::get<float>(t.value));
			break;

		default:
			ASSERT(false, "num node should not have this token" + to_string(t));
		}

	}

	void Compiler::int_int_binop(TokenType type)
	{

		switch (type)
		{
		case TokenType::ADD:
			write(ADD, Reg::EAX, Reg::ECX);
			break;
		case TokenType::SUB:
			write(SUB, Reg::EAX, Reg::ECX);
			break;
		case TokenType::MUL:
			write(MUL, Reg::ECX);
			break;
		case TokenType::DIV:
			write(MOV, Reg::EDX, 0);
			write(DIV, Reg::ECX);
			break;

		default:
			ASSERT(false, "op type not defined");
			exit(-1);
			break;
		}
		write(PUSH, Reg::EAX);
	}

	void Compiler::float_float_binop(TokenType type)
	{
		InstType inst_type = NO_INST;

		switch (type)
		{
		case TokenType::ADD:
			inst_type = ADDSS;
			break;
		case TokenType::SUB:
			inst_type = SUBSS;
			break;
		case TokenType::MUL:
			inst_type = MULSS;
			break;
		case TokenType::DIV:
			inst_type = DIVSS;
			break;
		}

		write(inst_type, Reg::XMM0, Reg::XMM1);

		write(MOVSS, { Reg::ESP, -4, DWORD }, Reg::XMM0);
		write(SUB, Reg::ESP, 4);
	}

	void Compiler::eval_arith_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");

		Node* right = std::get<BinOp>(node->value).right;
		Node* left = std::get<BinOp>(node->value).left;

		eval_expr(left);
		eval_expr(right);

		ValueType ltype = left->value_type;
		ValueType rtype = right->value_type;

		if (ltype == ValueType::FLOAT && rtype == ValueType::INT)
		{
			write(MOVSS, Reg::XMM0, { Reg::ESP, 4, DWORD });
			write(CVTSI2SS, Reg::XMM1, { Reg::ESP, 0, DWORD });
			write(ADD, Reg::ESP, 8);
			float_float_binop(std::get<BinOp>(node->value).type);
		}
		else if (ltype == ValueType::INT && rtype == ValueType::FLOAT)
		{
			write(CVTSI2SS, Reg::XMM0, { Reg::ESP, 4, DWORD });
			write(MOVSS, Reg::XMM1, { Reg::ESP, 0, DWORD });
			write(ADD, Reg::ESP, 8);
			float_float_binop(std::get<BinOp>(node->value).type);
		}
		else if (ltype == ValueType::INT && rtype == ValueType::INT)
		{
			write(POP, Reg::ECX);
			write(POP, Reg::EAX);
			int_int_binop(std::get<BinOp>(node->value).type);
		}
		else if (ltype == ValueType::FLOAT && rtype == ValueType::FLOAT)
		{
			write(MOVSS, Reg::XMM0, { Reg::ESP, 4, DWORD });
			write(MOVSS, Reg::XMM1, { Reg::ESP, 0, DWORD });
			write(ADD, Reg::ESP, 8);
			float_float_binop(std::get<BinOp>(node->value).type);
		}

	}

	void Compiler::eval_AND_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");

		Node* right = std::get<BinOp>(node->value).right;
		Node* left = std::get<BinOp>(node->value).left;

		eval_expr(left);
		ValueType ltype = left->value_type;

		if (ltype == ValueType::INT)
		{
			zero_cmp_int();
			write(JE, sub_label(0));
		}
		else if (ltype == ValueType::FLOAT)
		{
			zero_cmp_float();
			write(JE, sub_label(0));
		}

		eval_expr(right);
		ValueType rtype = right->value_type;

		if (rtype == ValueType::INT)
		{
			zero_cmp_int();
			write(JE, sub_label(0));
		}
		else if (rtype == ValueType::FLOAT)
		{
			zero_cmp_float();
			write(JE, sub_label(0));
		}

		write(MOV, Reg::EAX, 1);
		write(JMP, sub_label(1));

		write(sub_label(0));
		write(MOV, Reg::EAX, 0);
		write(sub_label(1));
		write(PUSH, Reg::EAX);

		offset_sub_label(2);

	}

	void Compiler::eval_OR_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");

		Node* right = std::get<BinOp>(node->value).right;
		Node* left = std::get<BinOp>(node->value).left;

		eval_expr(left);
		ValueType ltype = left->value_type;

		if (ltype == ValueType::INT)
		{
			zero_cmp_int();
			write(JNE, sub_label(0));
		}
		else if (ltype == ValueType::FLOAT)
		{
			zero_cmp_float();
			write(JNE, sub_label(0));
		}

		eval_expr(right);
		ValueType rtype = right->value_type;

		if (rtype == ValueType::INT)
		{
			zero_cmp_int();
			write(JE, sub_label(1));
		}
		else if (rtype == ValueType::FLOAT)
		{
			zero_cmp_float();
			write(JE, sub_label(1));
		}

		write(sub_label(0));
		write(MOV, Reg::EAX, 1);
		write(JMP, sub_label(2));

		write(sub_label(1));
		write(MOV, Reg::EAX, 0);

		write(sub_label(2));
		write(PUSH, Reg::EAX);

		offset_sub_label(3);

	}

	void Compiler::eval_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");
		BinOp& binop_val = std::get<BinOp>(node->value);

		switch (binop_val.type)
		{
		case TokenType::ADD:
		case TokenType::SUB:
		case TokenType::MUL:
		case TokenType::DIV:
			eval_arith_binop(node);
			break;
		case TokenType::KW_AND:
			eval_AND_binop(node);
			break;
		case TokenType::KW_OR:
			eval_OR_binop(node);
			break;

		default:
			ASSERT(false, "type of binop not supported");
			exit(-1);
		}
	}

	void Compiler::eval_SUB_unryop(Node* node)
	{
		ASSERT(node->type == NodeType::UNRYOP, "expected unryop type");
		UnryOp& unryop_val = std::get<UnryOp>(node->value);

		eval_expr(unryop_val.right);
		ValueType type = node->value_type;

		if (type == ValueType::INT)
		{
			write(POP, Reg::ECX);
			write(NEG, Reg::ECX);
			write(PUSH, Reg::ECX);
		}
		else if (type == ValueType::FLOAT)
		{
			write(MOVSS, Reg::XMM0, { Reg::ESP, 0, DWORD });
			write(MOV, Reg::EAX, (int) 0x80000000);
			write(MOVD, Reg::XMM1, Reg::EAX);
			write(PXOR, Reg::XMM0, Reg::XMM1);
			write(MOVSS, { Reg::ESP, 0, DWORD }, Reg::XMM0);
		}


		ASSERT(false, "unryop SUB not defined for this type");
	}

	void Compiler::eval_unryop(Node* node)
	{
		ASSERT(node->type == NodeType::UNRYOP, "expected unryop type");
		UnryOp& unryop_val = std::get<UnryOp>(node->value);

		switch (unryop_val.type)
		{
		case TokenType::SUB:
			eval_SUB_unryop(node);
			break;

		default:
			ASSERT(false, "type of unryop not supported");
			exit(-1);
		}

	}

	void Compiler::eval_assing(Node* node)
	{
		ASSERT(node->type == NodeType::ASSIGN, "expected unryop type");
		AssignOp& op = std::get<AssignOp>(node->value);

		eval_expr(op.expr);
		ValueType type = node->value_type;

		m_VarTable.insert({ op.var, StackVal { m_BPOffset, type } });
		write(MOV, Reg::EAX, { Reg::ESP, 0, DWORD });
		write(MOV, { Reg::EBP, -m_BPOffset, DWORD }, Reg::EAX);
		m_BPOffset += 4;

	}

	void Compiler::eval_access(std::string var)
	{
		if (m_VarTable.find(var) != m_VarTable.end())
		{
			write(PUSH, { Reg::EBP, -m_VarTable.at(var).offset, DWORD });
		}

	}

	void Compiler::eval_expr(Node* node)
	{
		if (!node) return;

		switch (node->type)
		{
		case NodeType::ROOT:
			for (Node* n : std::get<Root>(node->value).nodes)
			{
				eval_expr(n);
				print_value(n->value_type);
			}
			break;

		case NodeType::NUM:
			eval_num(std::get<Token>(node->value));
			break;
		case NodeType::BINOP:
			eval_binop(node);
			break;
		case NodeType::UNRYOP:
			eval_unryop(node);
			break;
		case NodeType::ASSIGN:
			eval_assing(node);
			break;
		case NodeType::ACCESS:
			eval_access(std::get<std::string>(node->value));
			break;

		default:
			ASSERT(false, "not implemented yet");
			break;
		}
	}

	void Compiler::compile(const char* name, Node* root)
	{
		m_Name = name;

		std::string file_name = name;
		file_name += ".asm";
		m_Output = std::ofstream(file_name.c_str());

		TypeChecker checker;
		checker.check_type(root);

		set_label("");
		write(GLOBAL, "main");
		write(EXTERN, "printf");
		write(EXTERN, "print_float");
		write(EXTERN, "alloc_heap");
		write(EXTERN, "heap_alloc_int");

		write_section(DATA);
		write_mem_def("int_format", DB, { "\"%d\"", 10, 0 });
		write_mem_def("hex_format", DB, { "\"%#06x\"", 10, 0 });
		write_mem_def("double_format", DB, { "\"%f\"", 10, 0 });

		write_section(BSS);
		write_mem_res("heap_ptr", RESB, PTR_SIZE);

		write_section(TEXT);

		set_label("main");
		write(AND, Reg::ESP, -8);
		write(PUSH, Reg::EBP);
		write(MOV, Reg::EBP, Reg::ESP);
		write(SUB, Reg::ESP, (int) checker.get_alloc_size());

		write(CALL, "alloc_heap");
		write(MOV, { "heap_ptr", 0, DWORD }, Reg::EAX);

		eval_expr(root);

		write(MOV, Reg::ESP, Reg::EBP);
		write(POP, Reg::EBP);
		write(MOV, Reg::EAX, 1);
		write(MOV, Reg::EBX, 1);
		write(INT, 0x80);

	}
}

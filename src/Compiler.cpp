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
		case Reg::AX : return  "AX";
		case Reg::CX : return  "CX";
		case Reg::DX : return  "DX";
		case Reg::BX : return  "BX";
		case Reg::SP : return  "SP";
		case Reg::BP : return  "BP";
		case Reg::SI : return  "SI";
		case Reg::DI : return  "DI";
		case Reg::AH : return  "AH";
		case Reg::AL : return  "AL";
		case Reg::CH : return  "CH";
		case Reg::CL : return  "CL";
		case Reg::DH : return  "DH";
		case Reg::DL : return  "DL";
		case Reg::BH : return  "BH";
		case Reg::BL : return  "BL";

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

	std::string to_string(const MemAdress& adr)
	{
		switch (adr.index())
		{
		case LABEL_ADR:
			return std::get<const char*>(adr);
		case REGISTER:
			return to_string(std::get<Reg>(adr));
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
		write(Instruction { BasicInst{ t, { a, false } } });
	}

	void Compiler::write(InstType t, MemAccess a, MemAccess b)
	{
		write(Instruction { BasicInst{ t, { a, b } } });
	}

	void Compiler::write(InstType t, MemAccess a, int b)
	{
		write(Instruction { BasicInst{ t, { a, b } } });
	}

	void Compiler::write(InstType t, MemAccess a, float b)
	{
		write(Instruction { BasicInst{ t, { a, b } } });
	}

	void Compiler::write(InstType t, int a)
	{
		write(Instruction { BasicInst{ t, { a, false } } });
	}

	void Compiler::write(InstType t, float a)
	{
		write(Instruction { BasicInst{ t, { a, false } } });
	}

	void Compiler::write_section(Section s)
	{
		write({ Instruction { s } });
	}

	void Compiler::write_mem_def(const char* var, DefineSize size, std::vector<std::variant<const char*, int>> bytes)
	{
		write(Instruction { DefineMem { var, size, std::move(bytes) } });
	}

	void Compiler::write_mem_res(const char* var, ReserveSize size, int count)
	{
		write(Instruction { ReserveMem { var, size, count } });
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
		case ValueType::INT_VAL:
			write(PUSH, "int_format");
			write(CALL, "printf");
			break;
		case ValueType::FLOAT_VAL:
			write(CALL, "print_float");
			break;
		default:
			write(PUSH, "hex_format");
			write(CALL, "printf");
		}
		
	}
	
	void Compiler::set_stack_mem()
	{
		BasicInst binst = { SUB, {Reg::ESP, m_BPOffset} };
		m_Code.at(m_CurrentLabel).insert(m_Code.at(m_CurrentLabel).begin() + m_StackMemAllocAdr,  binst);
	}

	ValueType Compiler::eval_num(Token& t)
	{
		switch (t.type)
		{
		case TokenType::INT:
			//write(MOV, Reg::EAX, std::get<int>(t.value));
			write(PUSH, std::get<int>(t.value));
			//write(PUSH, std::get<int>(t.value));
			//write(PUSH, { "heap_ptr", 0, DWORD });
			//write(CALL, "heap_alloc_int");
			//write(ADD, Reg::ESP, 8);
			return ValueType::INT_VAL;

		case TokenType::FLOAT:
			//write(MOV, Reg::EAX, std::get<float>(t.value));
			write(PUSH, std::get<float>(t.value));
			return ValueType::FLOAT_VAL;

		default:
			ASSERT(false, "num node should not have this token" + to_string(t));
		}

		return ValueType::UNKNWN_PTR;
	}

	bool can_calc_temp(Node& n)
	{
		if (n.type == NodeType::ACCESS) return true; //TODO: check value of pointer
		if (n.type != NodeType::NUM) return false;
		Token& t = std::get<Token>(n.value);
		if (t.type != TokenType::INT) return false;
		return true;
	}

	bool is_ptr(ValueType type)
	{
		if (type == ValueType::INT_VAL) return false;
		return true;
	}

	void Compiler::int_int_binop(TokenType type)
	{
		write(POP, Reg::ECX);
		write(POP, Reg::EAX);

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
		write(MOVSS, Reg::XMM1, { Reg::ESP, 0, DWORD });
		write(ADD, Reg::ESP, 4);
		write(MOVSS, Reg::XMM0, { Reg::ESP, 0, DWORD });
		write(ADD, Reg::ESP, 4);

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

		//write(FLD, { Reg::ESP, 0, DWORD });
		//write(ADD, Reg::ESP, 4);
		//write(FLD, { Reg::ESP, 0, DWORD });
		//write(ADD, Reg::ESP, 4);

		//switch (type)
		//{
		//case TokenType::ADD:
		//	write(FADD, Reg::ST0, Reg::ST1);
		//	break;
		//case TokenType::SUB:
		//	write(FSUB, Reg::ST0, Reg::ST1);
		//	break;
		//case TokenType::MUL:
		//	write(FMUL, Reg::ST0, Reg::ST1);
		//	break;
		//case TokenType::DIV:
		//	write(FDIV, Reg::ST0, Reg::ST1);
		//	break;

		//default:
		//	ASSERT(false, "unexpected token for binop");
		//	return;
		//}


		//write(FSTP, { Reg::ESP, -4, DWORD });
		//write(SUB, Reg::ESP, 4);
	}

	ValueType Compiler::eval_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");

		Node* right = std::get<BinOp>(node->value).right;
		Node* left = std::get<BinOp>(node->value).left;

		ValueType ltype = eval_expr(left);
		ValueType rtype = eval_expr(right);

		if (ltype == ValueType::INT_VAL && rtype == ValueType::FLOAT_VAL)
		{
			
		}

		if (ltype == ValueType::INT_VAL && rtype == ValueType::INT_VAL)
		{
			int_int_binop(std::get<BinOp>(node->value).type);
			return ValueType::INT_VAL;
		}
		else if (ltype == ValueType::FLOAT_VAL && rtype == ValueType::FLOAT_VAL)
		{
			float_float_binop(std::get<BinOp>(node->value).type);
			return ValueType::FLOAT_VAL;
		}


		//switch (std::get<BinOp>(node->value).type)
		//{
		//case TokenType::ADD:
		//	write(ADD, Reg::EAX, Reg::ECX);
		//	break;
		//case TokenType::SUB:
		//	write(SUB, Reg::EAX, Reg::ECX);
		//	break;
		//case TokenType::MUL:
		//	write(MUL, Reg::ECX);
		//	break;
		//case TokenType::DIV:
		//	write(MOV, Reg::EDX, 0);
		//	write(DIV, Reg::ECX);
		//	break;

		//default:
		//	ASSERT(false, "op type not defined");
		//	break;
		//}

		return ValueType::UNKNWN_PTR;
	}

	ValueType Compiler::eval_assing(NodeValues::AssignOp& op)
	{
		ValueType type = eval_expr(op.expr);

		//if (is_ptr(tmp))
		//{
		//	//Copy the value into eax
		//	write(MOV, Reg::EAX, { Reg::EAX, HEADER_SIZE, DWORD });
		//}

		//write(PUSH, Reg::EAX);
		//write(PUSH, { "heap_ptr", 0, DWORD });
		//write(CALL, "heap_alloc_int");
		//write(ADD, Reg::ESP, 8);

		//TODO: check if already present and check type of present var
		m_VarTable.insert({ op.var, StackVal { m_BPOffset, type } });
		write(MOV, { Reg::EBP, -m_BPOffset, DWORD }, Reg::EAX);
		m_BPOffset += 4;

		//write(MOV, Reg::EAX, { Reg::EAX, 4, DWORD });

		return ValueType::INT_VAL;
	}

	ValueType Compiler::eval_access(std::string var)
	{
		if (m_VarTable.find(var) != m_VarTable.end())
		{
			write(MOV, Reg::EAX, { Reg::EBP, -m_VarTable.at(var).offset, DWORD });
			return ValueType::INT_VAL;
		}

		return ValueType::UNKNWN_PTR;
	}

	ValueType Compiler::eval_expr(Node* node)
	{
		if (!node) return ValueType::UNKNWN_PTR;

		switch (node->type)
		{
		case NodeType::NUM: 
			return eval_num(std::get<Token>(node->value));

		case NodeType::BINOP:
			return eval_binop(node);

		case NodeType::ASSIGN:
			return eval_assing(std::get<NodeValues::AssignOp>(node->value));

		case NodeType::ACCESS:
			return eval_access(std::get<std::string>(node->value));

		default:
			ASSERT(false, "not implemented yet");
			return ValueType::UNKNWN_PTR;
		}
	}

	void Compiler::compile(const char* name, std::vector<Node*> nodes)
	{
		m_Name = name;

		std::string file_name = name;
		file_name += ".asm";
		m_Output = std::ofstream(file_name.c_str());

		set_label("");
		write(GLOBAL, "main");
		write(EXTERN, "printf");
		write(EXTERN, "print_float");
		write(EXTERN, "alloc_heap");
		write(EXTERN, "heap_alloc_int");

		write_section(DATA);
		write_mem_def("int_format", DB, {"\"%d\"", 10, 0});
		write_mem_def("hex_format", DB, {"\"%#06x\"", 10, 0});
		write_mem_def("double_format", DB, {"\"%f\"", 10, 0});

		write_section(BSS);
		write_mem_res("heap_ptr", RESB, PTR_SIZE);

		write_section(TEXT);

		set_label("main");
		write(AND, Reg::ESP, -8);
		write(PUSH, Reg::EBP);
		write(MOV, Reg::EBP, Reg::ESP);
		m_StackMemAllocAdr = m_Code.at(m_CurrentLabel).size();

		write(CALL, "alloc_heap");
		write(MOV, { "heap_ptr", 0, DWORD }, Reg::EAX);
		write(PUSH, { "heap_ptr", 0, DWORD });

		for (auto node : nodes)
		{
			ValueType val = eval_expr(node);
			print_value(val);
		}

		write(MOV, Reg::ESP, Reg::EBP);
		write(POP, Reg::EBP);
		write(MOV, Reg::EAX, 1);
		write(MOV, Reg::EBX, 1);
		write(INT, 0x80);

		set_stack_mem();

	}
}

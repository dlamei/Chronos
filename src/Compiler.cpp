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
			case MOV: return "MOV";
			case NOP: return "NOP";
			case PUSH: return "PUSH";
			case POP: return "POP";
			case CALL: return "CALL";
			case ADD: return "ADD";
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

	std::string to_string(const std::variant<MemAccess, int, bool>& acc)
	{
		if (acc.index() == 0) return to_string(std::get<MemAccess>(acc));
		else if (acc.index() == 1) return std::to_string(std::get<int>(acc));
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

	void Compiler::write(InstType t, int a)
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
			write(MOV, Reg::EAX, std::get<int>(t.value));
			//write(PUSH, std::get<int>(t.value));
			//write(PUSH, { "heap_ptr", 0, DWORD });
			//write(CALL, "heap_alloc_int");
			//write(ADD, Reg::ESP, 8);
			return ValueType::INT_VAL;

		case TokenType::FLOAT:
			ASSERT(false, "float eval not implemented");
			return ValueType::UNKNOWN;

		default:
			ASSERT(false, "num node should not have this token" + to_string(t));
		}

		return ValueType::UNKNOWN;
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

	ValueType Compiler::eval_binop(Node* node)
	{
		ASSERT(node->type == NodeType::BINOP, "expected binop type");

		Node* right = std::get<BinOp>(node->value).right;
		Node* left = std::get<BinOp>(node->value).left;

		ValueType tmp_right = eval_expr(right);
		write(PUSH, Reg::EAX);
		ValueType tmp_left = eval_expr(left);

		if (is_ptr(tmp_left) && can_calc_temp(*left))
		{
			tmp_left = ValueType::INT_VAL;
			write(MOV, Reg::EAX, { Reg::EAX, HEADER_SIZE, DWORD });
		}
		if (is_ptr(tmp_left))
		{
			write(MOV, Reg::EAX, { Reg::EAX, HEADER_SIZE, DWORD });
		}

		if (!is_ptr(tmp_right))
		{
			write(POP, Reg::ECX);
		}
		else if (can_calc_temp(*right))
		{
			tmp_right = ValueType::INT_VAL;
			write(POP, Reg::ECX);
			write(MOV, Reg::ECX, { Reg::ECX, HEADER_SIZE , DWORD });
		}
		else
		{
			write(POP, Reg::ECX);
			write(MOV, Reg::ECX, { Reg::ECX, HEADER_SIZE , DWORD });
		}



		switch (std::get<BinOp>(node->value).type)
		{
		case TokenType::ADD:
			write(ADD, Reg::EAX, Reg::ECX);
			break;
		case TokenType::SUB:
			write(SUB, Reg::EAX, Reg::ECX);
			break;
		case TokenType::MULT:
			write(MUL, Reg::ECX);
			break;
		case TokenType::DIV:
			write(MOV, Reg::EDX, 0);
			write(DIV, Reg::ECX);
			break;

		default:
			ASSERT(false, "op type not defined");
			break;
		}

		bool is_int_val = false;

		if ((tmp_left == ValueType::INT_VAL) != (tmp_right == ValueType::INT_VAL))
		{
			is_int_val = true;
			write(PUSH, Reg::EAX);
			write(PUSH, { "heap_ptr", 0, DWORD });
			write(CALL, "heap_alloc_int");
			write(ADD, Reg::ESP, 8);
		}

		if (is_int_val) return ValueType::INT_VAL;
		else return tmp_left;

	}

	ValueType Compiler::eval_assing(NodeValues::AssignOp& op)
	{
		ValueType tmp = eval_expr(op.expr);

		if (is_ptr(tmp))
		{
			//Copy the value into eax
			write(MOV, Reg::EAX, { Reg::EAX, HEADER_SIZE, DWORD });
		}

		write(PUSH, Reg::EAX);
		write(PUSH, { "heap_ptr", 0, DWORD });
		write(CALL, "heap_alloc_int");
		write(ADD, Reg::ESP, 8);

		m_VarTable.insert({ op.var, m_BPOffset});
		write(MOV, { Reg::EBP, -m_BPOffset, DWORD }, Reg::EAX);
		m_BPOffset += PTR_SIZE;

		write(MOV, Reg::EAX, { Reg::EAX, 4, DWORD });

		return ValueType::INT_VAL;
	}

	ValueType Compiler::eval_access(std::string var)
	{
		if (m_VarTable.find(var) != m_VarTable.end())
		{
			write(MOV, Reg::EAX, { Reg::EBP, -m_VarTable.at(var), DWORD });
			return ValueType::UNKNOWN;
		}

		return ValueType::UNKNOWN;
	}

	ValueType Compiler::eval_expr(Node* node)
	{
		if (!node) return ValueType::UNKNOWN;

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
			return ValueType::UNKNOWN;
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
		write(EXTERN, "alloc_heap");
		write(EXTERN, "heap_alloc_int");

		write_section(DATA);
		write_mem_def("int_format", DB, {"\"%d\"", 10, 0});
		write_mem_def("hex_format", DB, {"\"%#06x\"", 10, 0});

		write_section(BSS);
		write_mem_res("heap_ptr", RESB, PTR_SIZE);

		write_section(TEXT);

		set_label("main");
		write(PUSH, Reg::EBP);
		write(MOV, Reg::EBP, Reg::ESP);
		m_StackMemAllocAdr = m_Code.at(m_CurrentLabel).size();

		write(CALL, "alloc_heap");
		write(MOV, { "heap_ptr", 0, DWORD }, Reg::EAX);
		write(PUSH, { "heap_ptr", 0, DWORD });

		for (auto node : nodes)
		{
			ValueType tmp = eval_expr(node);
			write(PUSH, Reg::EAX);
			if (tmp == ValueType::INT_VAL) print_top();
			else print_chint();
		}

		write(MOV, Reg::ESP, Reg::EBP);
		write(POP, Reg::EBP);
		write(MOV, Reg::EAX, 1);
		write(MOV, Reg::EBX, 1);
		write(INT, 0x80);

		set_stack_mem();

	}
}

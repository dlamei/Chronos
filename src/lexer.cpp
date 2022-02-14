#include "Lexer.h"
#include <iostream>
#include <string>
#include <stdio.h>

namespace Chronos
{
	bool is_space(char c)
	{
		switch (c)
		{
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			return true;
		default:
			return false;
		}
	}

	bool is_digit(char c)
	{
		switch (c)
		{
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			return true;
		default: return false;
		}
	}

	std::string to_string(const Position& pos)
	{
		std::string s = "indx: ";
		s += std::to_string(pos.index) + ", line: " + std::to_string(pos.line) + ", column: " + std::to_string(pos.column);
		return s;
	}

	std::string to_string(const TokenType t)
	{
		switch (t)
		{
		case TokenType::INT: return "INT";
		case TokenType::FLOAT: return "FLOAT";
		case TokenType::ADD: return "ADD";
		case TokenType::SUB: return "SUB";
		case TokenType::MULT: return "MULT";
		case TokenType::DIV: return "DIV";
		case TokenType::EQUAL: return "EQUAL";
		case TokenType::LESS: return "LESS";
		case TokenType::GREATER: return "GREATER";
		case TokenType::LESS_EQ: return "LESS_EQ";
		case TokenType::GREATER_EQ: return "GREATER_EQ";
		case TokenType::LROUND: return "LROUND";
		case TokenType::RROUND: return "RROUND";

		case TokenType::KW_AND: return "KW_AND";
		case TokenType::KW_OR: return "KW_OR";
		default: exit(-2);
		}
	}

	std::string to_string(const Token& t)
	{
		std::string s;

		switch (t.type)
		{
		case TokenType::INT:
			s += "INT(";
			s += std::to_string(t.value.ival);
			s += ")";
			break;
		case TokenType::FLOAT:
			s += "FLOAT(";
			s += std::to_string(t.value.fval);
			s += ")";
			break;
		case TokenType::ADD: 
		case TokenType::SUB: 
		case TokenType::MULT: 
		case TokenType::DIV: 
			return to_string(t.type);
		}

#ifdef PRINT_POS
		s += " start: " + to_string(t.start_pos);
		s += ", end: " + to_string(t.end_pos);
#endif

		return s;
	}


	void Lexer::load_text(const char* text, size_t size)
	{
		m_TextSize = size;
		m_Text = text;
		m_CharPtr = text;
	}


	void Lexer::advance()
	{
		if (m_CharPtr == nullptr) return;
		switch (*m_CharPtr)
		{
		case '\n':
			++m_Line;
			m_Column = 0;
			break;

		default:
			++m_Column;
			break;
		}


		if (m_Index >= m_TextSize)
		{
			m_CharPtr = nullptr;
		}
		else
		{
			++m_CharPtr;
			++m_Index;
		}
	}

	Token Lexer::peek()
	{
		return m_Tokens.back();
	}

	void Lexer::pop()
	{
		m_Tokens.pop_back();
	}

	void Lexer::parse_tokens()
	{
		while (m_CharPtr)
		{
			char c = *m_CharPtr;

			if (is_space(c))
			{
				advance();
				continue;
			}
			else if (is_digit(c))
			{
				m_Tokens.push_back(make_number());
				continue;
			}

			Position pos = { m_Index, m_Line, m_Column };

			switch (c)
			{
			case '+':
				m_Tokens.push_back(create_token(TokenType::ADD, 0, pos, pos));
				break;
			case '-':
				m_Tokens.push_back(create_token(TokenType::SUB, 0, pos, pos));
				break;
			case '*':
				m_Tokens.push_back(create_token(TokenType::MULT, 0, pos, pos));
				break;
			case '/':
				m_Tokens.push_back(create_token(TokenType::DIV, 0, pos, pos));
				break;
			}

			advance();
		}
	}

	Token Lexer::make_number()
	{
		std::string num;
		Position start = { m_Index, m_Line, m_Column };
		bool has_dot = false;

		while (m_CharPtr && (is_digit(*m_CharPtr) || *m_CharPtr == '.'))
		{
			if (*m_CharPtr == '.' && !has_dot)
			{
				has_dot = true;
			}
			else if (*m_CharPtr == '.' && has_dot)
			{
				break;
			}

			num += *m_CharPtr;
			advance();
		}

		Position end = { m_Index, m_Line, m_Column };

		if (has_dot)
		{
			TokenValue value;
			value.fval = std::stof(num);
			return create_token(TokenType::FLOAT, value, start, end);
		}
		else
		{
			TokenValue value;
			value.ival = std::stoi(num);
			return create_token(TokenType::INT, value, start, end);
		}
	}

	void Lexer::print_tokens()
	{
		std::cout << "size: " << m_Tokens.size() << "\n";
		for (Token& t : m_Tokens)
		{
			std::cout << to_string(t) << ", ";
		}
		std::cout << "\n";
	}
	void Lexer::clear()
	{
		m_Tokens.clear();
		m_Line = 0;
		m_Column = 0;
		m_Index = 0;

		m_TextSize = 0;
		m_Text = nullptr;
		m_CharPtr = nullptr;
	}
}

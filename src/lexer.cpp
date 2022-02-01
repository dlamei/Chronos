#include "lexer.h"
#include <iostream>
#include <string>

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


	std::string token_to_string(Token t)
	{
		switch (t.type)
		{
		case TokenType::INT:
		{
			std::string s;
			s += "INT(";
			s += std::to_string(t.value.ival);
			s += ")";
			return s;
		}
		case TokenType::FLOAT:
		{
			std::string s;
			s += "FLOAT(";
			s += std::to_string(t.value.fval);
			s += ")";
			return s;
		}
		case TokenType::ADD: return "ADD";
		case TokenType::SUB: return "SUB";
		case TokenType::MULT: return "MULT";
		case TokenType::DIV: return "DIV";
		}
		return "";
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


		if (m_Index > m_TextSize)
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
				advance();
				continue;
			}

			Position pos = { m_Index, m_Line, m_Column };

			switch (c)
			{
			case '+':
				m_Tokens.push_back(Token{ TokenType::ADD, 0, pos});
				break;
			case '-':
				m_Tokens.push_back(Token{ TokenType::SUB, 0, pos});
				break;
			case '*':
				m_Tokens.push_back(Token{ TokenType::MULT, 0, pos});
				break;
			case '/':
				m_Tokens.push_back(Token{ TokenType::DIV, 0, pos});
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
			Token t = { TokenType::FLOAT, value, start, end };
			return t;
		}
		else
		{
			TokenValue value;
			value.ival = std::stoi(num);
			Token t = { TokenType::INT, value, start, end };
			return t;
		}
	}

	void Lexer::print_tokens()
	{
		std::cout << "size: " << m_Tokens.size() << "\n";
		for (Token& t : m_Tokens)
		{
			std::cout << token_to_string(t) << "\n";
		}
	}
	void Lexer::clear_token()
	{
		m_Tokens.clear();
	}
}

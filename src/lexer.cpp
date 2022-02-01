#include <cstring>
#include "lexer.h"
#include <iostream>

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


	const char* token_to_string(Token t)
		{
		switch (t.type)
		{
			case INT: return "INT";
			case ADD: return "ADD";
			case SUB: return "SUB";
			case MULT: return "MULT";
			case DIV: return "DIV";
		}
		return nullptr;
	}


	void Lexer::load_text(const char* text, size_t size)
	{
		m_TextSize = size;
		m_Text = text;
		m_CharPtr = text;
	}


	void Lexer::advance()
	{
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


		if (m_CharPtr - m_Text > m_TextSize)
		{
			m_CharPtr = nullptr;
		} else
		{
			++m_CharPtr;
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
			} else if (is_digit(c))
			{
				advance();
				continue;
			}

			//std::cout << "char: " << c << "\n";
			switch (c)
			{
				case '+':
					m_Tokens.push_back(Token { TokenType::ADD });
					break;
				case '-':
					m_Tokens.push_back(Token { TokenType::SUB });
					break;
				case '*':
					m_Tokens.push_back(Token { TokenType::MULT });
					break;
				case '/':
					m_Tokens.push_back(Token { TokenType::DIV });
					break;
			}

			advance();
		}

	}

	void Lexer::print_tokens()
	{
		std::cout << "size: " << m_Tokens.size() << "\n";
		for (Token t : m_Tokens) 
		{
			std::cout << token_to_string(t) << "\n";
		}
	}
}

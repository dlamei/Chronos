#pragma once

#include <cstdint>
#include <deque>
#include <string>

#include "common.h"

namespace Chronos
{

	struct Position
	{
		std::size_t index = 0;
		std::size_t line = 0;
		std::size_t column = 0;
	};

	enum class TokenType
	{
		INT = 0,
		FLOAT,
		ADD,
		SUB,
		MULT,
		DIV,
		EQUAL,
		LESS,
		GREATER,
		LESS_EQ,
		GREATER_EQ,
		LROUND,
		RROUND,

		KW_AND,
		KW_OR,
	};

	union TokenValue
	{
		int32_t ival;
		float fval;
	};

	struct Token
	{
		TokenType type;
		TokenValue value;

#ifdef DEBUG
		Position start_pos;
		Position end_pos;
#endif
	};

#ifdef DEBUG
	#define create_token(type, value, start, end) { type, value, start, end}
	#define token_start(token) token.start_pos
	#define token_end(token) token.end_pos
#else
	#define create_token(type, value, start, end) { type, value }
	#define token_start(token)
	#define token_end(token)
#endif


	std::string to_string(const TokenType t);
	std::string to_string(const Token& t);
	std::string to_string(const Position& pos);

	class Lexer
	{
		private:
			std::deque<Token> m_Tokens;

			size_t m_Line = 0;
			size_t m_Column = 0;
			size_t m_Index = 0;

			std::size_t m_TextSize = 0;
			const char *m_Text = nullptr;
			const char *m_CharPtr = nullptr;


			void advance();

			Token make_number();

		public:

			Lexer() {}

			std::deque<Token> get_tokens()
			{
				return m_Tokens;
			}

			void load_text(const char* text, size_t size);

			void parse_tokens();
			void print_tokens();

			void clear();

			Token peek();
			void pop();

	};
}

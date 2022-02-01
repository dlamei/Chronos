#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace Chronos
{
	const char* DIGITS = "1234567890";
	const char* LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";


	struct Position
	{
		std::size_t index = 0;
		std::size_t line = 0;
		std::size_t column = 0;
	};

	enum TokenType
	{
		INT = 0,
		ADD,
		SUB,
		MULT,
		DIV,
	};
	

	union TokenValue
	{
		int32_t int_value;
		float float_vlaue;
	};

	struct Token
	{
		TokenType type;
		TokenValue value;

		Position start_pos;
		Position end_pos;
	};


	const char* token_to_string(Token t);

	class Lexer
	{
		private:
			std::size_t m_Line = 0;
			std::size_t m_Column = 0;

			std::size_t m_TextSize = 0;
			const char *m_Text;
			const char *m_CharPtr = nullptr;

			std::vector<Token> m_Tokens;

			void advance();

		public:

			Lexer() {}

			void load_text(const char* text, size_t size);

			void parse_tokens();
			void print_tokens();

			Token peek();
			void pop();

	};
}

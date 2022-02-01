#pragma once

#include <cstdint>
#include <vector>
#include <string>

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
	};

	union TokenValue
	{
		float fval;
		int32_t ival;
	};

	struct Token
	{
		TokenType type;
		TokenValue value;

		Position start_pos;
		Position end_pos;
	};


	std::string token_to_string(Token t);

	class Lexer
	{
		private:
			size_t m_Line = 0;
			size_t m_Column = 0;
			size_t m_Index = 0;

			std::size_t m_TextSize = 0;
			const char *m_Text = nullptr;
			const char *m_CharPtr = nullptr;

			std::vector<Token> m_Tokens;

			void advance();

			Token make_number();

		public:

			Lexer() {}

			void load_text(const char* text, size_t size);

			void parse_tokens();
			void print_tokens();

			void clear_token();

			Token peek();
			void pop();

	};
}

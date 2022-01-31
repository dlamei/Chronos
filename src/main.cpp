#include <iostream>
#include <string>
#include <stdio.h>
#include <cstdint>
#include <cstring>
#include <vector>


namespace Chronos
{
	enum TokenType
	{
		INT,
		FLOAT,
		ADD,
		SUB,
		MUL,
		DIV,
		LROUND,
		RROUND,
		END,
	};

	struct Position
	{
		size_t index = 0;
		size_t line = 0;
		size_t column = 0;
	};

	struct Token
	{
		TokenType type;
		Position start_pos;
		Position end_pos;
	};

	struct IntToken : Token
	{
		int value;

		IntToken()
			: Token { TokenType::INT }
		{}
	};

	struct Lexer
	{
		private:
			const char* text;
			size_t index = 0;
			size_t line = 0;
			size_t column = 0;
			const char* current_char;

		public:
			inline void load_file(const char* text)
			{
				this->text = text;
			}

			void advance()
			{
				if (current_char != nullptr)
				{
					switch (*current_char)
					{
						case '\n':
							++line;
							++index;
							break;

						default:
							++index;
							++column;
					}
				}

				if (index < strlen(text))
				{
					current_char = text + index;
				}

				++index;
			}

			//void parse_token() std::vector<>
	};
}

int main()
{

	Chronos::IntToken t = {};

	while (1)
	{
		printf("chronos > ");
		std::string buffer;
		std::getline(std::cin, buffer);
		std::cout << buffer << "\n";
	}
}

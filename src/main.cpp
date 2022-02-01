#include <iostream>
#include <string>
#include <cstdint>

#include "lexer.h"


int main()
{
	std::string buffer;
	Chronos::Lexer lexer;

	while (1)
	{
		printf("chronos > ");
		std::getline(std::cin, buffer);

		lexer.load_text(buffer.c_str(), buffer.size());
		lexer.parse_tokens();
		lexer.print_tokens();

	}
}

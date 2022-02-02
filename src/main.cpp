#include <iostream>
#include <string>
#include <cstdint>


#include "Lexer.h"
#include "Parser.h"


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
		Chronos::parse_tokens(lexer.m_Tokens);
		lexer.clear_token();


	}
}

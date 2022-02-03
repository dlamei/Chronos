#include <iostream>
#include <string>
#include <cstdint>


#include "Lexer.h"
#include "Parser.h"


int main()
{
	std::string buffer;
	Chronos::Lexer lexer;
	Chronos::Parser parser;

	while (1)
	{
		printf("chronos > ");
		std::getline(std::cin, buffer);

		lexer.load_text(buffer.c_str(), buffer.size());
		lexer.parse_tokens();
		lexer.print_tokens();

		parser.load_tokens(lexer.get_tokens());
		auto res = parser.parse_nodes();
		if (res) std::cout << "result: " << to_string(*res.get_result()) << "\n";
		else std::cout << "error: " << res.get_error() << "\n";

		lexer.clear();


	}
}

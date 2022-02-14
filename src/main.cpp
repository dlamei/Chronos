#include <iostream>
#include <string>
#include <cstdint>


#include "Lexer.h"
#include "Parser.h"
#include "Compiler.h"


int main()
{
	std::string buffer;
	Chronos::Lexer lexer;
	Chronos::Parser parser;
	Chronos::Compiler compiler;

	while (1)
	{
		printf("chronos > ");
		std::getline(std::cin, buffer);

		lexer.load_text(buffer.c_str(), buffer.size());
		lexer.parse_tokens();
		lexer.print_tokens();

		parser.load_tokens(lexer.get_tokens());
		Chronos::ParseResult res = parser.parse_nodes();
		//if (res) std::cout << "result: " << Chronos::to_string(*res.get_result()) << "\n";
		//else std::cout << "error: " << res.get_error() << "\n";



		if (res)
		{
			Chronos::Node* nodes = res.get_result();
			if (nodes) std::cout << "result: " << Chronos::to_string(*nodes) << "\n";
			compiler.compile("Chronos", nodes);

			compiler.close();
			Chronos::delete_nodes(nodes);
		}
		else std::cout << "error: " << res.get_error() << "\n";

		lexer.clear();
	}
}

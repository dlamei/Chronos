#include <iostream>
#include <string>
#include <cstdint>


#include "Lexer.h"
#include "Parser.h"
#include "Compiler.h"

extern "C"
{
	#include "chlib.h"
}


int main()
{
	//ch_heap* h = alloc_heap();
	//
	//for (int i = 0; i < 10; i++)
	//{
	//	ch_int* ptr = heap_alloc_int(h, i);
	//	if (ptr)
	//	{
	//		print_line_marks(h->blocks);
	//		std::cout << type_from_ptr(h->blocks, ptr) << "\n";
	//	}
	//}

	//std::cin.get();

	Chronos::FileManager fm;

	std::string buffer;
	Chronos::Lexer lexer;
	Chronos::Parser parser;
	Chronos::Compiler compiler;

	while (1)
	{
		printf("chronos > ");
		std::getline(std::cin, buffer);

		fm.add_line(buffer, "<STDIN>");

		lexer.load_text(buffer.c_str(), buffer.size());
		lexer.parse_tokens();
		lexer.print_tokens();

		if (lexer.has_error()) std::cout << lexer.get_error().generate_message(fm.get_files()) << "\n";

		parser.load_tokens(lexer.get_tokens());
		Chronos::ParseResult res = parser.parse_nodes();

		if (res)
		{
			Chronos::Node* nodes = res.get_result();
			if (nodes) std::cout << "result: " << Chronos::to_string(*nodes) << "\n";
			compiler.compile("Chronos", nodes);

			compiler.close();
			Chronos::delete_nodes(nodes);
		}
		else std::cout << "error: " << res.get_error().generate_message(fm.get_files()) << "\n";

		lexer.clear();
		fm.clear();
	}
}

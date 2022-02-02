#include "Parser.h"
#include <iostream>

namespace Chronos
{
	enum class Priority
	{
		NONE = -1,
		CONTINUE,
		ADD,
		MULT,
	};

	Priority get_token_priority(Token& t)
	{
		switch (t.type)
		{
		case TokenType::ADD:
			return Priority::ADD;
		case TokenType::MULT:
			return Priority::MULT;

		case TokenType::INT:
		case TokenType::FLOAT:
			return Priority::CONTINUE;

		default:
			return Priority::NONE;
		}
	}

	//3 * 4 + 4

	void print_tokens(const std::vector<Token>& tokens)
	{
		std::cout << "\n";
		for (const Token& t : tokens)
		{
			std::cout << token_to_string(t) << ", ";
		}

		std::cout << "\n";
	}

	/*

		3 * 4 + 5

			3
			* rec

				4
				+ drop return 4

			return 3 * 4
		3 * 4
		+ rec
			
			5
			return 5

		(3 * 4) + 5


		--------------------------------------------

		3 + 4 * 5

			3
			+ rec
				
				4
				* rec
					
					5
					return 5

				return 4 * 5

			return 3 + (4 * 5)

		--------------------------------------------------

		foo ( a , b )

		a = 3 + 6

			a
			= rec
				
				3
				+ rec
					
					6
					return 6

				return 3 + 6

			a = (3 + 6)

	*/

	Node parse_expression(std::vector<Token>& tokens, Priority p)
	{
	}

	void parse_tokens(std::vector<Token>& tokens)
	{
		parse_expression(tokens, Priority::NONE);
	}

}

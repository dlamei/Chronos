#pragma once

#include "common.h"
#include "Lexer.h"

namespace Chronos
{

	union NodeValue
	{
		int ival;
		float fval;
	};

	enum class NodeType
	{
		INT,
		FLOAT,
		BINOP,
	};

	struct Node
	{
		NodeType type;
		NodeValue value;

#ifdef DEBUG
		Position start_pos;
		Position end_pos;
#endif
	};

#ifdef DEBUG
	#define create_node(type, value, start, end) { type, value, start, end}
#else
	#define create_node(type, value, start, end) { type, value }
#endif

	void parse_tokens(std::vector<Token>& tokens);

}


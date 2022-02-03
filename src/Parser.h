#pragma once

#include <vector>
#include <functional>

#include "common.h"
#include "Lexer.h"
#include "Error.h"

namespace Chronos
{
	struct Node;

	union NodeValue
	{
		Token token;

		struct Binop_val
		{
			Node* left;
			TokenType type;
			Node* right;
		} binop_value;

		struct Unry_val
		{
			TokenType type;
			Node* left;
		};
	};

	enum class NodeType
	{
		NUM,
		BINOP,
		UNRYOP,
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
#define node_start(node) (node).start_pos
#define node_end(node) (node).end_pos
#define create_node(type, value, start, end) { type, value, start, end}
#else
#define node_start(node) 0
#define node_end(node) 0
#define create_node(type, value, start, end) { type, value }
#endif

	using ParseResult = Result<Node*, std::string>;

	std::string to_string(const Node& n);

	class Parser
	{
	private:
		std::deque<Token> m_Tokens = {};
		size_t m_TokenIndex = 0;
		Token* m_CurrentToken = nullptr;

		void advance();
		void retreat();

		ParseResult atom();
		ParseResult factor();
		ParseResult wrap_callable(Node* node);
		ParseResult callable();
		ParseResult term();
		ParseResult arith_expression();
		ParseResult comp_expression();
		ParseResult binop_expression(std::function<ParseResult(Parser*)> func_a, std::vector<TokenType> ops,
			std::function<ParseResult(Parser*)> func_b);
		ParseResult expression();

	public:
		void load_tokens(std::deque<Token> tokens)
		{
			m_Tokens = std::move(tokens);
			m_TokenIndex = 0;
			if (!m_Tokens.empty()) 
			{
				m_CurrentToken = &m_Tokens[0];
			}
		}

		ParseResult parse_nodes();

	};



}


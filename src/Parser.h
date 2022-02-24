#pragma once

#include <vector>
#include <functional>

#include "common.h"
#include "Lexer.h"
#include "Error.h"

namespace Chronos
{
	struct Node;

	void delete_nodes(Node* nodes);


	enum class NodeType
	{
		NUM,
		BINOP,
		UNRYOP,
		ASSIGN,
		ACCESS,
	};

	namespace NodeValues
	{
		struct UnryOp
		{
			TokenType type;
			Node* right;
		};

		struct AssignOp
		{
			std::string var;
			Node* expr;
		};

		struct BinOp
		{
			Node* left;
			TokenType type;
			Node* right;
		};
	}

	const short ParseOk = 1;
	const short ParseErr = 0;
	using ParseResult = std::variant<Error, Node*>;
	using NodeValue = std::variant<int, std::string, Token, NodeValues::UnryOp, NodeValues::AssignOp, NodeValues::BinOp, Node*>;

	struct Node
	{
		NodeType type;
		NodeValue value;

		Position start_pos;
		Position end_pos;
	};

	//using ParseResult = Result<Node*, Error>;


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


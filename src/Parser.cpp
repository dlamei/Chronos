#include <iostream>
#include <algorithm>
#include <stack>

#include "Parser.h"

namespace Chronos
{
	void print_tokens(const std::deque<Token>& tokens)
	{
		std::cout << "\n";
		for (const Token& t : tokens)
		{
			std::cout << to_string(t) << ", ";
		}

		std::cout << "\n";
	}

	void delete_nodes(Node* root)
	{
		if (!root) return; 

		std::stack<Node*> nodes;
		nodes.push(root);

		while (!nodes.empty())
		{
			Node* n = nodes.top();
			nodes.pop();

			switch (n->type)
			{
			case NodeType::NUM:
				break;

			case NodeType::UNRYOP:
				nodes.push(std::get<UnryValue>(n->value).right);
				break;

			case NodeType::BINOP:
				nodes.push(std::get<BinopValue>(n->value).right);
				nodes.push(std::get<BinopValue>(n->value).left);
				break;

			default:
				ASSERT(false, "delete for this type not defined");
				exit(-1);
			}

			delete n;
		}
	}

	std::string to_string(const Node& n)
	{
		std::string s;

		switch (n.type)
		{
		case NodeType::NUM:
			s += "NUM(";
			s += to_string(std::get<Token>(n.value));
			s += ")";
			break;
		case NodeType::BINOP:
			s += "BINOP(";
			s += to_string(*std::get<BinopValue>(n.value).left);
			s += ", " + to_string(std::get<BinopValue>(n.value).type) + ", ";
			s += to_string(*std::get<BinopValue>(n.value).right);
			s += ")";
			break;

		case NodeType::UNRYOP:
			s += "UNRYOP(";
			s += to_string(std::get<UnryValue>(n.value).type) + ", ";
			s += to_string(*std::get<UnryValue>(n.value).right);
			s += ")";
			break;

		default:
			exit(-1);

		}

#ifdef PRINT_POS
		s += " start: " + to_string(n.start_pos);
		s += ", end: " + to_string(n.end_pos);
#endif

		return s;
	}

	void Parser::advance()
	{
		++m_TokenIndex;

		if (m_TokenIndex < m_Tokens.size())
		{
			m_CurrentToken = &m_Tokens[m_TokenIndex];
		}
	}

	void Parser::retreat()
	{
		--m_TokenIndex;
		if (m_TokenIndex > 0)
		{
			m_CurrentToken = &m_Tokens[m_TokenIndex];
		}
	}

	ParseResult Parser::atom()
	{
		Token t = *m_CurrentToken;

		switch (t.type)
		{
		case TokenType::INT:
		case TokenType::FLOAT:
			advance();
			return { new Node({ NodeType::NUM, t, t.start_pos, t.end_pos }) };

		case TokenType::LROUND:
		{
			advance();
			ParseResult res = expression();
			if (!res) return res;
			Node* expr = res.get_result();

			if (m_CurrentToken->type == TokenType::RROUND)
			{
				advance();
				return { expr };
			}
			else {
				std::string details = "Parser: expected ')' found: " + to_string(t.type);
			Error e = { ErrorType::INVALID_SYNTAX, details, m_CurrentToken->start_pos, m_CurrentToken->end_pos };
			return e;
			}
		}

		default:
		{
			std::string details = "Parser: expected INT, FLOAT, IDENTIFIER, '+', '-', or '(', found: " + to_string(t.type);
			Error e = { ErrorType::INVALID_SYNTAX, details, t.start_pos, t.end_pos };
			return e;
		}
		}
	}

	ParseResult Parser::factor()
	{
		Token t = *m_CurrentToken;

		switch (t.type)
		{
		case TokenType::SUB:
		{
			advance();
			auto fac = factor();
			if (!fac) return fac;
			Node* fac_node = fac.get_result();
			Node* n = new Node({ NodeType::UNRYOP, {}, t.start_pos, fac_node->end_pos });
			n->value = UnryValue { TokenType::SUB, fac_node };
			return n;
		}

		default:
			return callable();
		}

	}

	ParseResult Parser::wrap_callable(Node* node)
	{
		if (m_CurrentToken->type == TokenType::LROUND)
		{
			//TODO
			exit(-1);
		}
		else
		{
			return { node };
		}
	}

	ParseResult Parser::callable()
	{
		auto res = atom();
		if (!res) return res;
		Node* res_node = res.get_result();
		return wrap_callable(res_node);
	}

	ParseResult Parser::term()
	{
		return binop_expression(&Parser::factor, { TokenType::MULT, TokenType::DIV }, &Parser::factor);
	}

	ParseResult Parser::arith_expression()
	{
		return binop_expression(&Parser::term, { TokenType::ADD, TokenType::SUB }, &Parser::term);
	}

	ParseResult Parser::comp_expression()
	{
		return binop_expression(&Parser::arith_expression, 
			{ TokenType::EQUAL, TokenType::LESS, TokenType::GREATER, TokenType::LESS_EQ, TokenType::GREATER_EQ },
			&Parser::arith_expression);
	}

	ParseResult Parser::binop_expression(std::function<ParseResult(Parser*)> func_a, std::vector<TokenType> ops,
		std::function<ParseResult(Parser*)> func_b)
	{
		auto left = func_a(this);
		if (!left) return left;
		Node* left_node = left.get_result();
		
		while (std::find(ops.begin(), ops.end(), m_CurrentToken->type) != ops.end())
		{
			Token op_token = *m_CurrentToken;
			advance();

			auto right = func_b(this);
			if (!right) return right;
			Node* right_node = right.get_result();

			Node* node = new Node({ NodeType::BINOP, {}, left_node->start_pos, right_node->end_pos });
			node->value = BinopValue { left_node, op_token.type, right_node };
			left_node = node;
		}

		return { left_node };
	}

	ParseResult Parser::expression()
	{
		return binop_expression(&Parser::comp_expression, { TokenType::KW_AND, TokenType::KW_OR }, &Parser::comp_expression);
	}

	ParseResult Parser::parse_nodes()
	{
		if (m_Tokens.empty()) return nullptr;
		auto exp = expression();
		if (!exp) return exp;
		return { exp.get_result() };
	}

}

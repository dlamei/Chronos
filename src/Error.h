#pragma once

#include <string>
#include <iostream>

#include "common.h"

namespace Chronos
{
	struct File
	{
		std::string name;
		std::string text;
	};

	class FileManager
	{
	private:
		std::vector<File> m_Files = {};
		size_t m_CurrentLine = -1;

	public:

		void add_file(std::string name, std::string text)
		{
			File file = { name, text };
			m_Files.push_back(file);
			m_CurrentLine = 0;
		}

		void add_line(std::string line, std::string file_name)
		{
			if (m_Files.empty() || m_Files.front().name != file_name)
			{
				add_file(file_name, line);
			}
			else
			{
				File& last = m_Files.front();
				last.text += line;
				++m_CurrentLine;
			}
		}
	};

	enum class ErrorType
	{
		ILLEGAL_CHAR = 0,
		EXPECTED_CHAR,
		INVALID_SYNTAX,
		RUNTIME,
		UNDEFINED_OPERATOR,
	};

	struct Error
	{
		ErrorType type;
		std::string details;

#ifdef DEBUG
		Position start_pos;
		Position end_pos;
#endif
	};


}

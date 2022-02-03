#pragma once

#include <string>
#include <iostream>

namespace Chronos
{
	template <typename T, typename U>
	class Result
	{
	private:
		union ResultType
		{
			T result;
			U error;

			ResultType(U s)
				: error(s) {}

			ResultType(T n)
				: result(n) {}

			~ResultType() {}

		} value;

		bool is_ok;

	public:

		Result()
			: value("undefined")
		{
			is_ok = false;
		}

		~Result() {}

		Result(const Result& other)
			: value("undefined")
		{
			is_ok = other.is_ok;
			if (other.is_ok)
			{
				value.result = other.value.result;
			}
			else
			{
				value.error = other.value.error;
			}
		}

		Result(T res)
			: value(res)
		{
			is_ok = true;
		}

		Result(U err)
			: value(err)
		{
			is_ok = false;
		}

		operator bool() const
		{
			return is_ok;
		}

		T get_result()
		{
			return value.result;
		}

		U get_error()
		{
			return value.error;
		}
	};

}

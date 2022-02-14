#pragma once

#define DEBUG
//#define PRINT_POS

#ifdef DEBUG
#include <cassert>
#   define ASSERT(condition, message) \
    do { \
        if (! (condition)) { \
            std::cerr << "Assertion `" #condition "` failed in " << __FILE__ \
                      << " line " << __LINE__ << ": " << message << std::endl; \
            std::terminate(); \
        } \
    } while (false)
#else
#   define ASSERT(condition, message)
#endif

template <typename T>
class Optional
{
private:
	T m_Value;
	bool m_HasValue;

public:
	Optional(T t)
		: m_Value(t), m_HasValue(true) {}

	Optional()
		: m_Value(), m_HasValue(false) {}

	bool has_value() { return m_HasValue; }
	T get_value() { return m_Value; }

	operator bool() const
	{
		return m_HasValue;
	}
};

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

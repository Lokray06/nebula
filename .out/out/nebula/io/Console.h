// nebula/io/Console.h
#ifndef NEBULA_IO_CONSOLE_H
#define NEBULA_IO_CONSOLE_H

#include <memory>
#include <string>
#include <vector>
#include "../core/Object.h"
#include "../core/String.h"

namespace nebula {
	namespace io {
		class Console : public nebula::core::Object {
		public:
			virtual ~Console() = default;
			Console();
			static void print(const std::shared_ptr<nebula::core::Object>& anything);
			static void print(const std::shared_ptr<nebula::core::String>& anyString);
			static void print(char character);
			static void print(int number);
			static void print(bool booleanValue);
			static void print(float floatValue);
			static void print(double doubleValue);
			static void println(const std::shared_ptr<nebula::core::Object>& anything);
			static void println(const std::shared_ptr<nebula::core::String>& anyString);
			static void println(char character);
			static void println(int number);
			static void println(bool booleanValue);
			static void println(float floatValue);
			static void println(double doubleValue);
		}; // class Console
	} // namespace io
} // namespace nebula

#endif // NEBULA_IO_CONSOLE_H

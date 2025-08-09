// nebula/io/Console.cpp
#include "nebula/io/Console.h"
#include <sstream>
#include <functional>

namespace nebula {
	namespace io {
		
		void Console::print(const std::shared_ptr<nebula::core::Object>& anything) {
			std::cout << (anything ? anything->toString()->_data : "null");
		}

		void Console::print(const std::shared_ptr<nebula::core::String>& anyString) {
			std::cout << (anyString ? anyString->_data : "null");
		}

		void Console::print(char character) {
			std::cout << character;
		}

		void Console::print(int number) {
			std::cout << number;
		}

		void Console::print(bool booleanValue) {
			std::cout << std::boolalpha << booleanValue;
		}

		void Console::print(float floatValue) {
			std::cout << floatValue;
		}

		void Console::print(double doubleValue) {
			std::cout << doubleValue;
		}

		void Console::println(const std::shared_ptr<nebula::core::Object>& anything) {
			std::cout << (anything ? anything->toString()->_data : "null") << std::endl;
		}

		void Console::println(const std::shared_ptr<nebula::core::String>& anyString) {
			std::cout << (anyString ? anyString->_data : "null") << std::endl;
		}

		void Console::println(char character) {
			std::cout << character << std::endl;
		}

		void Console::println(int number) {
			std::cout << number << std::endl;
		}

		void Console::println(bool booleanValue) {
			std::cout << std::boolalpha << booleanValue << std::endl;
		}

		void Console::println(float floatValue) {
			std::cout << floatValue << std::endl;
		}

		void Console::println(double doubleValue) {
			std::cout << doubleValue << std::endl;
		}

	} // namespace io
} // namespace nebula

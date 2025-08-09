// nebula/io/Console.cpp
#include "Console.h"
#include "../core/String.h"
#include "../core/Object.h"
#include <iostream>
#include <sstream>
#include <functional>

namespace nebula {
	namespace io {
		void nebula::io::Console::print(const std::shared_ptr<nebula::core::Object>& anything) {
			if (anything) { std::cout << anything->toString()->raw(); } else { std::cout << "null"; }
		}

		void nebula::io::Console::print(const std::shared_ptr<nebula::core::String>& anyString) {
			if (anyString) { std::cout << anyString->toString()->raw(); } else { std::cout << "null"; }
		}

		void nebula::io::Console::print(char character) {
			std::cout << character;
		}

		void nebula::io::Console::print(int number) {
			std::cout << number;
		}

		void nebula::io::Console::print(bool booleanValue) {
			std::cout << std::boolalpha << booleanValue;
		}

		void nebula::io::Console::print(float floatValue) {
			std::cout << floatValue;
		}

		void nebula::io::Console::print(double doubleValue) {
			std::cout << doubleValue;
		}

		void nebula::io::Console::println(const std::shared_ptr<nebula::core::Object>& anything) {
			if (anything) { std::cout << anything->toString()->raw(); } else { std::cout << "null"; }
			std::cout << std::endl;
		}

		void nebula::io::Console::println(const std::shared_ptr<nebula::core::String>& anyString) {
			if (anyString) { std::cout << anyString->toString()->raw(); } else { std::cout << "null"; }
			std::cout << std::endl;
		}

		void nebula::io::Console::println(char character) {
			std::cout << character;
			std::cout << std::endl;
		}

		void nebula::io::Console::println(int number) {
			std::cout << number;
			std::cout << std::endl;
		}

		void nebula::io::Console::println(bool booleanValue) {
			std::cout << std::boolalpha << booleanValue;
			std::cout << std::endl;
		}

		void nebula::io::Console::println(float floatValue) {
			std::cout << floatValue;
			std::cout << std::endl;
		}

		void nebula::io::Console::println(double doubleValue) {
			std::cout << doubleValue;
			std::cout << std::endl;
		}

	} // namespace io
} // namespace nebula

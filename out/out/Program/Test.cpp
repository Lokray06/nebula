// Program/Test.cpp
#include "Test.h"
#include <iostream>
#include <sstream>
#include <functional>

namespace Program
{
	Program::Test::Test() { } 

	void Program::Test::main() {
		std::shared_ptr<nebula::core::Object> testObject = std::make_shared<nebula::core::Object>();
		nebula::io::Console::print(std::make_shared<nebula::core::String>("Hello from Nebula!"));
		nebula::io::Console::println(testObject->toString());
		int b = 1;
		int a = b;
		a++;
		a -= 2;
		a %= 2;
		nebula::io::Console::print((someFunc(a) + a));
		nebula::io::Console::print(true);
		nebula::io::Console::print((5 > 2));
		someFunc(2);
	}

	int Program::Test::someFunc(int a) {
		if ((a > 1)) {
			nebula::io::Console::println(std::make_shared<nebula::core::String>("true"));
			return 0; // Add a return statement here
		} else {
			return -1;
		}

	} // namespace Program
}
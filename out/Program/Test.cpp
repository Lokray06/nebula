// Program/Test.cpp
#include "Test.h"
#include <iostream>
#include <sstream>
#include <functional>

namespace Program {
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
		for (int i = 0; (i < 1000000000); i++) {
		}
		int c = 2;
		switch (c) {
			case 1:
				nebula::io::Console::print(true);
				break;
			case 2:
				nebula::io::Console::print(std::make_shared<nebula::core::String>("dasfañsldkfñlakjsdf"));
				break;
		}
		nebula::io::Console::println((someFunc(a) + a));
		nebula::io::Console::println(true);
		nebula::io::Console::println((5 > 2));
		someFunc(2);
	}

	int Program::Test::someFunc(int a) {
		if ((a > 1)) {
		nebula::io::Console::println(true);
		return -1;
		} else {
		return -1;
		}
	}

} // namespace Program

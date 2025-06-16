// Program/Test.h
#ifndef PROGRAM_TEST_H
#define PROGRAM_TEST_H

#include <memory>
#include <string>
#include <vector>
#include "../nebula/core/Object.h"
// ADD THIS LINE:
#include "../nebula/io/Console.h"
// ADD THIS LINE (for full String definition needed by std::make_shared in Test.cpp):
#include "../nebula/core/String.h"

namespace Program {
	class Test : public nebula::core::Object {
	public:
		virtual ~Test() = default;
		Test();
		static void main();
		static int someFunc(int a);
	}; // class Test
} // namespace Program

#endif // PROGRAM_TEST_H
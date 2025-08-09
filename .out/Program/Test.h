// Program/Test.h
#ifndef PROGRAM_TEST_H
#define PROGRAM_TEST_H

#include <memory>
#include <string>
#include <vector>
#include <cmath>
#include <iostream>

#include "nebula/core/Object.h"

namespace nebula { namespace core { class Object; class String; } }

namespace Program {
	class Test : public nebula::core::Object {
	public:
		
		Test(); // Default constructor
		
		static void main() const;
	}; // class Test
} // namespace Program

#endif // PROGRAM_TEST_H

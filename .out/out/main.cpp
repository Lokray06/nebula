#include <iostream>
#include <memory>
#include "nebula/io/Console.h"
#include "nebula/core/Object.h"
#include "nebula/core/String.h"
#include "Program/Test.h"

// The C++ standard main function, which will call Nebula's main entry point.
int main() {
    Program::Test::main(); // Call the Nebula program's entry point
    return 0;
}

#include "system/Console.h"
#include "core/Object.h"
#include "core/String.h" // Ensure String is included if used directly

// Using declarations to make it easier to refer to classes, similar to Nebula's 'import'
using namespace nebula::io;
using namespace nebula::core;

namespace Program {

class Test {
public:
    static void main() {
        // In Nebula, 'new Object()' implies heap allocation. In C++, for simplicity
        // in this example, we'll use stack allocation for `testObject`.
        // For true heap allocation and management, `std::unique_ptr<Object> testObject = std::make_unique<Object>();`
        // or `std::shared_ptr<Object> testObject = std::make_shared<Object>();` would be used.
        Object testObject;

        // Using nebula::core::String for string literals
        Console::print(String("Hello from Nebula!"));
        Console::println(testObject.toString());

        int b = 1;
        int a = b;

        a++;    // a becomes 2
        a -= 2; // a becomes 0
        a %= 2; // a becomes 0 (0 % 2 is 0)

        // The result of `someFunc(a)` is an int, which is then added to `a`.
        // `someFunc(0)` returns -1. So, `-1 + 0` equals `-1`.
        Console::println(someFunc(a) + a);
        Console::println(true);
        Console::println(5 > 2); // This evaluates to `true`

        // `someFunc(2)` will print "true" but its return value is ignored here.
        someFunc(2);
    }

    // This function must return an `int` on all code paths in C++.
    // The original comment "//Can not retrurn" for the `if` branch is handled
    // by adding a placeholder return value (0 in this case) to ensure valid C++.
    static int someFunc(int a) {
        if (a > 1) {
            Console::println(String("true")); // Prints "true" followed by a newline
            return 0; // Placeholder return to satisfy C++'s requirement for non-void functions
        } else {
            return -1;
        }
    }
};

} // namespace Program

// The global main function, which is the entry point of the C++ application.
// It calls the `Program::Test::main()` method to start the Nebula program logic.
int main() {
    Program::Test::main();
    return 0;
}
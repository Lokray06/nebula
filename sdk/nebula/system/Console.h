#ifndef NEBULA_SYSTEM_CONSOLE_H
#define NEBULA_SYSTEM_CONSOLE_H

#include "../core/Object.h" // For Object type
#include "../core/String.h" // For String type
#include <iostream>             // For std::cout and std::endl

namespace nebula::io {

class Console {
public:
    // Static methods for printing to console (wrapper around std::cout).
    // These are overloaded to accept different Nebula types.

    // print methods (without newline)
    static void print(const nebula::core::Object& anything);
    static void print(const nebula::core::String& anyString);
    static void print(char character);
    static void print(int number);
    static void print(bool booleanValue);
    static void print(float floatValue);
    static void print(double doubleValue);

    // println methods (with newline)
    static void println(const nebula::core::Object& anything);
    static void println(const nebula::core::String& anyString);
    static void println(char character);
    static void println(int number);
    static void println(bool booleanValue);
    static void println(float floatValue);
    static void println(double doubleValue);
};

} // namespace nebula::io

#endif // NEBULA_SYSTEM_CONSOLE_H
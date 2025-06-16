#include "Console.h"

namespace nebula::io {

// print implementations
void Console::print(const nebula::core::Object& anything) {
    // Converts the Nebula Object to its String representation, then to std::string for printing.
    std::cout << static_cast<std::string>(anything.toString());
}
void Console::print(const nebula::core::String& anyString) {
    // Converts the Nebula String to std::string for printing.
    std::cout << static_cast<std::string>(anyString);
}
void Console::print(char character) {
    std::cout << character;
}
void Console::print(int number) {
    std::cout << number;
}
void Console::print(bool booleanValue) {
    // Uses std::boolalpha to print "true" or "false" instead of 1 or 0.
    std::cout << std::boolalpha << booleanValue;
}
void Console::print(float floatValue) {
    std::cout << floatValue;
}
void Console::print(double doubleValue) {
    std::cout << doubleValue;
}

// println implementations (add std::endl after printing)
void Console::println(const nebula::core::Object& anything) {
    std::cout << static_cast<std::string>(anything.toString()) << std::endl;
}
void Console::println(const nebula::core::String& anyString) {
    std::cout << static_cast<std::string>(anyString) << std::endl;
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

} // namespace nebula::io
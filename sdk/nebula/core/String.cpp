#include "String.h"
#include <functional> // For std::hash

namespace nebula::core {

String::String() : m_value("") {}
String::String(const char* chars) : m_value(chars ? chars : "") {}
String::String(const std::string& str) : m_value(str) {}
String::String(const String& other) : m_value(other.m_value) {}

int String::length() const {
    return static_cast<int>(m_value.length());
}

String String::operator+(const String& other) const {
    return String(m_value + other.m_value);
}

// Implementation for the virtual override
bool String::operator==(const Object& other) const {
    // This is where you implement the 'is' keyword logic.
    // Check if 'other' is actually a 'String' (or convertible to one)
    // using dynamic_cast or typeid.
    const String* otherString = dynamic_cast<const String*>(&other);
    if (otherString) {
        // If it is a String, perform the value comparison.
        return m_value == otherString->m_value;
    }
    // If 'other' is not a String, they cannot be equal in this context.
    return false;
}

// Implementation for the non-virtual overload (if you choose to keep it)
bool String::operator==(const String& other) const {
    return m_value == other.m_value;
}

String String::toString() const {
    return *this;
}

std::string String::hashCode() const {
    return std::to_string(std::hash<std::string>{}(m_value));
}

String::operator std::string() const {
    return m_value;
}

} // namespace nebula::core
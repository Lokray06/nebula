#ifndef NEBULA_CORE_STRING_H
#define NEBULA_CORE_STRING_H

#include "Object.h"
#include <string>
#include <memory>

namespace nebula::core {

class String : public Object {
private:
    std::string m_value;

public:
    String();
    String(const char* chars);
    String(const std::string& str);
    String(const String& other);

    int length() const;

    String operator+(const String& other) const;

    // This is the correct override: it must take a 'const Object&'
    // to match the base class virtual method.
    bool operator==(const Object& other) const override;

    // You might still want a specific overload for comparing with other String objects
    // if that's more convenient or common in your Nebula-like syntax.
    // This *does not* override the Object's operator==, but is a new overload.
    bool operator==(const String& other) const;


    String toString() const override;

    std::string hashCode() const override;

    operator std::string() const;
};

} // namespace nebula::core

#endif // NEBULA_CORE_STRING_H
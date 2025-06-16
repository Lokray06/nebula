#ifndef NEBULA_CORE_OBJECT_H
#define NEBULA_CORE_OBJECT_H

#include <string>
#include <typeinfo>

namespace nebula::core {

class String;

class Object {
public:
    // This virtual operator== takes a const Object&.
    // Derived classes will need to accept Object& to override this.
    // They can then dynamic_cast or use specific overloads for their own types.
    virtual bool operator==(const Object& other) const;

    virtual std::string hashCode() const;

    virtual String toString() const;

    virtual ~Object() = default;
};

} // namespace nebula::core

#endif // NEBULA_CORE_OBJECT_H
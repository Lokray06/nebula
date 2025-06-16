#include "Object.h"
#include "String.h" // Include String to define Object::toString

namespace nebula::core {

bool Object::operator==(const Object& other) const {
    // If 'is' means exact type comparison:
    if (typeid(*this) == typeid(other)) {
        // Default comparison for Object might not be meaningful in C++ without
        // specific identity or value properties.
        // For the base Object, we'll return false, assuming meaningful comparison
        // happens in derived classes based on their content.
        return false;
    }
    return false;
}

std::string Object::hashCode() const {
    // Returns a conceptual hash code, for real hash codes, it would involve
    // memory address or a more complex calculation based on state.
    return "0";
}

String Object::toString() const {
    // The default implementation returns the hash code as a String.
    return String(hashCode());
}

} // namespace nebula::core
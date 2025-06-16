// nebula/core/Object.cpp
#include "Object.h"
#include "String.h"
#include <iostream>
#include <sstream>
#include <functional>

namespace nebula {
	namespace core {
		bool Object::operator==(const std::shared_ptr<Object>& other) const {
			return this == other.get();
		}

		std::shared_ptr<String> Object::hashCode() const {
			std::stringstream ss;
			ss << static_cast<const void*>(this);
			return std::make_shared<String>(ss.str());
		}

		std::shared_ptr<String> Object::toString() const {
			return this->hashCode();
		}

	} // namespace core
} // namespace nebula

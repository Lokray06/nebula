// nebula/core/Object.cpp
#include "nebula/core/Object.h"
#include <sstream>
#include <functional>

namespace nebula {
	namespace core {
		
		bool Object::operator==(const std::shared_ptr<nebula::core::Object>& other) {
			return this == other.get();
		}

		std::shared_ptr<nebula::core::String> Object::hashCode() {
			return std::make_shared<nebula::core::String>([](const void* p) { std::stringstream ss; ss << p; return ss.str(); }(this));
		}

		std::shared_ptr<nebula::core::String> Object::toString() {
			return this->hashCode();
		}

	} // namespace core
} // namespace nebula

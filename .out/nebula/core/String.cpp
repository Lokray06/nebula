// nebula/core/String.cpp
#include "nebula/core/String.h"
#include <sstream>
#include <functional>

namespace nebula {
	namespace core {
		
		int String::length() {
			return static_cast<int>(this->_data.length());
		}

		std::shared_ptr<nebula::core::String> String::operator+(const std::shared_ptr<nebula::core::String>& other) {
			return std::make_shared<nebula::core::String>(this->_data + (other ? other->_data : ""));
		}

		bool String::operator==(const std::shared_ptr<nebula::core::Object>& other) {
			return return (([&]() { auto other_str = std::dynamic_pointer_cast<nebula::core::String>(other); if (other_str) { return this->_data == other_str->_data; } return false; })());;
		}

		std::shared_ptr<nebula::core::String> String::toString() {
			return std::make_shared<nebula::core::String>(this->_data);
		}

		std::shared_ptr<nebula::core::String> String::hashCode() {
			return std::make_shared<nebula::core::String>(std::to_string(std::hash<std::string>{}(this->_data)));
		}

	} // namespace core
} // namespace nebula

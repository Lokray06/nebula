// nebula/core/String.cpp
#include "String.h"
#include <iostream>
#include <sstream>
#include <functional>

namespace nebula {
	namespace core {
		String::String() : _data("") {}

		String::String(const std::string& raw_str) : _data(raw_str) {}

		String::String(const std::shared_ptr<nebula::core::String>& other) : _data(other ? other->raw() : "") { /* User constructor logic */ }

		int String::length() const {
			return static_cast<int>(_data.length());
		}

		std::shared_ptr<String> String::operator+(const std::shared_ptr<String>& other) {
			return std::make_shared<String>(this->_data + (other ? other->raw() : ""));
		}

		bool String::operator==(const std::shared_ptr<Object>& other) const {
			auto other_str = std::dynamic_pointer_cast<String>(other);
			if (other_str) { return this->_data == other_str->_data; }
			return false;
		}

		bool String::operator==(const std::shared_ptr<String>& other) const {
			if (other) { return this->_data == other->_data; }
			return false;
		}

		std::shared_ptr<String> String::toString() const {
			return std::make_shared<String>(this->_data);
		}

		std::shared_ptr<String> String::hashCode() const {
			return std::make_shared<String>(std::to_string(std::hash<std::string>{}(this->_data)));
		}

		const std::string& String::raw() const {
			return _data;
		}

	} // namespace core
} // namespace nebula

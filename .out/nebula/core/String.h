// nebula/core/String.h
#ifndef NEBULA_CORE_STRING_H
#define NEBULA_CORE_STRING_H

#include <memory>
#include <string>
#include <vector>
#include <cmath>
#include <iostream>

#include "nebula/core/Object.h"

namespace nebula { namespace core { class Object; class String; } }

namespace nebula {
	namespace core {
		class String : public nebula::core::Object {
		public:
			
			String();
			String(const std::shared_ptr<nebula::core::String>& other);
			
			int length();
			std::shared_ptr<nebula::core::String> operator+(const std::shared_ptr<nebula::core::String>& other);
			bool operator==(const std::shared_ptr<nebula::core::Object>& other);
			std::shared_ptr<nebula::core::String> toString();
			std::shared_ptr<nebula::core::String> hashCode();
		}; // class String
	} // namespace core
} // namespace nebula

#endif // NEBULA_CORE_STRING_H

// nebula/core/Object.h
#ifndef NEBULA_CORE_OBJECT_H
#define NEBULA_CORE_OBJECT_H

#include <memory>
#include <string>
#include <vector>
#include <cmath>
#include <iostream>


namespace nebula { namespace core { class Object; class String; } }

namespace nebula {
	namespace core {
		class Object {
		public:
			
			
			bool operator==(const std::shared_ptr<nebula::core::Object>& other);
			std::shared_ptr<nebula::core::String> hashCode();
			std::shared_ptr<nebula::core::String> toString();
		}; // class Object
	} // namespace core
} // namespace nebula

#endif // NEBULA_CORE_OBJECT_H

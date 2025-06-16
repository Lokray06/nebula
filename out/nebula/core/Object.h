// nebula/core/Object.h
#ifndef NEBULA_CORE_OBJECT_H
#define NEBULA_CORE_OBJECT_H

#include <memory>
#include <string>
#include <vector>
namespace nebula { namespace core { class String; } } // Forward Declaration

namespace nebula {
	namespace core {
		class Object {
		public:
			virtual ~Object() = default;
			virtual bool operator==(const std::shared_ptr<nebula::core::Object>& other) const;
			virtual std::shared_ptr<nebula::core::String> hashCode() const;
			virtual std::shared_ptr<nebula::core::String> toString() const;
		}; // class Object
	} // namespace core
} // namespace nebula

#endif // NEBULA_CORE_OBJECT_H

// nebula/core/String.h
#ifndef NEBULA_CORE_STRING_H
#define NEBULA_CORE_STRING_H

#include <memory>
#include <string>
#include <vector>
#include "Object.h"

namespace nebula {
	namespace core {
		class String : public nebula::core::Object {
		private:
			std::string _data;
		public:
			virtual ~String() = default;
			String();
			String(const std::string& raw_str);
			String(const std::shared_ptr<nebula::core::String>& other);
			int length() const;
			std::shared_ptr<nebula::core::String> operator+(const std::shared_ptr<nebula::core::String>& other);
			virtual bool operator==(const std::shared_ptr<nebula::core::Object>& other) const override;
			bool operator==(const std::shared_ptr<nebula::core::String>& other) const;
			virtual std::shared_ptr<nebula::core::String> toString() const override;
			virtual std::shared_ptr<nebula::core::String> hashCode() const override;
			const std::string& raw() const;
		}; // class String
	} // namespace core
} // namespace nebula

#endif // NEBULA_CORE_STRING_H

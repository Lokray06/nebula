// File: nebula_runtime.cpp
#include <string>
#include <vector>
#include <sstream>
#include <memory>
#include <cstring> // For strlen, strcpy
#include <cstdio>  // For snprintf

/**
 * This struct MUST match the layout of your 'nebula.core.String' class
 * as defined in the LLVMIRGenerator's DeclarationVisitor.
 * For now, it's a simple wrapper around a C-style string.
 */
struct NebulaString {
    const char* c_str;
};

// 'extern "C"' is crucial. It prevents C++ from changing the function names,
// so that our LLVM code can find them using the simple names we define.
extern "C" {

    /**
     * A helper to create a new NebulaString on the heap from a C-style string.
     * This is the central factory for all toString methods.
     */
    NebulaString* create_nebula_string(const char* initial_value) {
        // Allocate memory for the NebulaString struct itself
        NebulaString* str_obj = new NebulaString();

        // Allocate memory for the character data and copy it
        char* str_data = new char[strlen(initial_value) + 1];
        strcpy(str_data, initial_value);

        // Point the struct's member to the newly allocated character data
        str_obj->c_str = str_data;
        return str_obj;
    }

    /**
     * Calculates the length of a NebulaString's underlying C-string.
     */
    int string_length(NebulaString* str_obj) {
        if (str_obj == nullptr || str_obj->c_str == nullptr) {
            return 0;
        }
        return strlen(str_obj->c_str);
    }

    // --- toString Implementations for Primitive Types ---

    NebulaString* int32_toString(int value) {
        std::string str = std::to_string(value);
        return create_nebula_string(str.c_str());
    }

    NebulaString* int64_toString(long long value) {
        std::string str = std::to_string(value);
        return create_nebula_string(str.c_str());
    }

    NebulaString* uint64_toString(unsigned long long value) {
        std::string str = std::to_string(value);
        return create_nebula_string(str.c_str());
    }

    NebulaString* double_toString(double value) {
        std::string str = std::to_string(value);
        return create_nebula_string(str.c_str());
    }

    NebulaString* bool_toString(bool value) {
        return create_nebula_string(value ? "true" : "false");
    }

    NebulaString* char_toString(int value) {
        // Nebula 'char' is i32, so we handle it as a single character string
        char str[2] = { (char)value, '\0' };
        return create_nebula_string(str);
    }

    // --- toString for Objects (returns memory address) ---
    NebulaString* object_toString(void* obj_ptr) {
        char buffer[20];
        snprintf(buffer, sizeof(buffer), "%p", obj_ptr);
        return create_nebula_string(buffer);
    }

    // --- String Concatenation ---
    NebulaString* string_concat(NebulaString* left, NebulaString* right) {
        std::string s1 = left ? left->c_str : "";
        std::string s2 = right ? right->c_str : "";
        std::string result = s1 + s2;
        return create_nebula_string(result.c_str());
    }
}
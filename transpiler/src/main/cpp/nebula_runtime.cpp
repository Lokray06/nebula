#include <string>
#include <vector>
#include <sstream>
#include <memory>
#include <cstring> // For strlen, strcpy
#include <cstdio>  // For snprintf
#include <atomic>  // For atomic reference counting

/**
 * A header that will be the first member of any heap-allocated,
 * reference-counted Nebula object.
 */
struct NebulaObjectHeader {
    std::atomic<int32_t> ref_count;
};

/**
 * The NebulaString struct, now with a reference-counting header.
 * This MUST match the layout in the LLVMIRGenerator: { i32, i8* }
 */
struct NebulaString {
    NebulaObjectHeader header;
    const char* c_str;
};

// 'extern "C"' is crucial for linking with LLVM IR.
extern "C" {

    // --- Core Memory Management Functions ---

    /**
     * Increments the reference count of an object.
     * This should be called whenever a reference is copied.
     */
    void nebula_retain(void* obj_ptr) {
        if (obj_ptr == nullptr) return;
        auto* header = static_cast<NebulaObjectHeader*>(obj_ptr);
        header->ref_count.fetch_add(1);
    }

    /**
     * Decrements the reference count of an object.
     * If the count reaches zero, it deallocates the object.
     */
    void nebula_release(void* obj_ptr) {
        if (obj_ptr == nullptr) return;

        auto* header = static_cast<NebulaObjectHeader*>(obj_ptr);
        // If this was the last reference, proceed to delete.
        if (header->ref_count.fetch_sub(1) == 1) {
            // For now, we only have NebulaString, so we can cast directly.
            // In the future, this would involve a vtable or type tag
            // to call the correct destructor.
            NebulaString* str_obj = static_cast<NebulaString*>(obj_ptr);
            delete[] str_obj->c_str; // Free the character data
            delete str_obj;          // Free the struct itself
        }
    }

    // --- String Factory and Operations ---

    /**
     * Creates a new NebulaString on the heap with a reference count of 1.
     * The caller "owns" this new reference.
     */
    NebulaString* create_nebula_string(const char* initial_value) {
        if (initial_value == nullptr) {
            initial_value = "";
        }
        NebulaString* str_obj = new NebulaString();
        str_obj->header.ref_count = 1; // Start with one reference

        char* str_data = new char[strlen(initial_value) + 1];
        strcpy(str_data, initial_value);

        str_obj->c_str = str_data;
        return str_obj;
    }

    /**
     * Calculates the length of a NebulaString's C-string.
     */
    int string_length(NebulaString* str_obj) {
        if (str_obj == nullptr || str_obj->c_str == nullptr) {
            return 0;
        }
        return strlen(str_obj->c_str);
    }

    // --- toString Implementations (all now return owned references) ---

    NebulaString* int32_toString(int32_t value) {
        return create_nebula_string(std::to_string(value).c_str());
    }

    NebulaString* int64_toString(int64_t value) {
        return create_nebula_string(std::to_string(value).c_str());
    }

    NebulaString* double_toString(double value) {
        return create_nebula_string(std::to_string(value).c_str());
    }

    NebulaString* bool_toString(bool value) {
        return create_nebula_string(value ? "true" : "false");
    }

    NebulaString* char_toString(int32_t value) {
        char str[2] = { static_cast<char>(value), '\0' };
        return create_nebula_string(str);
    }

    NebulaString* object_toString(void* obj_ptr) {
        char buffer[20];
        snprintf(buffer, sizeof(buffer), "%p", obj_ptr);
        return create_nebula_string(buffer);
    }

    /**
     * Concatenates two strings.
     * IMPORTANT: This function "consumes" its arguments, meaning it takes
     * ownership of the references passed to it. It calls nebula_release on them.
     * It returns a new string with a reference count of 1.
     */
    NebulaString* string_concat(NebulaString* left, NebulaString* right) {
        std::string s1 = left ? left->c_str : "";
        std::string s2 = right ? right->c_str : "";
        std::string result = s1 + s2;

        // Release the input strings as they have been consumed.
        nebula_release(left);
        nebula_release(right);

        // Return a new string with its own +1 reference count.
        return create_nebula_string(result.c_str());
    }
}
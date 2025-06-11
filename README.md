# Nebula: High-Level Expressiveness, Native Performance.

![Nebula Logo Placeholder](https://placehold.co/800x200/020202/FFFFFF?text=Nebula)

Nebula is a modern, statically-typed, object-oriented programming language designed for developers who demand both the elegant syntax of high-level languages and the raw speed of native code. By intelligently transpiling directly to optimized C/C++, Nebula empowers you to build performant, reliable, and portable applications without compromising on developer experience.

## ✨ Why Nebula?

Tired of choosing between developer productivity and blazing-fast execution? Nebula offers a compelling alternative:

* **⚡️ Native Performance:** Zero VM overhead. Your Nebula code becomes optimized C/C++ binaries, delivering near-native speed and predictable resource usage.
* **✍️ Clean & Familiar Syntax:** Borrowing the best from Java and C#, Nebula provides intuitive class structures, properties, namespaces, and modern language constructs that feel instantly familiar.
* **🎯 Strong Typing & Compile-Time Safety:** Catch errors early with a robust static type system, ensuring more reliable applications.
* **🚀 Cross-Platform Potential:** Write once, transpile, and compile for any platform supported by C++ compilers.
* **🛠️ Productive Tooling Philosophy:** Designed for a streamlined development workflow, from powerful loop constructs to expressive error handling.

## 💡 How It Works

Nebula isn't an interpreted language or one that runs on a custom virtual machine. Instead, it leverages the power of existing C/C++ compilers through a sophisticated **transpilation process**:

1.  **Nebula Source (`.neb`)**: You write your application code in Nebula, benefiting from its high-level features and clear syntax.
2.  **The Nebula Transpiler**: Our custom-built transpiler takes your `.neb` files and converts them into semantically equivalent, highly optimized C/C++ `.hpp` and `.cpp` source files. This includes transforming Nebula's advanced features (like C#-style properties, specific loop syntaxes, and optional parameters) into idiomatic C++ constructs.
3.  **Nebula JDK Integration**: A significant portion of Nebula's standard library (the "JDK") is actually written in Nebula itself! This is then transpiled to C++ and linked with a minimal C++ runtime that handles the absolute lowest-level operations.
4.  **Native C++ Compilation**: The generated C/C++ code, along with the transpiled Nebula JDK and the minimal runtime, is then compiled by your standard C/C++ compiler (GCC, Clang, MSVC, etc.) into a lean, native executable.

The result is a self-contained binary that runs directly on your target system, delivering the performance you need without the overhead of a large runtime environment.

## 🚀 Key Features

Here's a closer look at what Nebula brings to the table:

* **Object-Oriented Programming (OOP)**
    * **Classes, Fields, Methods, Constructors:** Standard OOP building blocks with `public` and `private` access modifiers.
    * **Getters and Setters (C#-style Properties):** Define concise properties with underlying backing fields and custom logic for data access.
    * **Inheritance (`extends`):** Facilitates code reuse and establishes "is-a" relationships.
    * **Interfaces (`interface`, `implements`):** Define contracts for polymorphic behavior, allowing multiple interface implementation.
    * **Abstract Classes and Methods:** Create base classes with abstract behavior that must be implemented by subclasses.

* **Code Organization**
    * **Namespaces:** Organize your code logically to prevent naming collisions and improve modularity, similar to C# namespaces.

* **Advanced Type System & Method Design**
    * **Explicit Static Type System:** Strong typing for early error detection and enhanced reliability.
    * **Method Overloading by Return Type:** Uniquely allows methods to be overloaded based on their return type, enabling more precise function definitions (e.g., `float getPi()` and `double getPi()`).

* **Flexible Control Flow**
    * **Standard Loops (`for`, `while`, `do-while`):** All the traditional looping constructs you'd expect.
    * **Simplified `for` Loops:** Convenient shorthand like `for (i < 10)` (iterates `i` from 0 to 9) and `for (j = 5 < 15)` (iterates `j` from 5 to 14), automatically handling initialization and increment/decrement.

* **Modern Language Constructs**
    * **Operator Overloading:** Define custom behavior for standard operators on your own classes, making code more intuitive (e.g., `Vector2 + Vector2`).
    * **Tuples:** Easily group and return multiple, possibly different types of values from methods.
    * **Optional Parameters:** Define method and constructor parameters that can be omitted by the caller. Values for omitted parameters are either `null` (for reference types) or a specified default, easily handled with the `??` (null-coalescing) operator.

* **Application Structure**
    * **`main` Entry Point:** Standardized `public static void main()` method for application startup.

* **Developer Convenience**
    * **Alias Feature:** Create `global` or `local` aliases for long or frequently used method calls (e.g., `global alias println = System.Console.WriteLine;`).

* **Robust Error Handling**
    * **`try-catch-finally` Blocks:** Familiar structure for handling exceptions and ensuring cleanup.
    * **Exception Hierarchy:** Root `Throwable` with `Error` for unrecoverable issues and `Exception` for recoverable ones (all `Exception` subclasses are unchecked for simplicity).
    * **Result Types for Expected Errors:** Encourages the use of `Result<T, E>` (similar to Rust) for anticipated failure scenarios, making error handling explicit and part of the normal control flow.
    * **RAII Philosophy:** Leverages C++'s Resource Acquisition Is Initialization for efficient and automatic resource cleanup, complementing `finally` blocks.

## 🏁 Getting Started (Coming Soon!)

We're actively developing Nebula! Detailed instructions on how to set up the transpiler, write your first Nebula program, and compile it to native code will be provided here.

## 🤝 Contributing

Nebula is an open-source project. We welcome contributions from the community! If you're interested in language design, compiler development, or building standard libraries, please check our contribution guidelines (coming soon) and open issues.

## 📄 License

Nebula is released under the [MIT License](LICENSE)

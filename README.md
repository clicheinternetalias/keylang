KeyLang: A Scripting Language in the Key of C
=============================================

KeyLang was designed to be small, simple, and easy to integrate and use. No attempts have been made to support any of the latest whiz-bang paradigms or theoretical stuff, just the basics of procedural programming. The primary design goal was to avoid malloc or GC during script execution.

Key features of KeyLang:

* Simple syntax, similar to C, C++, Java, Javascript, etc., etc. (Well, not similar to C++. Nobody wants to be similar to C++.)
* No run-time malloc or GC. The only allocations are during compilation or by user-defined functions.
* No objects. No closures. No co-routines. No clever, mathy, or weird stuff. Just good old-fashioned procedural programming.
* No arrays, hash-maps, collections, lists, or sequences. No file I/O.
* No threading support, but scripts can be paused and resumed, which is close enough.
* Dynamic yet strict typing. Only integers, strings, and undefined are built-in.
* Strings are atomic, immutable, interned, and mostly encoding-agnostic. (Anything consistent with ASCII, including UTF-8.)
* No FFI, but easy-enough integration with native code and data.
* User-defined operators, functions, and types.
* Easy to get up and running; the API isn't full of constructors for stuff that will wind up being the same in every program anyway.
* No actual boolean type, but undefined, zero, the empty string, and NULL custom types are false.
* Great for video games. (Hopefully. I haven't actually used this for anything yet.)

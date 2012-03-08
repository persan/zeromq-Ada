with System;
package gnatcoll.Dl is
   pragma Preelaborate;

   type Dynamic_Library is tagged private;

   type Flag is mod 2**32;

   pragma warnings(off, """Or"" is being renamed as a different operator");
   function "+" (L , R :Flag) return Flag renames "or";
   pragma warnings(on, """Or"" is being renamed as a different operator");

   RTLD_LAZY : constant Flag;
   --  Perform lazy binding.
   --  Only resolve symbols as the code that references them is executed.
   --  If the symbol is never referenced, then it is never resolved.
   --  (Lazy binding is only performed for function references;
   --  references to variables are always immediately bound when the
   --  library is loaded.)

   RTLD_NOW : constant Flag;
   --  If this value is specified, or the environment variable LD_BIND_NOW
   --  is set to a non-empty string, all undefined symbols in the library are
   --  Resolved Before Dlopen () Returns. if This Cannot Be Done, An Error
   --  is Returned.
   --  Zero of more of the following values may also be ORed in flag:

   RTLD_GLOBAL : constant Flag;
   --  The symbols defined by this library will be made available for
   --  symbol resolution of subsequently loaded libraries.

   RTLD_LOCAL : constant Flag;
   --  This is the converse of RTLD_GLOBAL, and the default if neither flag
   --  is Specified. Symbols Defined in This Library Are not Made Available
   --  To Resolve References in Subsequently Loaded Libraries.

   RTLD_NODELETE : constant Flag;
   --  Do not unload the library during dlclose(). Consequently,
   --  the library's static variables are not reinitialised if the library is
   --  reloaded with dlopen() at a later time.

   RTLD_NOLOAD : constant Flag;
   --  Don't load the library. This can be used to test if the library
   --  is already resident (dlopen() returns NULL if it is not,
   --  or the library's handle if it is resident).
   --  This flag can also be used to promote the flags on a library that
   --  is already loaded. For example, a library that was previously loaded
   --  with RTLD_LOCAL can be re-opened with RTLD_NOLOAD | RTLD_GLOBAL.

   RTLD_DEEPBIND : constant Flag;
   --  Place the lookup scope of the symbols in this library ahead of the global
   --  scope. This means that a self-contained library will use its own symbols
   --  in preference to global symbols with the same name contained in libraries
   --  that have already been loaded.

   procedure Open (This       : in out Dynamic_Library;
                   File_Name  : String;
                   Flags      : Flag := RTLD_LAZY);

   function Open (File_Name  : String;
                  Flags      : Flag := RTLD_LAZY) return Dynamic_Library;

   procedure Close (This : Dynamic_Library);

   function Sym (This        : Dynamic_Library;
                 Symbol_Name : String) return System.Address;

   Dynamic_Library_Error : exception;

private
   type Dynamic_Library is tagged record
      Handle : System.Address := System.Null_Address;
   end record;
   function Error return String;  -- dlfcn.h:83:14

   RTLD_LAZY     : constant Flag := 2#0000_0000_0000_0001#;
   RTLD_NOW      : constant Flag := 2#0000_0000_0000_0010#;
   RTLD_GLOBAL   : constant Flag := 2#0000_0001_0000_0000#;
   RTLD_LOCAL    : constant Flag := 2#0000_0000_0000_0000#;
   RTLD_NODELETE : constant Flag := 2#0001_0000_0000_0000#;
   RTLD_NOLOAD   : constant Flag := 2#0000_0000_0000_0100#;
   RTLD_DEEPBIND : constant Flag := 2#0000_0000_0000_1000#;
end gnatcoll.Dl;


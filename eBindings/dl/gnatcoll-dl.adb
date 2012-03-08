with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
package body gnatcoll.dl is

   package dlfcn_h is

      function dlopen (arg1 : System.Address; arg2 : Flag) return System.Address;  -- dlfcn.h:57:14
      pragma Import (C, dlopen, "dlopen");

      function dlclose (arg1 : System.Address) return int;  -- dlfcn.h:61:12
      pragma Import (C, dlclose, "dlclose");

      function dlsym (arg1 : System.Address; arg2 : System.Address) return System.Address;  -- dlfcn.h:65:14
      pragma Import (C, dlsym, "dlsym");

      function dlerror return Interfaces.C.Strings.chars_ptr;  -- dlfcn.h:83:14
      pragma Import (C, dlerror, "dlerror");

   end dlfcn_h;
   use dlfcn_h;
   use type system.Address;

   procedure Open (This       : in out Dynamic_Library;
                   File_Name  : String;
                   Flags      : Flag := RTLD_LAZY) is
      L_name : aliased constant string := File_Name & ASCII.NUL;
   begin
      this.handle := dlopen (L_name'Address, Flags);
      if this.handle = System.Null_Address then
         raise Dynamic_Library_Error  with error;
      end if;
   end;

   function Open (File_Name  : String;
                  Flags      : Flag := RTLD_LAZY) return Dynamic_Library is
   begin
      return  ret : Dynamic_Library do
         open(ret,File_Name,Flags);
      end return;
   end;


   procedure close (this : dynamic_Library) is
      ret : int;
   begin
      ret := dlclose (this.handle);
      if ret /= 0 then
         raise Dynamic_Library_Error  with error;
      end if;
   end;

   function sym (this : dynamic_Library; Symbol_name : String) return System.Address is
      L_name : aliased constant string := Symbol_name & ASCII.NUL;
   begin
      return ret : System.Address do
         ret := dlsym (this.handle, L_name'Address);
         if ret = System.Null_Address then
            raise Dynamic_Library_Error  with error;
         end if;
      end return;
   end;

   function error return String is
   begin
      return Interfaces.C.Strings.Value (dlerror);
   end;

end gnatcoll.dl;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
package body dl is

   package dlfcn_h is

      function dlopen (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : int) return System.Address;  -- dlfcn.h:57:14
      pragma Import (C, dlopen, "dlopen");

      function dlclose (arg1 : System.Address) return int;  -- dlfcn.h:61:12
      pragma Import (C, dlclose, "dlclose");

      function dlsym (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;  -- dlfcn.h:65:14
      pragma Import (C, dlsym, "dlsym");

      function dlerror return Interfaces.C.Strings.chars_ptr;  -- dlfcn.h:83:14
      pragma Import (C, dlerror, "dlerror");

   end dlfcn_h;
   use dlfcn_h;
   use type system.Address;

   procedure open (this       : in out dynamic_Library;
                   File_Name  : string;
                   Flags      : integer) is
   begin
      this.handle := dlopen (Strings.Null_Ptr,0);
      if this.handle = System.Null_Address then
         raise Dynamic_Library_Error  with error;
      end if;
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
   begin
      return ret : System.Address do
         ret := dlsym (this.handle, strings.Null_Ptr);
         if ret = System.Null_Address then
            raise Dynamic_Library_Error  with error;
         end if;
      end return;
   end;

   function error return String is
   begin
      return Interfaces.C.Strings.Value (dlerror);
   end;

end dl;

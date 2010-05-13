with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package dlfcn_h is

   function dlopen (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : int) return System.Address;  -- /usr/include/dlfcn.h:57:14
   pragma Import (C, dlopen, "dlopen");

   function dlclose (arg1 : System.Address) return int;  -- /usr/include/dlfcn.h:61:12
   pragma Import (C, dlclose, "dlclose");

   function dlsym (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;  -- /usr/include/dlfcn.h:65:14
   pragma Import (C, dlsym, "dlsym");

   function dlerror return Interfaces.C.Strings.chars_ptr;  -- /usr/include/dlfcn.h:83:14
   pragma Import (C, dlerror, "dlerror");

end dlfcn_h;

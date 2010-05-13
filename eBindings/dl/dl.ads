with System;
package dl is
   pragma Preelaborate;

   type dynamic_Library is tagged private;

   procedure open (this       : in out dynamic_Library;
                   File_Name  : string;
                   Flags      : integer);

   procedure close (this : dynamic_Library);

   function sym (this        : dynamic_Library;
                 Symbol_name : String) return System.Address;

   Dynamic_Library_Error : exception;

private
   type dynamic_Library is tagged record
      handle : System.Address := System.Null_Address;
   end record;
   function error return String;  -- dlfcn.h:83:14
end dl;

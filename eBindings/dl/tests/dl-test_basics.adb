with ada.Text_IO;
with GNAT.Source_Info;
with AUnit.Assertions;
with System.Address_Image;
with Interfaces.C.Strings;
with ada.Unchecked_Conversion;
package body Dl.Test_Basics is
   use AUnit;
   use AUnit.Assertions;

   -- Fixture elements


   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File & ":(no description)");
   end Name;


   -------------------------
   --  SampleTest
   -------------------------
   procedure SampleTest (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
      P : Dynamic_Library;
      type zlibVersion_Access is access function return Interfaces.C.Strings.chars_ptr;
      function conv is new ada.Unchecked_Conversion (system.Address, zlibVersion_Access);
      zlibVersion : zlibVersion_Access;
   begin
      p.open (File_Name => "/lib/libz.so.1");
      zlibVersion := conv (p.Sym ("zlibVersion"));
      declare
         version : constant string := Interfaces.C.Strings.Value (zlibVersion.all);
      begin
         test.Assert (Version (version'first + 1 ) = '.', "Dont seem to be a version numnber");
         test.Assert (Version (version'first + 3 ) = '.', "Dont seem to be a version numnber");
      end;

      p.close;
   end SampleTest;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine  (T, SampleTest'Access, "SampleTest");
   end Register_Tests;

end Dl.Test_Basics;

with Ada.Text_IO;
with GNAT.Source_Info;
package body uuid.Test is

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Test_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format (GNAT.Source_Info.File);
   end Name;

   procedure test_Generate (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      t : UUID;
   begin
      Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location);
      t.Clear;
      Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location);
      t.Generate;
      Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location);
      Ada.Text_IO.Put_Line (Unparse (t));
      Ada.Text_IO.Put_Line (Unparse_Lower (t));
      Ada.Text_IO.Put_Line (Unparse_Upper (t));
   end test_Generate;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, test_Generate'Access, "test_Generate");
   end Register_Tests;

end uuid.Test;

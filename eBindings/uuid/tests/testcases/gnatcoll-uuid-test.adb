with GNAT.Source_Info;
package body GNATCOLL.uuid.Test is

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
      t : UUID;
   begin
      t.Generate;

      t.Generate_Random;
      Test.Assert (t.Get_Type = DCE_RANDOM, "Get_Type RANDOM");

      t.Generate_Time;
      Test.Assert (t.Get_Type = DCE_TIME, "Get_Type TIME");

      Test.Assert (t.Get_Variant = DCE, "Get_Variant");
   end test_Generate;

   procedure test_Parse (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      t : UUID;
      t2 : UUID;
   begin
      t.Generate_Random;
      t2 := Parse (t.Unparse);
      Test.Assert (t = t2, "Roundtrip failed");
      t.Clear;
      Test.Assert (t.Unparse_Lower = "00000000-0000-0000-0000-000000000000",
                   "Unparse_Lower");
      Test.Assert (t.Unparse_Upper = "00000000-0000-0000-0000-000000000000",
                   "Unparse_Upper");
   end test_Parse;


   procedure test_Clear (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      t : UUID;
   begin
      t.Clear;
      Test.Assert (t.Is_Null, "Clear failed");
      t.Generate_Random;
      Test.Assert (not t.Is_Null, "Fill failed");
   end test_Clear;

   procedure test_gtlt (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      t, t2 : UUID;
   begin
      t.Generate_Time;
      t2.Generate_Time;
      Test.Assert (t2 > t, "> failed");
      Test.Assert (not (t2 < t), "< Faild");
      Test.Assert (not (t2 = t), "= Faild");
   end test_gtlt;


   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, test_Generate'Access, "test_Generate");
      Register_Routine (T, test_Parse'Access, "test_Parse");
      Register_Routine (T, test_Clear'Access, "test_Clear");
      Register_Routine (T, test_gtlt'Access, "test_gtlt");
   end Register_Tests;

end GNATCOLL.uuid.Test;

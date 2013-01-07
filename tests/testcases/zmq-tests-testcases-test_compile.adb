pragma Warnings (off);
with GNAT.Source_Info;
with ZMQ.Contexts;
with ZMQ.Sockets;
with ZMQ.Messages;
with ZMQ.Low_Level;
with ZMQ.Proxys;
with ZMQ.Devices;
package body ZMQ.Tests.Testcases.Test_Compile is
   use AUnit;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;


   -------------------------
   --  SampleTest
   -------------------------
   procedure Compile (Test : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end Compile;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine  (T, Compile'Access, "Compile");
   end Register_Tests;

end ZMQ.Tests.TestCases.Test_Compile;

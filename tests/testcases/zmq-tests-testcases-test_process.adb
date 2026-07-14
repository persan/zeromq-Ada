with GNAT.Source_Info;
with Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
package body ZMQ.Tests.Testcases.Test_Process is
   use AUnit;
   use Ada.Strings.Unbounded;
   MSG_STRING : constant Unbounded_String := To_Unbounded_String ("Query");


   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;



   Test_Port : constant String := "inproc://pub-sub";

   ------------
   -- Set_Up --
   ------------

   overriding
   procedure Set_Up (Test : in out Test_Case) is
      T : Test_Case renames Test;
   begin
      T.Pub.Initialize (T.Ctx, Sockets.PUB);
      T.Sub.Initialize (T.Ctx, Sockets.SUB);
      T.Pub.Bind (Test_Port);
      T.Sub.Connect (Test_Port);
      T.Sub.Establish_Message_Filter ("");
      delay 0.1;
   end Set_Up;

   -------------------------
   --  Publish
   -------------------------
   procedure Send (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T     : Test_Case renames Test_Case (Test);
      msg   : Ada.Strings.Unbounded.Unbounded_String;

   begin
      T.Pub.Send (MSG_STRING);
      delay 0.1;

      T.Sub.Recv (msg);
      Assert (msg = MSG_STRING, "Error");
      delay 0.1;
   end Send;


   ---------------
   -- Tear_Down --
   ---------------

   overriding
   procedure Tear_Down (Test : in out Test_Case) is
      T : Test_Case renames Test;
   begin
      T.Pub.Finalize;
      T.Sub.Finalize;
      delay 0.1;
   end Tear_Down;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine (T, Send'Access, "Send");
   end Register_Tests;

end ZMQ.Tests.TestCases.Test_Process;

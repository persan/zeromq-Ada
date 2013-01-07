with GNAT.Source_Info;
with Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
package body ZMQ.Tests.Testcases.Test_REQRESP is
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



   -------------------------
   --  initialize
   -------------------------
   procedure Initialize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin

      T.Sub.Initialize (T.Ctx, Sockets.REP);
      T.Sub.Bind ("inproc://req");

      T.Pub.Initialize (T.Ctx, Sockets.REQ);
      T.Pub.Connect ("inproc://req");


   end Initialize;

   -------------------------
   --  Publish
   -------------------------
   procedure Send (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T     : Test_Case renames Test_Case (Test);
      Msg   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.Pub.Send (MSG_STRING);
      T.Sub.Recv (Msg);
      T.Sub.Send (Msg);
      T.Pub.Recv (Msg);
      Assert (Msg = MSG_STRING, "Error");
   end Send;

   -------------------------
   --  Subscribe
   -------------------------
   procedure Finalize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.Pub.Finalize;
      T.Sub.Finalize;
   end Finalize;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine (T, Initialize'Access, "initialize");
      Register_Routine (T, Send'Access, "Send");
      Register_Routine (T, Finalize'Access, "Finalize");
   end Register_Tests;

end ZMQ.Tests.TestCases.Test_REQRESP;

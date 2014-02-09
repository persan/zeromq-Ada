

with GNAT.Source_Info;
with Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
package body ZMQ.Tests.Testcases.Test_Pubsub is
   use AUnit;
   use Ada.Strings.Unbounded;
   use GNAT.Source_Info;
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

   -------------------------
   --  initialize
   -------------------------
   procedure initialize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      --  T.Ctx.Set_number_of_IO_threads (1);
      T.Pub.Initialize (T.Ctx, Sockets.PUB);
      T.Sub.Initialize (T.Ctx, Sockets.SUB);
      T.Pub.Connect (Test_Port);
      T.Sub.Bind    (Test_Port);
      T.Sub.Establish_message_filter ("");
      delay 0.1;
   end initialize;

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
      Register_Routine (T, initialize'Access, "initialize");
      Register_Routine (T, Send'Access, "Send");
      Register_Routine (T, Finalize'Access, "Finalize");
   end Register_Tests;

end ZMQ.Tests.TestCases.Test_Pubsub;

with GNAT.Source_Info;
with Ada.Strings.Unbounded;
package body ZMQ.Tests.Testcases.Test_Pubsub is
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
   procedure initialize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.Ctx.Initialize (1);
      T.pub.Initialize (T.Ctx, Sockets.PUB);

      T.Sub.Initialize (T.Ctx, Sockets.SUB);
      T.Sub.Establish_message_filter ("");

      T.Sub.Bind ("inproc://pub-sub");
      T.pub.Connect ("inproc://pub-sub");
   end initialize;

   -------------------------
   --  Publish
   -------------------------
   procedure Send (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T     : Test_Case renames Test_Case (Test);
      msg   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.pub.Send (MSG_STRING);
      T.Sub.recv (msg);
      T.Assert (msg = MSG_STRING, "Error");
   end Send;

   -------------------------
   --  Subscribe
   -------------------------
   procedure Finalize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.pub.Finalize;
      T.Sub.Finalize;
      T.Ctx.Finalize;
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

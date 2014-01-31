

with GNAT.Source_Info;
with Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
with GNAT.Spitbol; use GNAT.Spitbol;
package body ZMQ.Tests.Testcases.Test_REQRESP is
   use AUnit;
   use Ada.Strings.Unbounded;
   use GNAT.Source_Info;

   REQUEST_STRING  : constant Unbounded_String := V ("Query");
   RESPONSE_STRING : constant Unbounded_String := V ("Reply");


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

   addr : constant String := "tcp://127.0.0.1:5555";
   procedure Initialize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.RESP.Initialize (T.Ctx, Sockets.REP);
      T.RESP.Bind (addr);
      delay 0.1;
      T.REQ.Initialize (T.Ctx, Sockets.REQ);
      T.REQ.Connect (addr);
      delay 0.1;

   end Initialize;

   -------------------------
   --  Publish
   -------------------------
   procedure Send (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T     : Test_Case renames Test_Case (Test);
      Msg   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      T.REQ.Send (REQUEST_STRING);

      T.RESP.Recv (Msg);
      Assert (Msg = REQUEST_STRING, "Error");

      T.RESP.Send (RESPONSE_STRING);

      T.REQ.Recv (Msg);
      Assert (Msg = RESPONSE_STRING, "Error");


   end Send;

   -------------------------
   --  Subscribe
   -------------------------
   procedure Finalize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.REQ.Finalize;
      T.RESP.Finalize;
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

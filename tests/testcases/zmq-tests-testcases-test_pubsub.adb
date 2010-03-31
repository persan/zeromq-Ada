with GNAT.Source_Info;
with Zmq.Messages;
package body Zmq.Tests.Testcases.Test_Pubsub is
   use AUnit;
   -- Fixture elements

   MSG_STRING : constant string := "test.data.kalle saldhfhsfkhsafkhsadjfhsdjhfsajkhsdajksadjksadjhfsdkjasdlhfsldjsajsdajkhsdjhdsajkhsdfjhds";
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
   --  initialize
   -------------------------
   procedure initialize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.ctx.initialize (1, 1, 0);
      T.pub.initialize (T.ctx, Sockets.PUB);
      T.Sub.initialize (T.ctx, Sockets.SUB);
      T.Sub.Bind ("tcp://lo:5555");
      T.Pub.Bind ("tcp://lo:5555");
   end initialize;
   -------------------------
   --  Publish
   -------------------------
   procedure Send (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T     : Test_Case renames Test_Case (Test);
      msg : ZMQ.Messages.Message;
   begin
      msg.Initialize (MSG_STRING);
      T.pub.send (msg);
   end Send;

   -------------------------
    --  Subscribe
   -------------------------
   procedure Recieve (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
      msg : ZMQ.Messages.Message;
   begin
      msg.Initialize;
      T.Sub.recv (msg);
      T.Assert (STring'(msg.getData) = MSG_STRING, "Wrong Data");
   end Recieve;
   procedure Finalize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      T.pub.Finalize;
      T.Sub.Finalize;
      T.ctx.Finalize;
   end Finalize;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine(T, initialize'access, "initialize");
      Register_Routine(T, Send'access, "Send");
      Register_Routine(T, Recieve'access, "Recieve");
      Register_Routine(T, Finalize'access, "Finalize");
   end Register_Tests;

end Zmq.Tests.Testcases.Test_Pubsub;

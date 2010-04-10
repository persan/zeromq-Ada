with GNAT.Source_Info;
package body Zmq.Tests.Testcases.Test_Pubsub is
   use AUnit;
   use ada.Strings.Unbounded;

   MSG_STRING : constant Unbounded_String := To_Unbounded_String ("Query");



   task body server is --  (self : not null access Test_Case) is
   begin
      loop
         select
            accept read;
            self.sub.recv (self.msg);
         or
            accept stop;
            exit;
         or
            terminate;
         end select;
      end loop;
   end server;

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
      T.ctx.initialize (2, 2, 0);
      T.pub.initialize (T.ctx, Sockets.PUB);

      T.Sub.initialize (T.ctx, Sockets.SUB);
      T.Sub.setsockopt (Sockets.SUBSCRIBE, "");

      T.Sub.Bind ("inproc://pub-sub");
      T.Pub.Bind ("inproc://pub-sub");
   end initialize;
   -------------------------
   --  Publish
   -------------------------
   procedure Send (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T     : Test_Case renames Test_Case (Test);
   begin
      t.msg:=To_Unbounded_String("");
      t.s.read;
      delay 0.1;
      T.pub.send (MSG_STRING);
      delay 0.1;
      T.assert (t.msg = MSG_STRING, "Error");
   end Send;

   -------------------------
    --  Subscribe
   -------------------------
   procedure Finalize (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      t.s.stop;
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
      Register_Routine(T, Finalize'access, "Finalize");
   end Register_Tests;

end Zmq.Tests.Testcases.Test_Pubsub;

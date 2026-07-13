with AUnit;
with AUnit.Test_Cases;
with ZMQ.Contexts;
with ZMQ.Sockets;

package ZMQ.Tests.TestCases.Test_Polling is

   type Test_Case;

   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Ctx        : ZMQ.Contexts.Context;
      Pub        : aliased ZMQ.Sockets.Socket;
      Sub        : aliased ZMQ.Sockets.Socket;
      Pub_Access : ZMQ.Sockets.Any_Socket;
      Sub_Access : ZMQ.Sockets.Any_Socket;
   end record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   overriding
   procedure Set_Up_Case (Test : in out Test_Case);

   overriding
   procedure Tear_Down_Case (Test : in out Test_Case);

   function Name (T : Test_Case)
                  return AUnit.Message_String;
   --  Returns name identifying the test case

end ZMQ.Tests.TestCases.Test_Polling;

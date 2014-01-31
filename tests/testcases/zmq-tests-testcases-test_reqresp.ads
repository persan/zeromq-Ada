

with AUnit;
with AUnit.Test_Cases;
with ZMQ.Contexts;
with ZMQ.Sockets;
package ZMQ.Tests.Testcases.Test_REQRESP is
   type Test_Case;

   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Ctx  : ZMQ.Contexts.Context;
      REQ  : ZMQ.Sockets.Socket;
      RESP : ZMQ.Sockets.Socket;
   end record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return AUnit.Message_String;
   --  Returns name identifying the test case

end ZMQ.Tests.TestCases.Test_REQRESP;

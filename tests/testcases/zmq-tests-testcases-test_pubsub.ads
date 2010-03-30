with AUnit;
with AUnit.Test_Cases;
with ZMQ.Contexts;
with ZMQ.Sockets;
package Zmq.Tests.Testcases.Test_Pubsub is

   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Ctx : ZMQ.Contexts.context;
      pub : ZMQ.Sockets.Socket;
      Sub : ZMQ.Sockets.Socket;
   end record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return Aunit.Message_String;
   --  Returns name identifying the test case

end Zmq.Tests.Testcases.Test_Pubsub;

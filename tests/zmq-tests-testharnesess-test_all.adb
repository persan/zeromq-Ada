with AUnit.Run;
--  with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

with ZMQ.Tests.TestSuits.Test_All;

--------------------------------------
-- Zmq.Tests.Testharnesess.Test_All --
--------------------------------------

procedure ZMQ.Tests.Testharnesess.Test_All is

   procedure Run is new AUnit.Run.Test_Runner
     (ZMQ.Tests.TestSuits.Test_All.Suite);
   --  Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   Run (Reporter);
end ZMQ.Tests.TestHarnesess.Test_All;

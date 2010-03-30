with AUnit.Run;
-- with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

with Zmq.Tests.Testsuits.Test_All;

--------------------------------------
-- Zmq.Tests.Testharnesess.Test_All --
--------------------------------------

procedure Zmq.Tests.Testharnesess.Test_All is

   procedure Run is new AUnit.Run.Test_Runner (Zmq.Tests.Testsuits.Test_All.Suite);
   -- Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   Run (Reporter);
end Zmq.Tests.Testharnesess.Test_All;
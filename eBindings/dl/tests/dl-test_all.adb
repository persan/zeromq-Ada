with AUnit.Run;
-- with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

with Dl.Test_All_Suit;

-----------------
-- Dl.Test_All --
-----------------

procedure Dl.Test_All is

   procedure Run is new AUnit.Run.Test_Runner (Dl.Test_All_Suit.Suite);
   -- Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   Run (Reporter);
end Dl.Test_All;
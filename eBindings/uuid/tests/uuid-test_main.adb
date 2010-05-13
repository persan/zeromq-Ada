with AUnit;

--------------------
-- Uuid.Test_Main --
--------------------
with AUnit.Reporter.Text;
with AUnit.Run;
with uuid.Suite;
procedure uuid.Test_Main is
   procedure Run is new AUnit.Run.Test_Runner (Standard.uuid.Suite.Suite);
   reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (reporter);
end uuid.Test_Main;

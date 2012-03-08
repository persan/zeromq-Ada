with AUnit;

--------------------
-- Uuid.Test_Main --
--------------------
with AUnit.Reporter.Text;
with AUnit.Run;
with GNATCOLL.uuid.Suite;
procedure GNATCOLL.uuid.Test_Main is
   procedure Run is new AUnit.Run.Test_Runner (GNATCOLL.uuid.Suite.Suite);
   reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (reporter);
end GNATCOLL.uuid.Test_Main;

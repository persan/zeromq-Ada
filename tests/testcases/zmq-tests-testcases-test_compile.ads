with AUnit;
with AUnit.Test_Cases;

package ZMQ.Tests.Testcases.Test_Compile is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return AUnit.Message_String;
   --  Returns name identifying the test case

end ZMQ.Tests.TestCases.Test_Compile;

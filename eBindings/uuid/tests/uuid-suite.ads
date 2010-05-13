with AUnit; use AUnit;
with AUnit.Test_Suites;
package uuid.Suite is

   function Suite return Test_Suites.Access_Test_Suite;
   --  Return the test suite

end uuid.Suite;

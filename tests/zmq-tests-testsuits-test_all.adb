

--  Import tests and sub-suites to run
with ZMQ.Tests.TestCases.Test_Compile;
with ZMQ.Tests.TestCases.Test_Pubsub;
package body ZMQ.Tests.Testsuits.Test_All is
   use AUnit.Test_Suites;


   --  Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:
   Test_1 : aliased TestCases.Test_Compile.Test_Case;
   Test_2 : aliased TestCases.Test_Pubsub.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      return Result'Access;
   end Suite;

end ZMQ.Tests.TestSuits.Test_All;

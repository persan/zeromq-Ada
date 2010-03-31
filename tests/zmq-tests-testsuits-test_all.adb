

--  Import tests and sub-suites to run
with Zmq.Tests.Testcases.Test_Compile;
with Zmq.Tests.Testcases.Test_Pubsub;
package body Zmq.Tests.Testsuits.Test_All is
   use AUnit.Test_Suites;


   -- Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:
   Test_1 : aliased Testcases.Test_Compile.Test_Case;
   Test_2 : aliased Testcases.Test_Pubsub.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      return Result'Access;
   end Suite;

end Zmq.Tests.Testsuits.Test_All;



--  Import tests and sub-suites to run
with Dl.Test_Basics;

package body Dl.Test_All_Suit is
   use AUnit.Test_Suites;


   -- Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:
   Test_1 : aliased Dl.Test_Basics.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end Dl.Test_All_Suit;
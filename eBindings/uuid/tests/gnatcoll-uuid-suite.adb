
with GNATCOLL.uuid.Test;
package body GNATCOLL.uuid.Suite is
   use AUnit.Test_Suites;
   Result : aliased Test_Suite;
   Test_1 : aliased GNATCOLL.uuid.Test.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.uuid.Suite;

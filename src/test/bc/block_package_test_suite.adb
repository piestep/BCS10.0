-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Block_Package.Block_Test;
--

package body Block_Package_Test_Suite is

   Result : aliased Test_Suite;

   Block_Tests : aliased Block_Package.Block_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Block_Tests'Access);
      return Result'Access;
   end Suite;

end Block_Package_Test_Suite;

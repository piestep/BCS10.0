-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Optimize_Package.Optimize_Test;
--

package body Optimize_Package_Test_Suite is

   Result : aliased Test_Suite;

   Optimize_Tests : aliased Optimize_Package.Optimize_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Optimize_Tests'Access);
      return Result'Access;
   end Suite;

end Optimize_Package_Test_Suite;

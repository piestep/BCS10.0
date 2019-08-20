-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Error_Package.Error_Test;
--

package body Error_Package_Test_Suite is

   Result : aliased Test_Suite;

   Error_Tests : aliased Error_Package.Error_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Error_Tests'Access);
      return Result'Access;
   end Suite;

end Error_Package_Test_Suite;

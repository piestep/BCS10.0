-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Scanner_Package.Scanner_Test;
--

package body Scanner_Package_Test_Suite is

   Result : aliased Test_Suite;

   Scanner_Tests : aliased Scanner_Package.Scanner_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Scanner_Tests'Access);
      return Result'Access;
   end Suite;

end Scanner_Package_Test_Suite;

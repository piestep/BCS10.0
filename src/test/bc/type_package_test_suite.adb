-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Type_Package.Type_Test;
--

package body Type_Package_Test_Suite is

   Result : aliased Test_Suite;

   Type_Tests : aliased Type_Package.Type_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Type_Tests'Access);
      return Result'Access;
   end Suite;

end Type_Package_Test_Suite;

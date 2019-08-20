-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Identifier_Package.Identifier_Test;
--

package body Identifier_Package_Test_Suite is

   Result : aliased Test_Suite;

   Identifier_Tests : aliased Identifier_Package.Identifier_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Identifier_Tests'Access);
      return Result'Access;
   end Suite;

end Identifier_Package_Test_Suite;

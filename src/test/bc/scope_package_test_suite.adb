-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Scope_Package.Scope_Test;
--

package body Scope_Package_Test_Suite is

   Result : aliased Test_Suite;

   Scope_Tests : aliased Scope_Package.Scope_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Scope_Tests'Access);
      return Result'Access;
   end Suite;

end Scope_Package_Test_Suite;

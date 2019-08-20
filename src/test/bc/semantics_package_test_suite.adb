-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Semantics_Package.Semantics_Test;
--

package body Semantics_Package_Test_Suite is

   Result : aliased Test_Suite;

   Semantics_Tests : aliased Semantics_Package.Semantics_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Semantics_Tests'Access);
      return Result'Access;
   end Suite;

end Semantics_Package_Test_Suite;

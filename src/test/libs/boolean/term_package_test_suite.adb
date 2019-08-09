-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Term_Package.Term_Test;
--

package body Term_Package_Test_Suite is

   Result : aliased Test_Suite;

   Term_Tests : aliased Term_Package.Term_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Term_Tests'Access);
      return Result'Access;
   end Suite;

end Term_Package_Test_Suite;

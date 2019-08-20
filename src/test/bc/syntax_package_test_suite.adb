-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Syntax_Package.Syntax_Test;
--

package body Syntax_Package_Test_Suite is

   Result : aliased Test_Suite;

   Syntax_Tests : aliased Syntax_Package.Syntax_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Syntax_Tests'Access);
      return Result'Access;
   end Suite;

end Syntax_Package_Test_Suite;

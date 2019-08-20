-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Operand_Package.Operand_Test;
--

package body Operand_Package_Test_Suite is

   Result : aliased Test_Suite;

   Operand_Tests : aliased Operand_Package.Operand_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Operand_Tests'Access);
      return Result'Access;
   end Suite;

end Operand_Package_Test_Suite;

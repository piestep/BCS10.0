-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Equation_Package.Equation_Test;
with Equation_Package.IO_Package.IO_Test;
--

package body Equation_Package_Test_Suite is

   Result : aliased Test_Suite;

   Equation_Tests : aliased Equation_Package.Equation_Test.Test;
   IO_Tests : aliased Equation_Package.IO_Package.IO_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Equation_Tests'Access);
      Add_Test (Result'Access, IO_Tests'Access);
      return Result'Access;
   end Suite;

end Equation_Package_Test_Suite;

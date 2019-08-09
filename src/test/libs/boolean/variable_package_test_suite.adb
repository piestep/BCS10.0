-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Variable_Package.Variable_Test;
with Variable_Package.IO_Package.IO_Test;
--

package body Variable_Package_Test_Suite is

   Result : aliased Test_Suite;

   Variable_Tests : aliased Variable_Package.Variable_Test.Test;
   IO_Tests : aliased Variable_Package.IO_Package.IO_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Variable_Tests'Access);
      Add_Test (Result'Access, IO_Tests'Access);
      return Result'Access;
   end Suite;

end Variable_Package_Test_Suite;

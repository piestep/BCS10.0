-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Number_Package.Number_Test;
with Number_Package.IO_Package.IO_Test;
--

package body Number_Package_Test_Suite is

   Result : aliased Test_Suite;

   Number_Tests : aliased Number_Package.Number_Test.Test;
   IO_Tests : aliased Number_Package.IO_Package.IO_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Number_Tests'Access);
      Add_Test (Result'Access, IO_Tests'Access);
      return Result'Access;
   end Suite;

end Number_Package_Test_Suite;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Parameter_Package.IO_Package.IO_Test;
--

package body Parameter_IO_Test_Suite is

   Result : aliased Test_Suite;

   IO_Tests : aliased Parameter_Package.IO_Package.IO_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, IO_Tests'Access);
      return Result'Access;
   end Suite;

end Parameter_IO_Test_Suite;

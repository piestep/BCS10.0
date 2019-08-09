-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Word_Package.Word_Test;
with Word_Package.IO_Package.IO_Test;
--

package body Word_Package_Test_Suite is

   Result : aliased Test_Suite;

   Word_Tests : aliased Word_Package.Word_Test.Test;
   IO_Tests : aliased Word_Package.IO_Package.IO_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Word_Tests'Access);
      Add_Test (Result'Access, IO_Tests'Access);
      return Result'Access;
   end Suite;

end Word_Package_Test_Suite;

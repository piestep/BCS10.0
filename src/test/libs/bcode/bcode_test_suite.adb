-- BCS Boolean Compiler System
-- Copyright (c) 2019 Paul Estep

with BCode_Package.BCode_Test;
with BCode_Package.IO_Package.IO_Test;
--

package body BCode_Test_Suite is

   Result : aliased Test_Suite;

   BCode_Tests : aliased BCode_Package.BCode_Test.Test;
   IO_Tests : aliased BCode_Package.IO_Package.IO_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, BCode_Tests'Access);
      Add_Test (Result'Access, IO_Tests'Access);
      return Result'Access;
   end Suite;

end BCode_Test_Suite;

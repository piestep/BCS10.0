-- BCS Boolean Compiler System
-- Copyright (c) 2019 Paul Estep

with Pool_Package.Pool_Test;
--

package body Pool_Test_Suite is

   Result : aliased Test_Suite;

   Pool_Tests : aliased Pool_Package.Pool_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Pool_Tests'Access);
      return Result'Access;
   end Suite;

end Pool_Test_Suite;

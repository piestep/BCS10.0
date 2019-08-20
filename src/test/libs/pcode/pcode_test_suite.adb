-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

with PCode_Package.PCode_Test;
--

package body PCode_Test_Suite is

   Result : aliased Test_Suite;

   PCode_Tests : aliased PCode_Package.PCode_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, PCode_Tests'Access);
      return Result'Access;
   end Suite;

end PCode_Test_Suite;

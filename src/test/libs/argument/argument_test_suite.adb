-- BCS Boolean Compiler System
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Argument_Package.Argument_Test;
--

package body Argument_Test_Suite is

   Result : aliased Test_Suite;

   Argument_Tests : aliased Argument_Package.Argument_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
            Add_Test (Result'Access, Argument_Tests'Access);
      return Result'Access;
   end Suite;

end Argument_Test_Suite;

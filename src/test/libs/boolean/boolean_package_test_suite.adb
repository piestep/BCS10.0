-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Boolean_Package.Boolean_Test;
--

package body Boolean_Package_Test_Suite is

   Result : aliased Test_Suite;

   Boolean_Tests : aliased Boolean_Package.Boolean_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Boolean_Tests'Access);
      return Result'Access;
   end Suite;

end Boolean_Package_Test_Suite;

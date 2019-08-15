-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep
pragma Ada_2012;

with Mask_Package.Mask_Test;
--

package body Mask_Package_Test_Suite is

   Result : aliased Test_Suite;

   Mask_Tests : aliased Mask_Package.Mask_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Mask_Tests'Access);
      return Result'Access;
   end Suite;

end Mask_Package_Test_Suite;

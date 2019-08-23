-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Generate_Package.ACode_Test;
--

package body Generate_Package_ACode_Test_Suite is

   Result : aliased Test_Suite;

   Generate_Tests : aliased Generate_Package.ACode_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Generate_Tests'Access);
      return Result'Access;
   end Suite;

end Generate_Package_ACode_Test_Suite;

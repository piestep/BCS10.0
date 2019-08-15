-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep
pragma Ada_2012;

with Values_Package.Values_Test;
--

package body Values_Package_Test_Suite is

   Result : aliased Test_Suite;

   Values_Tests : aliased Values_Package.Values_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Values_Tests'Access);
      return Result'Access;
   end Suite;

end Values_Package_Test_Suite;

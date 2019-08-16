-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Parameter_Package.Parameter_Test;
--

package body Parameter_Test_Suite is

   Result : aliased Test_Suite;

   Parameter_Tests : aliased Parameter_Package.Parameter_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Parameter_Tests'Access);
      return Result'Access;
   end Suite;

end Parameter_Test_Suite;

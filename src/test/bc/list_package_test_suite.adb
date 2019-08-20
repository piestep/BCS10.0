-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with List_Package.List_Test;
--

package body List_Package_Test_Suite is

   Result : aliased Test_Suite;

   List_Tests : aliased List_Package.List_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, List_Tests'Access);
      return Result'Access;
   end Suite;

end List_Package_Test_Suite;

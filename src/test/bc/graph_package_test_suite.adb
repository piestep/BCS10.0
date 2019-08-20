-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Graph_Package.Graph_Test;
--

package body Graph_Package_Test_Suite is

   Result : aliased Test_Suite;

   Graph_Tests : aliased Graph_Package.Graph_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Graph_Tests'Access);
      return Result'Access;
   end Suite;

end Graph_Package_Test_Suite;

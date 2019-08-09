-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Word_Test;
--

package body Word_Test_Suite is

   Result : aliased Test_Suite;

   Word_Tests : aliased Word_Test.Test;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Word_Tests'Access);
      return Result'Access;
   end Suite;

end Word_Test_Suite;

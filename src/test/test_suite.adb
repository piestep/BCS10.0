-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with AUnit.Tests;
with AUnit.Test_Caller;
--
with Boolean_Test_Suite;
with Parameter_Test_Suite;
with Argument_Test_Suite;
with BC_Test_Suite;
with PCode_Test_Suite;
--

package body Test_Suite is

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Boolean_Test_Suite.Suite);
      Result.Add_Test (Parameter_Test_Suite.Suite);
      Result.Add_Test (Argument_Test_Suite.Suite);
      Result.Add_Test (BC_Test_Suite.Suite);
      Result.Add_Test (PCode_Test_Suite.Suite);
      return Result;
   end Suite;

end Test_Suite;

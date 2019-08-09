-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Boolean_Package_Test_Suite;
with Values_Package_Test_Suite;
with Variable_Package_Test_Suite;
with Mask_Package_Test_Suite;
with Term_Package_Test_Suite;
with Number_Package_Test_Suite;
with Equation_Package_Test_Suite;
with Word_Package_Test_Suite;
with Word_Test_Suite;
--

package body Boolean_Test_Suite is

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Boolean_Package_Test_Suite.Suite);
      Result.Add_Test (Values_Package_Test_Suite.Suite);
      Result.Add_Test (Variable_Package_Test_Suite.Suite);
      Result.Add_Test (Mask_Package_Test_Suite.Suite);
      Result.Add_Test (Term_Package_Test_Suite.Suite);
      Result.Add_Test (Number_Package_Test_Suite.Suite);
      Result.Add_Test (Equation_Package_Test_Suite.Suite);
      Result.Add_Test (Word_Package_Test_Suite.Suite);
      Result.Add_Test (Word_Test_Suite.Suite);
      return Result;
   end Suite;

end Boolean_Test_Suite;

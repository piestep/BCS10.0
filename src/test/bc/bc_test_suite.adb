-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Error_Package_Test_Suite;
with Source_Package_Test_Suite;
with Scanner_Package_Test_Suite;
with List_Package_Test_Suite;
with Type_Package_Test_Suite;
with Identifier_Package_Test_Suite;
with Operand_Package_Test_Suite;
with Scope_Package_Test_Suite;
with Block_Package_Test_Suite;
with Graph_Package_Test_Suite;
with Syntax_Package_Test_Suite;
with Semantics_Package_Test_Suite;
--  with Optimize_Package_Test_Suite;
--  with Generate_Package_Parameters_Test_Suite;
--  with Generate_Package_PCode_Test_Suite;
--  with Generate_Package_BCode_Test_Suite;
--

package body BC_Test_Suite is

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Error_Package_Test_Suite.Suite);
      Result.Add_Test (Source_Package_Test_Suite.Suite);
      Result.Add_Test (Scanner_Package_Test_Suite.Suite);
      Result.Add_Test (List_Package_Test_Suite.Suite);
      Result.Add_Test (Type_Package_Test_Suite.Suite);
      Result.Add_Test (Identifier_Package_Test_Suite.Suite);
      Result.Add_Test (Operand_Package_Test_Suite.Suite);
      Result.Add_Test (Scope_Package_Test_Suite.Suite);
      Result.Add_Test (Block_Package_Test_Suite.Suite);
      Result.Add_Test (Graph_Package_Test_Suite.Suite);
      Result.Add_Test (Syntax_Package_Test_Suite.Suite);
      Result.Add_Test (Semantics_Package_Test_Suite.Suite);
      --        Result.Add_Test (Optimize_Package_Test_Suite.Suite);
      --          Result.Add_Test (Generate_Package_Parameters_Test_Suite.Suite);
      --          Result.Add_Test (Generate_Package_PCode_Test_Suite.Suite);
      --          Result.Add_Test (Generate_Package_BCode_Test_Suite.Suite);
      return Result;
   end Suite;

end BC_Test_Suite;

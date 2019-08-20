-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Operand_Package.Operand_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);

   procedure Test_Copy (The_Test : in out Test_Case'Class);
   procedure Test_Dispose (The_Test : in out Test_Case'Class);
   procedure Test_Is_Constant (The_Test : in out Test_Case'Class);
   procedure Test_Is_Variable (The_Test : in out Test_Case'Class);
   procedure Test_Is_Identifier (The_Test : in out Test_Case'Class);
   procedure Test_Is_Array (The_Test : in out Test_Case'Class);
   procedure Test_Constant_Operation_Uniary (The_Test : in out Test_Case'Class);
   procedure Test_Constant_Operation_Binary (The_Test : in out Test_Case'Class);

end Operand_Package.Operand_Test;

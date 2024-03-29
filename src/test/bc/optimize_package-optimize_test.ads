-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Optimize_Package.Optimize_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);
   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);

   -- optimize test

   procedure Test_Procedure (The_Test : in out Test_Case'Class);
   procedure Test_Scalar (The_Test : in out Test_Case'Class);
   procedure Test_Mod (The_Test : in out Test_Case'Class);
   procedure Test_Range (The_Test : in out Test_Case'Class);
   procedure Test_Array (The_Test : in out Test_Case'Class);
   procedure Test_Identifiers (The_Test : in out Test_Case'Class);
   procedure Test_Assignment (The_Test : in out Test_Case'Class);
   procedure Test_If (The_Test : in out Test_Case'Class);
   procedure Test_For (The_Test : in out Test_Case'Class);
   procedure Test_Unary (The_Test : in out Test_Case'Class);
   procedure Test_Relation (The_Test : in out Test_Case'Class);
   procedure Test_And (The_Test : in out Test_Case'Class);
   procedure Test_Or (The_Test : in out Test_Case'Class);
   procedure Test_Xor (The_Test : in out Test_Case'Class);
   procedure Test_Addition (The_Test : in out Test_Case'Class);
   procedure Test_Subtraction (The_Test : in out Test_Case'Class);
   procedure Test_Multiplication (The_Test : in out Test_Case'Class);
   procedure Test_Division (The_Test : in out Test_Case'Class);
   procedure Test_Parameter (The_Test : in out Test_Case'Class);
   procedure Test_Index (The_Test : in out Test_Case'Class);

   procedure Test_Assign (The_Test : in out Test_Case'Class);
   procedure Test_Assign_Left (The_Test : in out Test_Case'Class);
   procedure Test_Assign_Right (The_Test : in out Test_Case'Class);

   -- optimize error test

   procedure Test_Variable_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Unary_Expression_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Binary_Expression_Errors (The_Test : in out Test_Case'Class);

   procedure Test_Optimize (The_Test : in out Test_Case'Class);

end Optimize_Package.Optimize_Test;

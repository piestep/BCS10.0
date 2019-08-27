-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Semantics_Package.Semantics_Test is

   Semantics_Dump     : Boolean := False;
   Semantics_Generate : Boolean := False;
   Semantics_Replace  : Boolean := False;

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   -- semantics test

   procedure Test_Procedure (The_Test : in out Test_Case'Class);
   procedure Test_Discreate_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Scalar_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Mod_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Range_Declarations (The_Test : in out Test_Case'Class);
   procedure Test_Array_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Identifier_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Parameters (The_Test : in out Test_Case'Class);
   procedure Test_Assignments (The_Test : in out Test_Case'Class);
   procedure Test_Ifs (The_Test : in out Test_Case'Class);
   procedure Test_Fors (The_Test : in out Test_Case'Class);
   procedure Test_Operators (The_Test : in out Test_Case'Class);
   procedure Test_Precedence (The_Test : in out Test_Case'Class);
   procedure Test_Expressions (The_Test : in out Test_Case'Class);
   procedure Test_Attributes (The_Test : in out Test_Case'Class);

   -- semantic error test

   procedure Test_Package_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Procedure_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Parameter_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Type_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Identifier_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Range_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Mod_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Array_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Assignment_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Variable_Errors (The_Test : in out Test_Case'Class);
   procedure Test_If_Errors (The_Test : in out Test_Case'Class);
   procedure Test_For_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Unary_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Binary_Errors (The_Test : in out Test_Case'Class);
   procedure Test_Attribute_Errors (The_Test : in out Test_Case'Class);

end Semantics_Package.Semantics_Test;

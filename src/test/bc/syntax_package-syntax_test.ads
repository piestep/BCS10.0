-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Syntax_Package.Syntax_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);

   procedure Test_Procedure (The_Test : in out Test_Case'Class);
   procedure Test_Scalar_Declarations (The_Test : in out Test_Case'Class);
   procedure Test_Mod_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Array_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Identifier_Declaration (The_Test : in out Test_Case'Class);
   procedure Test_Parameters (The_Test : in out Test_Case'Class);
   procedure Test_Assignments (The_Test : in out Test_Case'Class);
   procedure Test_Ifs (The_Test : in out Test_Case'Class);
   procedure Test_Fors (The_Test : in out Test_Case'Class);
   procedure Test_Statements (The_Test : in out Test_Case'Class);
   procedure Test_Operators (The_Test : in out Test_Case'Class);
   procedure Test_Precedences (The_Test : in out Test_Case'Class);
   procedure Test_Expressions (The_Test : in out Test_Case'Class);

   procedure Test_Syntax_Errors (The_Test : in out Test_Case'Class);

end Syntax_Package.Syntax_Test;

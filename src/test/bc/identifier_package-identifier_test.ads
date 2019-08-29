-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Identifier_Package.Identifier_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   procedure Test_Dispose (The_Test : in out Test_Case'Class);
   procedure Test_Clear (The_Test : in out Test_Case'Class);
   procedure Test_Is_Package (The_Test : in out Test_Case'Class);
   procedure Test_Is_Procedure (The_Test : in out Test_Case'Class);
   procedure Test_Is_Type (The_Test : in out Test_Case'Class);
   procedure Test_Is_Typed (The_Test : in out Test_Case'Class);
   procedure Test_Is_Constant (The_Test : in out Test_Case'Class);
   procedure Test_Is_Addressable (The_Test : in out Test_Case'Class);
   procedure Test_Is_Variable (The_Test : in out Test_Case'Class);
   procedure Test_Is_Index (The_Test : in out Test_Case'Class);
   procedure Test_Is_Parameter (The_Test : in out Test_Case'Class);

end Identifier_Package.Identifier_Test;

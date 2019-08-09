-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Equation_Package.Equation_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   procedure Test_Create_With_Boolean (The_Test : in out Test_Case'Class);
   procedure Test_Create_With_Variable (The_Test : in out Test_Case'Class);
   procedure Test_Dispose (The_Test : in out Test_Case'Class);
   procedure Test_Copy (The_Test : in out Test_Case'Class);
   procedure Test_Mask_Of (The_Test : in out Test_Case'Class);
   procedure Test_Normalize (The_Test : in out Test_Case'Class);
   procedure Test_Not_Op (The_Test : in out Test_Case'Class);
   procedure Test_And_Op (The_Test : in out Test_Case'Class);
   procedure Test_Or_Op (The_Test : in out Test_Case'Class);
   procedure Test_Xor_Op (The_Test : in out Test_Case'Class);
   procedure Test_Is_Constant_With_Boolean (The_Test : in out Test_Case'Class);
   procedure Test_Is_Constant (The_Test : in out Test_Case'Class);
   procedure Test_To_Constant (The_Test : in out Test_Case'Class);
   procedure Test_Solve (The_Test : in out Test_Case'Class);
   procedure Test_Length_Of (The_Test : in out Test_Case'Class);
   procedure Test_Variables_Of (The_Test : in out Test_Case'Class);
   procedure Test_Number_Of (The_Test : in out Test_Case'Class);
   procedure Test_Boolean_Of (The_Test : in out Test_Case'Class);
   procedure Test_Image_Of (The_Test : in out Test_Case'Class);

end Equation_Package.Equation_Test;

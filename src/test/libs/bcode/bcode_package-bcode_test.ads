-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package BCode_Package.BCode_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   procedure Test_Dispose (The_Test : in out Test_Case'Class);
   procedure Test_Append (The_Test : in out Test_Case'Class);
   procedure Test_Normalize (The_Test : in out Test_Case'Class);
   procedure Test_Number_Of (The_Test : in out Test_Case'Class);
   procedure Test_Solve (The_Test : in out Test_Case'Class);
   procedure Test_Boolean_Of (The_Test : in out Test_Case'Class);
   procedure Test_Image_Of (The_Test : in out Test_Case'Class);
   procedure Test_Iterate (The_Test : in out Test_Case'Class);

end BCode_Package.BCode_Test;

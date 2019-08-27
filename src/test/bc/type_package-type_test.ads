-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Type_Package.Type_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   procedure Test_Dispose (The_Test : in out Test_Case'Class);
   procedure Test_Is_Array (The_Test : in out Test_Case'Class);
   procedure Test_Is_Scalar (The_Test : in out Test_Case'Class);
   procedure Test_Is_Discrete (The_Test : in out Test_Case'Class);
   procedure Test_Is_Signed (The_Test : in out Test_Case'Class);
   procedure Test_Is_Modular (The_Test : in out Test_Case'Class);
   procedure Test_Is_Boolean (The_Test : in out Test_Case'Class);
   procedure Test_Is_Integer (The_Test : in out Test_Case'Class);
   procedure Test_Base_Of (The_Test : in out Test_Case'Class);
   procedure Test_First_Of (The_Test : in out Test_Case'Class);
   procedure Test_Last_Of (The_Test : in out Test_Case'Class);
   procedure Test_Size_Of (The_Test : in out Test_Case'Class);
   procedure Test_Is_Within (The_Test : in out Test_Case'Class);
   procedure Test_Is_Compatiable (The_Test : in out Test_Case'Class);
   procedure Test_Best_Of (The_Test : in out Test_Case'Class);
   procedure Test_Size_Of_Integer (The_Test : in out Test_Case'Class);
   procedure Test_Size_Of_Modular (The_Test : in out Test_Case'Class);

end Type_Package.Type_Test;

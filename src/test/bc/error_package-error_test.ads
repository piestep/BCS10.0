-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Error_Package.Error_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);

   procedure Test_Source_Error (The_Test : in out Test_Case'Class);
   procedure Test_Source_Warning (The_Test : in out Test_Case'Class);
   procedure Test_Compiler_Error (The_Test : in out Test_Case'Class);
   procedure Test_Compiler_Error_With_Position
     (The_Test : in out Test_Case'Class);
   procedure Test_List_Messages (The_Test : in out Test_Case'Class);
   procedure Test_Iterate (The_Test : in out Test_Case'Class);
   procedure Test_Is_Less_Than_Equal (The_Test : in out Test_Case'Class);

end Error_Package.Error_Test;

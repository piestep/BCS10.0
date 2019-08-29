-- BCS Boolean Compiler System
-- Copyright (c) 2019 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package PCode_Package.IO_Package.IO_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   procedure Test_Load (The_Test : in out Test_Case'Class);
   procedure Test_Save (The_Test : in out Test_Case'Class);
   procedure Test_List (The_Test : in out Test_Case'Class);

end PCode_Package.IO_Package.IO_Test;

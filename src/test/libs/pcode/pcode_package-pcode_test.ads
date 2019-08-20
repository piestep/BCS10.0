-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package PCode_Package.PCode_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   procedure Test_String_Of (The_Test : in out Test_Case'Class);

end PCode_Package.PCode_Test;

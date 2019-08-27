-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package List_Package.List_Test is

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);
   overriding procedure Set_Up (The_Test : in out Test);
   overriding procedure Tear_Down (The_Test : in out Test);

   procedure Test_Listing (The_Test : in out Test_Case'Class);

end List_Package.List_Test;

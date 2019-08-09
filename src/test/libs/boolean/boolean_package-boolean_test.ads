-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Boolean_Package.Boolean_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   procedure Test_Count_Of (The_Test : in out Test_Case'Class);
   procedure Test_BCInteger_At (The_Test : in out Test_Case'Class);
   procedure Test_BCModular_At (The_Test : in out Test_Case'Class);
   procedure Test_Image_Of (The_Test : in out Test_Case'Class);

end Boolean_Package.Boolean_Test;

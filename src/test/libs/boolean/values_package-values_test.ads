-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Values_Package.Values_Test is

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   procedure Test_Create (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_With_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Concatenation
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Image_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class);

end Values_Package.Values_Test;

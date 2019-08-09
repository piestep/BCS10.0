-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
with AUnit.Test_Cases;
--

package Equation_Package.IO_Package.IO_Test is

   type Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);

   procedure Test_Write (The_Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Read (The_Test : in out AUnit.Test_Cases.Test_Case'Class);

end Equation_Package.IO_Package.IO_Test;

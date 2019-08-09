-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Argument_Package.Argument_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);

   procedure Test_Simple_Scalar_Integer_Argument
     (The_Test : in out Test_Case'Class);
   procedure Test_Simple_Scalar_Modular_Argument
     (The_Test : in out Test_Case'Class);
   procedure Test_Multiple_Scalar_Arguments (The_Test : in out Test_Case'Class);
   procedure Test_Simple_Array_Integer_Argument
     (The_Test : in out Test_Case'Class);
   procedure Test_Simple_Array_Modular_Argument
     (The_Test : in out Test_Case'Class);
   procedure Test_Multiple_Arguments (The_Test : in out Test_Case'Class);

end Argument_Package.Argument_Test;

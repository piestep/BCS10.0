-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit;
with AUnit.Test_Fixtures;
--
with AUnit.Test_Cases; use AUnit.Test_Cases;
--

package Parameter_Package.Parameter_Test is

   type Test is new Test_Case with null record;

   overriding function Name (The_Test : in Test) return AUnit.Test_String;
   overriding procedure Register_Tests (The_Test : in out Test);

   overriding procedure Set_Up_Case (The_Test : in out Test);
   overriding procedure Tear_Down_Case (The_Test : in out Test);

   procedure Test_Scalar_Signed_Parameter (The_Test : in out Test_Case'Class);
   procedure Test_Scalar_Unsigned_Parameter (The_Test : in out Test_Case'Class);
   procedure Test_Multiple_Scalar_Parameters
     (The_Test : in out Test_Case'Class);
   procedure Test_Array_Signed_Parameter (The_Test : in out Test_Case'Class);
   procedure Test_Array_Unsigned_Parameter (The_Test : in out Test_Case'Class);
   procedure Test_Multiple_Array_Parameters (The_Test : in out Test_Case'Class);
   procedure Test_Multiple_Parameters (The_Test : in out Test_Case'Class);

end Parameter_Package.Parameter_Test;

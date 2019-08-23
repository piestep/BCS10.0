-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with AUnit.Assertions;
--
with Test_Package; use Test_Package;
--

package body Generate_Package.Parameters_Test is

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("generate_package.parameters_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Parameters'Access, "test_parameters!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
   begin
      null;
   end Tear_Down_Case;

   ---------------------
   -- Test_Parameters --
   ---------------------

   procedure Test_Parameters (The_Test : in out Test_Case'Class) is
   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Parameters;

end Generate_Package.Parameters_Test;

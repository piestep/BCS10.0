-- BCS Boolean Compiler System
-- Copyright (c) 2019 Paul Estep
pragma Ada_2012;

with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package;    use Test_Package;
with System_Package;  use System_Package;
--

package body BCode_Package.IO_Package.IO_Test is

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("bcode_package.io_package.io_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_BCode'Access, "test_bcode!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down_Case;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down;

   ----------------
   -- Test_BCode --
   ----------------

   procedure Test_BCode
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_BCode;

end BCode_Package.IO_Package.IO_Test;
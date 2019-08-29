-- BCS Boolean Compiler System
-- Copyright (c) 2019 Paul Estep
pragma Ada_2012;

with AUnit.Assertions; use AUnit.Assertions;
--
with Pool_Package;
with Test_Package;    use Test_Package;
with System_Package;  use System_Package;
--

package body BCode_Package.BCode_Test is

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("bcode_package.bcode_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Dispose'Access, "test_dispose!");
      Register_Routine(The_Test, Test_Append'Access, "test_append!");
      Register_Routine(The_Test, Test_Normalize'Access, "test_normalize!");
      Register_Routine(The_Test, Test_Number_Of'Access, "test_number_of!");
      Register_Routine(The_Test, Test_Solve'Access, "test_solve!");
      Register_Routine(The_Test, Test_Boolean_Of'Access, "test_boolean_of!");
      Register_Routine(The_Test, Test_Image_Of'Access, "test_image_of!");
      Register_Routine(The_Test, Test_Iterate'Access, "test_iterate!");
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

   ------------------
   -- Test_Dispose --
   ------------------

   procedure Test_Dispose (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Dispose;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Append;

   --------------------
   -- Test_Normalize --
   --------------------

   procedure Test_Normalize (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Normalize;

   --------------------
   -- Test_Number_Of --
   --------------------

   procedure Test_Number_Of (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Number_Of;

   ----------------
   -- Test_Solve --
   ----------------

   procedure Test_Solve (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Solve;

   ---------------------
   -- Test_Boolean_Of --
   ---------------------

   procedure Test_Boolean_Of (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Boolean_Of;

   -------------------
   -- Test_Image_Of --
   -------------------

   procedure Test_Image_Of (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Image_Of;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      AUnit.Assertions.Assert
        (False,
         "Test not implemented.");
   end Test_Iterate;

end BCode_Package.BCode_Test;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

-- To test write out an arbitrary set of variables, read the set and compare the
-- written set to the read set.
with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--

package body Variable_Package.IO_Package.IO_Test is

   FILENAME : constant String := "vario.txt";

   The_Written : Variable_Set_Type :=
     Variable_Set_Type'(True, False, True, True, others => False);

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("variable_package.io_package.io_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_Write'Access, "test_write!");
      Register_Routine (The_Test, Test_Read'Access, "test_read!");
   end Register_Tests;

   ----------------
   -- Test_Write --
   ----------------

   procedure Test_Write (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File : Storage_IO.File_Type;
   begin
      -- test write an arbitrary set of variables
      Storage_IO.Create (The_File, Storage_IO.Out_File, FILENAME);
      Write (The_File, The_Written);
      Storage_IO.Close (The_File);
   end Test_Write;

   ---------------
   -- Test_Read --
   ---------------

   procedure Test_Read (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File      : Storage_IO.File_Type;
      The_Variables : Variable_Set_Type;
   begin
      -- test read the written set and compare
      Storage_IO.Open (The_File, Storage_IO.In_File, FILENAME);
      Read (The_File, The_Variables);
      Assert
        (The_Result   => Image_Of (The_Written),
         The_Expected => Image_Of (The_Variables),
         The_Test     => "(1) variables io.");
      Storage_IO.Delete (The_File);
   end Test_Read;

end Variable_Package.IO_Package.IO_Test;

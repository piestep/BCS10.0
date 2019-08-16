-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--
with Boolean_Package; use Boolean_Package;
--

-- To test write out an arbitrary number, read the number back and compare the
-- numbers.

package body Number_Package.IO_Package.IO_Test is

   FILENAME : constant String := "numio.txt";

   The_Written : Number_Type;
   The_Number  : Number_Type;
   The_File    : Storage_IO.File_Type;

   The_Unmarked_Number_Allocations : Natural;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("number_package.io_package.io_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Write'Access, "Test_Write!");
      Register_Routine(The_Test, Test_Read'Access, "Test_Read!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
   begin
      -- 64 boolean output to match storage size
      The_Written :=
        new Number_Array_Type'
          (False,
           False,
           False,
           True,
           False,
           False,
           False,
           True,
           True,
           True,
           True,
           True,
           True,
           True,
           True,
           True,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           True,
           False,
           False,
           False,
           True,
           False,
           False,
           False,
           True,
           False,
           False,
           False,
           True,
           True,
           True,
           True,
           True,
           True,
           True,
           True,
           True,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           False,
           True,
           False,
           False,
           False,
           True);
      The_Unmarked_Number_Allocations :=
        Pool_Package.Unmarked_Allocations (Number_Package.The_Pool);
   end Set_Up_Case;

   ---------------
   -- Tear_Down_Case --
   ---------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
   begin
      Dispose (The_Written);
   end Tear_Down_Case;

   ----------------
   -- Test_Write --
   ----------------

   procedure Test_Write (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test write an arbitrary number
      Storage_IO.Create (The_File, Storage_IO.Out_File, FILENAME);
      Write (The_File, The_Written);
      Storage_IO.Close (The_File);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Number_Package.The_Pool) =
             The_Unmarked_Number_Allocations,
         "(1) Incorrect number allocations.");
   end Test_Write;

   ---------------
   -- Test_Read --
   ---------------

   procedure Raise_Read_Constraint_Error is
   begin
      -- read with incorrect variables
      Read (The_File, 2, The_Number);
   end Raise_Read_Constraint_Error;

   procedure Test_Read (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test read the written number and compare
      Storage_IO.Open (The_File, Storage_IO.In_File, FILENAME);
      Read (The_File, 6, The_Number);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected => Boolean_Of (The_Written),
         The_Test     => "(2) number io.");

      Dispose (The_Number);
      Storage_IO.Close (The_File);

      Storage_IO.Open (The_File, Storage_IO.In_File, FILENAME);
      AUnit.Assertions.Assert_Exception
        (Raise_Read_Constraint_Error'Access,
         "(3) number io.");

      Storage_IO.Delete (The_File);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Number_Package.The_Pool) =
             The_Unmarked_Number_Allocations,
         "(4) Incorrect number allocations.");
   end Test_Read;

end Number_Package.IO_Package.IO_Test;

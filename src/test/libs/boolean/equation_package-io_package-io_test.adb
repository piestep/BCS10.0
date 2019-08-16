-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--
with Pool_Package;
--

-- To test write out an arbitrary boolean equation, read the equation and
-- compare the equations.

package body Equation_Package.IO_Package.IO_Test is

   FILENAME : constant String := "equio.txt";

   The_Written : Equation_Type;

   The_Unmarked_Number_Allocations : SYSNatural;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("equation_package.io_package.io_test!");
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

   ------------
   -- Set_Of --
   ------------

   function Set_Of
     (The_Members : Variable_Array_Type) return Variable_Set_Type
   is
      The_Set : Variable_Set_Type := EMPTY_SET;
   begin
      for I in The_Members'Range loop
         Include (The_Members (I), The_Set);
      end loop;
      return The_Set;
   end Set_Of;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      The_Number : Number_Type;
   begin
      Create (The_Number);
      Normalize (The_Number, (2, (True, False, others => False)));
      The_Written :=
        Equation_Type'
          (The_Variables => Set_Of ((0, 1)), The_Number => The_Number);

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

      The_File : Storage_IO.File_Type;
   begin
      Storage_IO.Create (The_File, Storage_IO.Out_File, FILENAME);
      Write (The_File, The_Written);
      Storage_IO.Close (The_File);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Write;

   ---------------
   -- Test_Read --
   ---------------

   procedure Test_Read (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File  : Storage_IO.File_Type;
      The_Input : Equation_Type;
   begin
      Storage_IO.Open (The_File, Storage_IO.In_File, FILENAME);
      Read (The_File, The_Input);
      Assert
        (The_Result   => Boolean_Of (The_Input),
         The_Expected => Boolean_Of (The_Written),
         The_Test     => "Test-1 equation io.");

      Dispose (The_Input);
      Storage_IO.Delete (The_File);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Read;

end Equation_Package.IO_Package.IO_Test;

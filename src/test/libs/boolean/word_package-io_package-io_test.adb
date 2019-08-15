-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--
with Pool_Package;
--

package body Word_Package.IO_Package.IO_Test is

   FILENAME : constant String := "wordio.txt";

   The_Written : Word_Type;

   The_Unmarked_Number_Allocations : SYSNatural;
   The_Unmarked_Word_Allocations   : SYSNatural;

   -- create set of variables from array of variables

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

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format("Word_Package.IO_Package.IO_Test!");
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
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
      The_Equ3 : Equation_Type;
      The_Equ4 : Equation_Type;
   begin
      Create (The_Equ1, False);
      Create (The_Equ2, True);
      Create (The_Equ3, 0);

      Create (The_Equ4, 1);
      Normalize (The_Equ4, Set_Of ((1 => 0)));
      The_Written := new Word_Array_Type'(The_Equ1, The_Equ2, The_Equ3, The_Equ4);

      The_Unmarked_Number_Allocations :=
        Pool_Package.Unmarked_Allocations (Number_Package.The_Pool);
      The_Unmarked_Word_Allocations :=
        Pool_Package.Unmarked_Allocations (Word_Package.The_Pool);
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
   end Test_Write;

   ---------------
   -- Test_Read --
   ---------------

   procedure Test_Read (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_File : Storage_IO.File_Type;
      The_Word : Word_Type;
   begin
      Storage_IO.Open (The_File, Storage_IO.In_File, FILENAME);
      Read (The_File, The_Word);
      Assert
        (The_Result   => Boolean_Of (The_Word),
         The_Expected => Boolean_Of (The_Written),
         The_Test     => "(1) word io.");

      Dispose (The_Word);

      Storage_IO.Delete (The_File);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(2) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(3) Incorrect word allocations.");
   end Test_Read;

end Word_Package.IO_Package.IO_Test;

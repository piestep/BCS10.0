-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Text_IO;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with System_Package; use System_Package;
--

package body Parameter_Package.IO_Package.IO_Test is

   FILENAME : constant String := "paramio.txt";

   Multiple_Parameter_Inputs  : Parameter_List_Type;
   Multiple_Parameter_Outputs : Parameter_List_Type;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("parameter_package.io_package.io_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_Write'Access, "test_write!");
      Register_Routine (The_Test, Test_Read'Access, "test_read!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      Scalar_Unsigned : Parameter_Type;
      Scalar_Signed   : Parameter_Type;
      Array_Unsigned  : Parameter_Type;
   begin
      Scalar_Signed :=
        (The_Kind  => Scalar_Parameter,
         The_Size  => 3,
         The_First => BCInteger_To_BCModular (-4),
         The_Last  => 3,
         Is_Signed => True);

      Scalar_Unsigned :=
        (The_Kind  => Scalar_Parameter,
         The_Size  => 3,
         The_First => 0,
         The_Last  => 7,
         Is_Signed => False);

      Array_Unsigned :=
        (The_Kind   => Array_Parameter,
         The_Size   => 3,
         The_First  => 1,
         The_Last   => 3,
         Is_Signed  => False,
         The_Length => 2);

      Append (Multiple_Parameter_Inputs, Array_Unsigned);
      Append (Multiple_Parameter_Inputs, Scalar_Signed);
      Append (Multiple_Parameter_Inputs, Scalar_Unsigned);

      Append (Multiple_Parameter_Outputs, Scalar_Signed);
      Append (Multiple_Parameter_Outputs, Array_Unsigned);
      Append (Multiple_Parameter_Outputs, Scalar_Unsigned);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
   begin
      Vector (Multiple_Parameter_Inputs)  := Empty_Vector;
      Vector (Multiple_Parameter_Outputs) := Empty_Vector;
   end Tear_Down_Case;

   ----------------
   -- Test_Write --
   ----------------

   procedure Test_Write (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Save (FILENAME, Multiple_Parameter_Inputs, Multiple_Parameter_Outputs);
   end Test_Write;

   ---------------
   -- Test_Read --
   ---------------

   procedure Test_Read (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);
      The_Inputs  : Parameter_List_Type;
      The_Outputs : Parameter_List_Type;
      The_File    : Ada.Text_IO.File_Type;
   begin
      Load (FILENAME, The_Inputs, The_Outputs);

      AUnit.Assertions.Assert
        (Format (Boolean_Array_Type'(0 .. 11 => False), The_Inputs) =
         Format
           (Boolean_Array_Type'(0 .. 11 => False),
            Multiple_Parameter_Inputs),
         "Test io.");
      AUnit.Assertions.Assert
        (Format (Boolean_Array_Type'(0 .. 11 => False), The_Outputs) =
         Format
           (Boolean_Array_Type'(0 .. 11 => False),
            Multiple_Parameter_Outputs),
         "Test io.");

      Ada.Text_IO.Open (The_File, Ada.Text_IO.Out_File, FILENAME);
      Ada.Text_IO.Delete (The_File);

      Vector (The_Inputs)  := Empty_Vector;
      Vector (The_Outputs) := Empty_Vector;
   end Test_Read;

end Parameter_Package.IO_Package.IO_Test;

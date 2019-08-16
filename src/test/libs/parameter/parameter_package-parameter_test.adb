-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Ada.Containers; use Ada.Containers;
--
with Pool_Package;
--
with Test_Package; use Test_Package;
--
with System_Package;  use System_Package;
with Boolean_Package; use Boolean_Package;
--

package body Parameter_Package.Parameter_Test is

   Scalar_Signed_Parameter    : Parameter_List_Type;
   Scalar_Unsigned_Parameter  : Parameter_List_Type;
   Multiple_Scalar_Parameters : Parameter_List_Type;
   Array_Signed_Parameter     : Parameter_List_Type;
   Array_Unsigned_Parameter   : Parameter_List_Type;
   Multiple_Array_Parameters  : Parameter_List_Type;
   Multiple_Parameters        : Parameter_List_Type;

   procedure Assert is new Integer_Type_Assert (BCInteger);
   procedure Assert is new Modular_Type_Assert (SYSModular);

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("parameter_package.parameter_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_Scalar_Signed_Parameter'Access, "test_scalar_signed_parameter!");
      Register_Routine (The_Test, Test_Scalar_Unsigned_Parameter'Access, "test_scalar_unsigned_parameter!");
      Register_Routine (The_Test, Test_Multiple_Scalar_Parameters'Access, "test_multiple_scalar_parameters!");
      Register_Routine (The_Test, Test_Array_Signed_Parameter'Access, "test_array_signed_parameter!");
      Register_Routine (The_Test, Test_Array_Unsigned_Parameter'Access, "test_array_unsigned_parameter!");
      Register_Routine (The_Test, Test_Multiple_Array_Parameters'Access, "test_multiple_array_parameters!");
      Register_Routine (The_Test, Test_Multiple_Parameters'Access, "test_multiple_parameters!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      Scalar_Unsigned : Parameter_Type;
      Scalar_Signed   : Parameter_Type;
      Array_Signed    : Parameter_Type;
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

      Array_Signed :=
        (The_Kind   => Array_Parameter,
         The_Size   => 3,
         The_First  => BCInteger_To_BCModular (-2),
         The_Last   => 2,
         Is_Signed  => True,
         The_Length => 3);

      Array_Unsigned :=
        (The_Kind   => Array_Parameter,
         The_Size   => 3,
         The_First  => 1,
         The_Last   => 3,
         Is_Signed  => False,
         The_Length => 2);

      Append (Scalar_Signed_Parameter, Scalar_Signed);
      Append (Scalar_Unsigned_Parameter, Scalar_Unsigned);

      Append (Multiple_Scalar_Parameters, Scalar_Unsigned);
      Append (Multiple_Scalar_Parameters, Scalar_Signed);
      Append (Multiple_Scalar_Parameters, Scalar_Unsigned);

      Append (Array_Signed_Parameter, Array_Signed);
      Append (Array_Unsigned_Parameter, Array_Unsigned);

      Append (Multiple_Array_Parameters, Array_Unsigned);
      Append (Multiple_Array_Parameters, Array_Signed);

      Append (Multiple_Parameters, Array_Unsigned);
      Append (Multiple_Parameters, Scalar_Signed);
      Append (Multiple_Parameters, Scalar_Unsigned);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
   begin
      Vector (Scalar_Signed_Parameter)    := Empty_Vector;
      Vector (Scalar_Unsigned_Parameter)  := Empty_Vector;
      Vector (Multiple_Scalar_Parameters) := Empty_Vector;
      Vector (Array_Signed_Parameter)     := Empty_Vector;
      Vector (Array_Unsigned_Parameter)   := Empty_Vector;
      Vector (Multiple_Array_Parameters)  := Empty_Vector;
      Vector (Multiple_Parameters)        := Empty_Vector;
   end Tear_Down_Case;

   ----------------------------------
   -- Test_Scalar_Sgned_Parameter --
   ----------------------------------

   procedure Test_Scalar_Signed_Parameter
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 2);
      The_Last       : Boolean_Array_Type (0 .. 2);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
   begin

      Assert
        (The_Result   => Size_Of (Scalar_Signed_Parameter),
         The_Expected => 3,
         The_Test     => "Test 1 single scalar signed parameter (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Scalar_Signed_Parameter)),
         The_Expected => "001",
         The_Test     => "Test 2 single scalar signed parameter (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Scalar_Signed_Parameter)),
         The_Expected => "110",
         The_Test     => "Test 3 single scalar signed parameter (last of).");

      The_Index := First_Of (Scalar_Signed_Parameter);
      The_Last  := Last_Of (Scalar_Signed_Parameter);

      The_Count := 0;
      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Scalar_Signed_Parameter);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected => 7,
         The_Test     => "Test 4 single scalar signed parameter (increment).");

      Assert
        (The_Result   => Format ((True, False, True), Scalar_Signed_Parameter),
         The_Expected => "!3.-3",
         The_Test     => "Test 5 single scalar signed parameter (format).");

      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => BCInteger_To_BCModular (-4),
         The_Last  => 3,
         Is_Signed => True);

      Assert (The_Result   => SYSInteger(Length (The_Parameters)),
              The_Expected => 1,
              The_Test     => "Test 6 single scalar signed parameter (append).");

      The_Parameter := Last_Element (The_Parameters);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!3. 65532 3",
         The_Test     => "Test 7 single scalar signed parameter (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Scalar_Signed_Parameter;

   -------------------------------------
   -- Test_Scalar_Unsigned_Parameter --
   -------------------------------------

   procedure Test_Scalar_Unsigned_Parameter
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 2);
      The_Last       : Boolean_Array_Type (0 .. 2);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
   begin
      Assert
        (The_Result   => Size_Of (Scalar_Unsigned_Parameter),
         The_Expected => 3,
         The_Test     => "Test 1 single scalar unsigned parameter (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Scalar_Unsigned_Parameter)),
         The_Expected => "000",
         The_Test     => "Test 2 single scalar unsigned parameter (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Scalar_Unsigned_Parameter)),
         The_Expected => "111",
         The_Test     => "Test 3 single scalar unsigned parameter (last of).");

      The_Index := First_Of (Scalar_Unsigned_Parameter);
      The_Last  := Last_Of (Scalar_Unsigned_Parameter);
      The_Count := 0;
      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Scalar_Unsigned_Parameter);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected => 7,
         The_Test     => "Test 4 single scalar unsigned parameter (increment).");

      Assert
        (The_Result   => Format ((True, False, True), Scalar_Unsigned_Parameter),
         The_Expected => "!%3.5",
         The_Test     => "Test 5 single scalar unsigned parameter (format).");

      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => 0,
         The_Last  => 7,
         Is_Signed => False);

      Assert
        (The_Result   => SYSInteger(Length (The_Parameters)),
         The_Expected => 1,
         The_Test     => "Test 6 single scalar unsigned parameter (append).");

      The_Parameter := Last_Element (The_Parameters);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!%3. 0 7",
         The_Test     => "Test 6 single scalar unsigned parameter (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Scalar_Unsigned_Parameter;

   --------------------------------------
   -- Test_Multiple_Scalar_Parameters --
   --------------------------------------

   procedure Test_Multiple_Scalar_Parameters
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 8);
      The_Last       : Boolean_Array_Type (0 .. 8);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
      The_Cursor     : Cursor;
   begin
      Assert
        (The_Result   => Size_Of (Multiple_Scalar_Parameters),
         The_Expected => 9,
         The_Test     => "Test 1 multiple scalar parameters (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Multiple_Scalar_Parameters)),
         The_Expected => "000001000",
         The_Test     => "Test 2 multiple scalar parameters (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Multiple_Scalar_Parameters)),
         The_Expected => "111110111",
         The_Test     => "Test 3 multiple scalar parameters (last of).");

      The_Index := First_Of (Multiple_Scalar_Parameters);
      The_Last  := Last_Of (Multiple_Scalar_Parameters);
      The_Count := 0;

      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Multiple_Scalar_Parameters);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected => 511,
         The_Test     => "Test 4 multiple scalar parameters (increment).");

      Assert
        (The_Result   => Format
           ((True, False, True, True, False, True, True, False, True),
            Multiple_Scalar_Parameters),
         The_Expected => "!%3.5 !3.-3 !%3.5",
         The_Test     => "Test 5 multiple scalar parameters (format).");

      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => 0,
         The_Last  => 7,
         Is_Signed => False);
      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => BCInteger_To_BCModular (-4),
         The_Last  => 3,
         Is_Signed => True);
      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => 0,
         The_Last  => 7,
         Is_Signed => False);

      Assert
        (The_Result   => SYSInteger (Length (The_Parameters)),
         The_Expected => 3,
         The_Test     => "Test 6 multiple scalar parameters (append).");

      The_Cursor    := First (The_Parameters);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!%3. 0 7",
         The_Test     => "Test 7 multiple scalar parameters (<list>).");

      The_Cursor    := Next (The_Cursor);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!3. 65532 3",
         The_Test     => "Test 8 multiple scalar parameters (<list>).");

      The_Cursor    := Next (The_Cursor);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!%3. 0 7",
         The_Test     => "Test 9 multiple scalar parameters (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Multiple_Scalar_Parameters;

   ----------------------------------
   -- Test_Array_Signed_Parameter --
   ----------------------------------

   procedure Test_Array_Signed_Parameter
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 8);
      The_Last       : Boolean_Array_Type (0 .. 8);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
   begin
      Assert
        (The_Result   => Size_Of (Array_Signed_Parameter),
         The_Expected => 9,
         The_Test     => "Test 1 single array signed parameters (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Array_Signed_Parameter)),
         The_Expected => "011011011",
         The_Test     => "Test 2 single array signed parameters (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Array_Signed_Parameter)),
         The_Expected => "010010010",
         The_Test     => "Test 3 single array signed parameters (last of).");

      The_Index := First_Of (Array_Signed_Parameter);
      The_Last  := Last_Of (Array_Signed_Parameter);
      The_Count := 0;

      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Array_Signed_Parameter);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected => 124,
         The_Test     => "Test 4 single array signed parameters (increment).");

      Assert
        (The_Result   =>
           Format ((True, False, True, True, False, True, True, False, True),
             Array_Signed_Parameter),
         The_Expected => "@3.3 -3 -3 -3",
         The_Test     => "Test 5 single array signed parameters (format).");

      Append
        (The_Parameters,
         The_Size   => 3,
         The_First  => BCInteger_To_BCModular (-2),
         The_Last   => 2,
         Is_Signed  => True,
         The_Length => 3);

      Assert
        (The_Result   => SYSInteger(Length (The_Parameters)),
         The_Expected => 1,
         The_Test     => "Test 6 single array signed parameters (append).");

      The_Parameter := Last_Element (The_Parameters);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "@3.3 65534 2",
         The_Test     => "Test 7 single array signed parameters (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Array_Signed_Parameter;

   ------------------------------------
   -- Test_Array_Unsigned_Parameter --
   ------------------------------------

   procedure Test_Array_Unsigned_Parameter
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 5);
      The_Last       : Boolean_Array_Type (0 .. 5);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
   begin
      Assert
        (The_Result   => Size_Of (Array_Unsigned_Parameter),
         The_Expected => 6,
         The_Test     => "Test 1 single array unsigned parameters (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Array_Unsigned_Parameter)),
         The_Expected => "100100",
         The_Test     => "Test 2 single array unsigned parameters (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Array_Unsigned_Parameter)),
         The_Expected => "110110",
         The_Test     => "Test 3 single array unsigned parameters (last of).");

      The_Index := First_Of (Array_Unsigned_Parameter);
      The_Last  := Last_Of (Array_Unsigned_Parameter);
      The_Count := 0;

      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Array_Unsigned_Parameter);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected => 8,
         The_Test     => "Test 4 single array unsigned parameters (increment).");

      Assert
        (The_Result   => Format
           ((True, False, False, True, True, False),
            Array_Unsigned_Parameter),
         The_Expected => "@%3.2 1 3",
         The_Test     => "Test 5 single array unsigned parameters (format).");

      Append
        (The_Parameters,
         The_Size   => 3,
         The_First  => 1,
         The_Last   => 3,
         Is_Signed  => False,
         The_Length => 2);

      Assert
        (The_Result   => SYSInteger(Length (The_Parameters)),
         The_Expected =>  1,
         The_Test     => "Test 6 single array unsigned parameters (append).");

      The_Parameter := Last_Element (The_Parameters);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "@%3.2 1 3",
         The_Test     => "Test 7 single array unsigned parameters (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Array_Unsigned_Parameter;

   -------------------------------------
   -- Test_Multiple_Array_Parameters --
   -------------------------------------

   procedure Test_Multiple_Array_Parameters
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 14);
      The_Last       : Boolean_Array_Type (0 .. 14);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
      The_Cursor     : Cursor;
   begin
      Assert
        (The_Result   => Size_Of (Multiple_Array_Parameters),
         The_Expected =>  15,
         The_Test     => "Test 1 multiple array parameters (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Multiple_Array_Parameters)),
         The_Expected => "100100011011011",
         The_Test     => "Test 2 multiple array parameters (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Multiple_Array_Parameters)),
         The_Expected =>  "110110010010010",
         The_Test     => "Test 3 multiple array parameters (last of).");

      The_Index := First_Of (Multiple_Array_Parameters);
      The_Last  := Last_Of (Multiple_Array_Parameters);
      The_Count := 0;

      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Multiple_Array_Parameters);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected =>  1124,
         The_Test     => "Test 4 multiple array parameters (increment).");

      Assert
        (The_Result   => Format
           ((True,
            False,
            False,
            True,
            True,
            False,
            True,
            False,
            False,
            False,
            True,
            False,
            True,
            False,
            True),
            Multiple_Array_Parameters),
         The_Expected =>  "@%3.2 1 3 @3.3  1  2 -3",
         The_Test     => "Test 5 multiple array parameters (format).");

      Append
        (The_Parameters,
         The_Size   => 3,
         The_First  => 1,
         The_Last   => 3,
         Is_Signed  => False,
         The_Length => 2);
      Append
        (The_Parameters,
         The_Size   => 3,
         The_First  => BCInteger_To_BCModular (-2),
         The_Last   => 2,
         Is_Signed  => True,
         The_Length => 3);

      Assert
        (The_Result   => SYSNatural(Length (The_Parameters)),
         The_Expected =>  2,
         The_Test     => "Test 6 multiple array parameters (append).");

      The_Cursor    := First (The_Parameters);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   =>Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "@%3.2 1 3",
         The_Test     => "Test 7 multiple array parameters (<list>).");

      The_Cursor    := Next (The_Cursor);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   => Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "@3.3 65534 2",
         The_Test     => "Test 8 multiple array parameters (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Multiple_Array_Parameters;

   -------------------------------
   -- Test_Multiple_Parameters --
   -------------------------------

   procedure Test_Multiple_Parameters
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Index      : Boolean_Array_Type (0 .. 11);
      The_Last       : Boolean_Array_Type (0 .. 11);
      The_Count      : SYSModular;
      The_Parameters : Parameter_List_Type;
      The_Parameter  : Parameter_Type;
      The_Cursor     : Cursor;
   begin
      Assert
        (The_Result   => Size_Of (Multiple_Parameters),
         The_Expected => 12,
         The_Test     => "Test 1 multiple parameters (size of).");

      Assert
        (The_Result   => Image_Of (First_Of (Multiple_Parameters)),
         The_Expected => "100100001000",
         The_Test     => "Test 2 multiple parameters (first of).");

      Assert
        (The_Result   => Image_Of (Last_Of (Multiple_Parameters)),
         The_Expected => "110110110111",
         The_Test     => "Test 3 multiple parameters (last of).");

      The_Index := First_Of (Multiple_Parameters);
      The_Last  := Last_Of (Multiple_Parameters);
      The_Count := 0;

      while The_Index /= The_Last loop
         The_Count := The_Count + 1;
         Increment (The_Index, Multiple_Parameters);
      end loop;

      Assert
        (The_Result   => The_Count,
         The_Expected => 575,
         The_Test     => "Test 4 multiple parameters (increment).");

      Assert
        (The_Result   => Format
           (Boolean_Array_Type'
                (True,
                 False,
                 False,
                 True,
                 True,
                 False,
                 True,
                 False,
                 True,
                 True,
                 False,
                 True),
            Multiple_Parameters),
         The_Expected => "@%3.2 1 3 !3.-3 !%3.5",
         The_Test     => "Test 5 multiple parameters (format).");

      Append
        (The_Parameters,
         The_Size   => 3,
         The_First  => 1,
         The_Last   => 3,
         Is_Signed  => False,
         The_Length => 2);
      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => BCInteger_To_BCModular (-4),
         The_Last  => 3,
         Is_Signed => True);
      Append
        (The_Parameters,
         The_Size  => 3,
         The_First => 0,
         The_Last  => 7,
         Is_Signed => False);

      Assert
        (The_Result   => SYSNatural(Length (The_Parameters)),
         The_Expected => 3,
         The_Test     => "Test 6 multiple parameters (append).");

      -- Multiple_Parameters(Array_Unsigned, Scalar_Signed, Scalar_Unsigned)

      The_Cursor    := First (The_Parameters);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   =>  Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "@%3.2 1 3",
         The_Test     => "Test 7 multiple parameters (<list>).");

      The_Cursor    := Next (The_Cursor);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   =>  Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!3. 65532 3",
         The_Test     => "Test 8 multiple parameters (<list>).");

      The_Cursor    := Next (The_Cursor);
      The_Parameter := Element (The_Cursor);

      Assert
        (The_Result   =>  Image_Of (The_Parameter) &
           BCModular'Image(The_Parameter.The_First) &
           BCModular'Image(The_Parameter.The_Last),
         The_Expected => "!%3. 0 7",
         The_Test     => "Test 9 multiple parameters (<list>).");

      Vector (The_Parameters) := Empty_Vector;
   end Test_Multiple_Parameters;

end Parameter_Package.Parameter_Test;

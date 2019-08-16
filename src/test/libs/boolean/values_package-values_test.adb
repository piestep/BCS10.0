-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Text_IO;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--

package body Values_Package.Values_Test is

   -- return string of values using definitions and not package routines.

   function Image_Of_Test_Values (The_Values : Values_Type) return String is
   begin
      return Image_Of
        (Boolean_Array_Type
           (The_Values.The_Array (0 .. The_Values.The_Length - 1)));
   end Image_Of_Test_Values;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("values_package.values_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_Create'Access, "test_create!");
      Register_Routine (The_Test, Test_Create'Access, "test_create!");
      Register_Routine (The_Test, Test_Concatenation'Access, "test_concatenation!");
      Register_Routine (The_Test, Test_Length_Of'Access, "test_length_of!");
      Register_Routine (The_Test, Test_Image_Of'Access, "test_image_of!");
   end Register_Tests;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Values : Values_Type;
   begin
      -- test 0 values

      The_Values := Create (0);
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "*",
         The_Test     => "(1) create.");

      -- test values of length 1

      The_Values := Create (1);
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "0",
         The_Test     => "(2) create.");

      -- test values of length 2

      The_Values := Create (2);
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "00",
         The_Test     => "(3) create.");

      -- test values of max length

      The_Values := Create (Values_Size_Type'Last);
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => (1 .. Values_Size_Type'Last => '0'),
         The_Test     => "(4) create.");
   end Test_Create;

   procedure Test_Create_With_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Values : Values_Type;
   begin
      -- test 0 values

      The_Values := Create ((0 .. -1 => False));
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "*",
         The_Test     => "(5) create with array.");

      -- test values of length 1

      The_Values := Create ((0 .. 0 => False));
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "0",
         The_Test     => "(6) create with array.");

      -- test values of length 1

      The_Values := Create ((0 .. 0 => True));
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "1",
         The_Test     => "(7) create with array.");

      -- test values of length 2

      The_Values := Create ((0 => False, 1 => True));
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => "01",
         The_Test     => "(8) create with array.");

      -- test values of max length

      The_Values := Create
        ((0 .. Values_Index_Type'Last/2 => False,
          Values_Index_Type'Last/2 + 1 .. Values_Index_Type'Last => True));
      Assert
        (The_Result   => Image_Of_Test_Values (The_Values),
         The_Expected => (1 .. Values_Index_Type'Last/2 + 1 => '0',
                          Values_Index_Type'Last/2 + 2 ..
                            Values_Size_Type'Last => '1'),
         The_Test     => "(9) create.");
   end Test_Create_With_Array;

   procedure Test_Concatenation
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test two empty values
      Assert
        (The_Result   => Image_Of_Test_Values
           ((0, (Values_Index_Type'Range => False)) &
            (0, (Values_Index_Type'Range => False))),
         The_Expected => "*",
         The_Test     => "(10) concatenation.");

      -- test empty values and a length of 1
      Assert
        (The_Result   => Image_Of_Test_Values
           ((1, (Values_Index_Type'Range => False)) &
            (0, (Values_Index_Type'Range => False))),
         The_Expected => "0",
         The_Test     => "(11) concatenation.");

      -- test a length of 1 and empty values
      Assert
        (The_Result   => Image_Of_Test_Values
           ((0, (Values_Index_Type'Range => False)) &
            (1, (Values_Index_Type'Range => False))),
         The_Expected => "0",
         The_Test     => "(12) concatenation.");

      -- test two length of 1 values
      Assert
        (The_Result   => Image_Of_Test_Values
           ((1, (Values_Index_Type'Range => True)) &
            (1, (Values_Index_Type'Range => False))),
         The_Expected => "10",
         The_Test     => "(13) concatenation.");

      -- test two length of values resulting in a maximum length
      Assert
        (The_Result   => Image_Of_Test_Values
           ((Values_Size_Type'Last/2, (Values_Index_Type'Range => True)) &
            (Values_Size_Type'Last - Values_Size_Type'Last/2, (Values_Index_Type'Range => False))),
         The_Expected =>
           (1 .. Values_Size_Type'Last/2 => '1') &
         (1 .. Values_Size_Type'Last - Values_Size_Type'Last/2 => '0'),
         The_Test     => "(14) concatenation.");
   end Test_Concatenation;

   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test empty values
      Assert
        (The_Result   => Length_Of((0, (Values_Index_Type'Range => False))),
         The_Expected => 0,
         The_Test     => "(15) length of.");

      -- test length of 1 values
      Assert
        (The_Result   => Length_Of((1, (Values_Index_Type'Range => False))),
         The_Expected => 1,
         The_Test     => "(16) length of.");

      -- test maximum length of values
      Assert
        (The_Result   => Length_Of
           ((Values_Count_Type'Last,
            (Values_Index_Type'Range => False))),
         The_Expected => Values_Count_Type'Last,
         The_Test     => "(17) length of.");
   end Test_Length_Of;

   procedure Test_Image_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test empty values
      Assert
        (The_Result   => Image_Of(Values_Type'(0, (Values_Index_Type'Range => False))),
         The_Expected => "*",
         The_Test     => "(18) image of.");

      -- test length of 1 values
      Assert
        (The_Result   => Image_Of(Values_Type'(1, (Values_Index_Type'Range => False))),
         The_Expected => "0",
         The_Test     => "(19) image of.");

      -- test maximum length of flase values
      Assert
        (The_Result   => Image_Of
           (Values_Type'(Values_Count_Type'Last,
            (Values_Index_Type'Range => False))),
         The_Expected => (1 .. Values_Count_Type'Last => '0'),
         The_Test     => "(20) image of.");

      -- test maximum length of true values
      Assert
        (The_Result   => Image_Of
           (Values_Type'(Values_Count_Type'Last,
            (Values_Index_Type'Range => True))),
         The_Expected => (1 .. Values_Count_Type'Last => '1'),
         The_Test     => "(21) image of.");
   end Test_Image_Of;

end Values_Package.Values_Test;

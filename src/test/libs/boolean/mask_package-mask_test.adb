-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Text_IO;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--

package body Mask_Package.Mask_Test is

   -- return string of Mask using definitions and not package routines.

   function Image_Of_Test_Mask (The_Mask : Mask_Type) return String is
   begin
      return Image_Of
        (Boolean_Array_Type
           (The_Mask.The_Array (0 .. The_Mask.The_Length - 1)));
   end Image_Of_Test_Mask;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("mask_package.mask_test!");
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
      Register_Routine (The_Test, Test_Count_Of'Access, "test_count_of!");
      Register_Routine (The_Test, Test_Length_Of'Access, "test_length_of!");
      Register_Routine (The_Test, Test_Image_Of'Access, "test_image_of!");
   end Register_Tests;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Mask : Mask_Type;
   begin
      -- test 0 Mask

      The_Mask := Create (0);
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "*",
         The_Test     => "(1) create.");

      -- test Mask of length 1

      The_Mask := Create (1);
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "0",
         The_Test     => "(2) create.");

      -- test Mask of length 2

      The_Mask := Create (2);
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "00",
         The_Test     => "(3) create.");

      -- test Mask of max length

      The_Mask := Create (Mask_Length_Type'Last);
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => (1 .. Mask_Length_Type'Last => '0'),
         The_Test     => "(4) create.");
   end Test_Create;

   procedure Test_Create_With_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Mask : Mask_Type;
   begin
      -- test 0 Mask

      The_Mask := Create ((0 .. -1 => False));
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "*",
         The_Test     => "(5) create with array.");

      -- test Mask of length 1

      The_Mask := Create ((0 .. 0 => False));
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "0",
         The_Test     => "(6) create with array.");

      -- test Mask of length 1

      The_Mask := Create ((0 .. 0 => True));
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "1",
         The_Test     => "(7) create with array.");

      -- test Mask of length 2

      The_Mask := Create ((0 => False, 1 => True));
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => "01",
         The_Test     => "(8) create with array.");

      -- test Mask of max length

      The_Mask := Create
        ((0 .. Mask_Index_Type'Last/2 => False,
          Mask_Index_Type'Last/2 + 1 .. Mask_Index_Type'Last => True));
      Assert
        (The_Result   => Image_Of_Test_Mask (The_Mask),
         The_Expected => (1 .. Mask_Index_Type'Last/2 + 1 => '0',
                          Mask_Index_Type'Last/2 + 2 ..
                            Mask_Length_Type'Last => '1'),
         The_Test     => "(9) create.");
   end Test_Create_With_Array;

   procedure Test_Count_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test empty Mask
      Assert
        (The_Result   => Count_Of(Mask_Type'(0, (Mask_Index_Type'Range => False))),
         The_Expected => 0,
         The_Test     => "(10) count of.");

      -- test length of 1 with 0 inclusions
      Assert
        (The_Result   => Count_Of(Mask_Type'(1, (Mask_Index_Type'Range => False))),
         The_Expected => 0,
         The_Test     => "(11) count of.");

      -- test length of 1 with 1 inclusions
      Assert
        (The_Result   => Count_Of(Mask_Type'(1, (0 => True, others => False))),
         The_Expected => 1,
         The_Test     => "(12) count of.");

      -- test length of 2 with 1 inclusions
      Assert
        (The_Result   => Count_Of(Mask_Type'(2, (0 => True, others => False))),
         The_Expected => 1,
         The_Test     => "(13) count of.");

      -- test length of 2 with 2 inclusions
      Assert
        (The_Result   => Count_Of(Mask_Type'(1, (0 .. 1 => True, others => False))),
         The_Expected => 2,
         The_Test     => "(14) count of.");

      -- test maximum length of with maximim inclusions
      Assert
        (The_Result   => Count_Of
           (Mask_Type'(Mask_Count_Type'Last,
            (Mask_Index_Type'Range => True))),
         The_Expected => Mask_Count_Type'Last,
         The_Test     => "(15) count of.");
   end Test_Count_Of;

   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test empty Mask
      Assert
        (The_Result   => Length_Of((0, (Mask_Index_Type'Range => False))),
         The_Expected => 0,
         The_Test     => "(16) length of.");

      -- test length of 1 Mask
      Assert
        (The_Result   => Length_Of((1, (Mask_Index_Type'Range => False))),
         The_Expected => 1,
         The_Test     => "(17) length of.");

      -- test maximum length of Mask
      Assert
        (The_Result   => Length_Of
           ((Mask_Count_Type'Last,
            (Mask_Index_Type'Range => False))),
         The_Expected => Mask_Count_Type'Last,
         The_Test     => "(18) length of.");
   end Test_Length_Of;

   procedure Test_Image_Of (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test empty Mask
      Assert
        (The_Result   => Image_Of(Mask_Type'(0, (Mask_Index_Type'Range => False))),
         The_Expected => "*",
         The_Test     => "(19) image of.");

      -- test length of 1 Mask
      Assert
        (The_Result   => Image_Of(Mask_Type'(1, (Mask_Index_Type'Range => False))),
         The_Expected => "0",
         The_Test     => "(20) image of.");

      -- test maximum length of flase Mask
      Assert
        (The_Result   => Image_Of
           (Mask_Type'(Mask_Count_Type'Last,
            (Mask_Index_Type'Range => False))),
         The_Expected => (1 .. Mask_Count_Type'Last => '0'),
         The_Test     => "(21) image of.");

      -- test maximum length of true Mask
      Assert
        (The_Result   => Image_Of
           (Mask_Type'(Mask_Count_Type'Last,
            (Mask_Index_Type'Range => True))),
         The_Expected => (1 .. Mask_Count_Type'Last => '1'),
         The_Test     => "(22) image of.");
   end Test_Image_Of;

end Mask_Package.Mask_Test;

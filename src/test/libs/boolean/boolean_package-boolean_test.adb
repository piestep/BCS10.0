-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--

package body Boolean_Package.Boolean_Test is

   procedure Assert is new Integer_Type_Assert (BCInteger);
   procedure Assert is new Modular_Type_Assert (SYSModular);

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("Boolean.");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_Count_Of'Access, "Count_Of.");
      Register_Routine (The_Test, Test_BCInteger_At'Access, "BCInteger_At.");
      Register_Routine (The_Test, Test_BCModular_At'Access, "BCModular_At.");
      Register_Routine (The_Test, Test_Image_Of'Access, "Image_Of.");
   end Register_Tests;

   --------------------
   -- Test_Count_Of --
   --------------------

   procedure Test_Count_Of (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test null boolean array
      Assert
        (The_Result   => Count_Of ((1 .. 0 => True)),
         The_Expected => 0,
         The_Test     => "(1) boolean count of.");

      -- test arbitrary boolean array of false values
      Assert
        (The_Result   => Count_Of (Boolean_Array_Type'(0 .. 5 => False)),
         The_Expected => 0,
         The_Test     => "(2) boolean count of.");

      -- test arbitrary boolean array of true values
      Assert
        (The_Result   => Count_Of ((0 .. 5 => True)),
         The_Expected => 6,
         The_Test     => "(3) boolean count of.");

      -- test arbitrary boolean array of mixed false and true values
      Assert
        (The_Result   => Count_Of ((False, True, False, True, False, True)),
         The_Expected => 3,
         The_Test     => "(4) boolean count of.");
   end Test_Count_Of;

   ---------------------
   -- Test_Integer_At --
   ---------------------

   procedure Test_BCInteger_At (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test full length of arbitrary boolean array of false values
      Assert
        (The_Result =>
           BCInteger_At
             (The_Index      => 0,
              The_Size       => 6,
              From_The_Array => (0 .. 5 => False)),
         The_Expected => 0,
         The_Test     => "(5) boolean integer at.");

      -- test full length of arbitrary boolean array of true values
      Assert
        (The_Result =>
           BCInteger_At
             (The_Index      => 0,
              The_Size       => 6,
              From_The_Array => (0 .. 5 => True)),
         The_Expected => -1,
         The_Test     => "(6) boolean integer at.");

      -- test full length of arbitrary boolean array of mixed values
      Assert
        (The_Result =>
           BCInteger_At
             (The_Index      => 0,
              The_Size       => 6,
              From_The_Array => (False, True, False, False, False, True)),
         The_Expected => -30,
         The_Test     => "(7) boolean integer at.");

      -- test partial length of arbitrary boolean array of false values
      Assert
        (The_Result =>
           BCInteger_At
             (The_Index      => 2,
              The_Size       => 3,
              From_The_Array => (0 .. 5 => False)),
         The_Expected => 0,
         The_Test     => "(8) boolean integer at.");

      -- test partial length of arbitrary boolean array of true values
      Assert
        (The_Result =>
           BCInteger_At
             (The_Index      => 2,
              The_Size       => 3,
              From_The_Array => (0 .. 5 => True)),
         The_Expected => -1,
         The_Test     => "(9) boolean integer at.");

      -- test partial length of arbitrary boolean array of mixed values
      Assert
        (The_Result =>
           BCInteger_At
             (The_Index      => 1,
              The_Size       => 4,
              From_The_Array => (False, True, False, False, False, True)),
         The_Expected => 1,
         The_Test     => "(10) boolean integer at.");
   end Test_BCInteger_At;

   ---------------------
   -- Test_Modular_At --
   ---------------------

   procedure Test_BCModular_At (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test full length of arbitrary boolean array of false values
      Assert
        (The_Result =>
           BCModular_At
             (The_Index      => 0,
              The_Size       => 6,
              From_The_Array => (0 .. 5 => False)),
         The_Expected => 0,
         The_Test     => "(11) boolean modular at.");

      -- test full length of arbitrary boolean array of true values
      Assert
        (The_Result =>
           BCModular_At
             (The_Index      => 0,
              The_Size       => 6,
              From_The_Array => (0 .. 5 => True)),
         The_Expected => 63,
         The_Test     => "(12) boolean modular at.");

      -- test full length of arbitrary boolean array of mixed values
      Assert
        (The_Result =>
           BCModular_At
             (The_Index      => 0,
              The_Size       => 6,
              From_The_Array => (False, True, False, False, False, True)),
         The_Expected => 34,
         The_Test     => "(13) boolean modular at.");

      -- test partial length of arbitrary boolean array of false values
      Assert
        (The_Result =>
           BCModular_At
             (The_Index      => 2,
              The_Size       => 3,
              From_The_Array => (0 .. 5 => False)),
         The_Expected => 0,
         The_Test     => "(14) boolean modular at.");

      -- test partial length of arbitrary boolean array of true values
      Assert
        (The_Result =>
           BCModular_At
             (The_Index      => 2,
              The_Size       => 3,
              From_The_Array => (0 .. 5 => True)),
         The_Expected => 7,
         The_Test     => "(15) boolean modular at.");

      -- test partial length of arbitrary boolean array of mixed values
      Assert
        (The_Result =>
           BCModular_At
             (The_Index      => 1,
              The_Size       => 4,
              From_The_Array => (False, True, False, False, False, True)),
         The_Expected => 1,
         The_Test     => "(16) boolean modular at.");
   end Test_BCModular_At;

   -------------------
   -- Test_Image_Of --
   -------------------

   procedure Test_Image_Of (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- test null array
      Assert
        (The_Result   => Image_Of (Boolean_Array_Type'(1 .. 0 => False)),
         The_Expected => "*",
         The_Test     => "(17) image of.");

      -- test arbitrary boolean array of false values
      Assert
        (The_Result   => Image_Of ((0 .. 5 => False)),
         The_Expected => "000000",
         The_Test     => "(18) image of.");

      -- test arbitrary boolean array of true values
      Assert
        (The_Result   => Image_Of ((0 .. 5 => True)),
         The_Expected => "111111",
         The_Test     => "(19) image of.");

      -- test arbitrary boolean array of mixed values
      Assert
        (The_Result   => Image_Of ((False, True, False, False, False, True)),
         The_Expected => "010001",
         The_Test     => "(20) image of.");
   end Test_Image_Of;

end Boolean_Package.Boolean_Test;

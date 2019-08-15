-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with AUnit.Assertions; use AUnit.Assertions;
--
with Test_Package; use Test_Package;
--
with Argument_Package.Command_Line; use Argument_Package.Command_Line;
--

package body Argument_Package.Argument_Test is

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format("Argument_Package.Argument_Test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Simple_Scalar_Integer_Argument'Access, "Test_Simple_Scalar_Integer_Argument!");
      Register_Routine(The_Test, Test_Simple_Scalar_Modular_Argument'Access, "Test_Simple_Scalar_Modular_Argument!");
      Register_Routine(The_Test, Test_Multiple_Scalar_Arguments'Access, "Test_Multiple_Scalar_Arguments!");
      Register_Routine(The_Test, Test_Simple_Array_Integer_Argument'Access, "Test_Simple_Array_Integer_Argument!");
      Register_Routine(The_Test, Test_Simple_Array_Modular_Argument'Access, "Test_Simple_Array_Modular_Argument!");
      Register_Routine(The_Test, Test_Multiple_Arguments'Access, "Test_Multiple_Arguments!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
   begin
      null;
   end Set_Up_Case;

   -----------------------------------------
   -- Test_Simple_Scalar_Integer_Argument --
   -----------------------------------------

   procedure Test_Simple_Scalar_Integer_Argument
     (The_Test : in out Test_Case'Class)
   is
   begin
      Set_Line ("!2.-1");
      Parse
        (The_Start => 1,
         The_End   => 1);

      Assert
        (The_Result   => Number_Of,
         The_Expected => 1,
         The_Test     => "Test 1 simple scalar integer argument (number_of).");
      Assert
        (The_Result   => Length_Of,
         The_Expected => 2,
         The_Test     => "Test 2 simple scalar integer argument (length of).");
      Assert
        (The_Result   => Image_Of (Boolean_Of),
         The_Expected => "11",
         The_Test     => "Test 3 simple scalar integer argument (boolean of).");
      Assert
        (The_Result   => Image_Of,
         The_Expected => "!2.-1 * 11",
         The_Test     => "Test 4 simple scalar integer argument (image of).");
   end Test_Simple_Scalar_Integer_Argument;

   -----------------------------------------
   -- Test_Simple_Scalar_Modular_Argument --
   -----------------------------------------

   procedure Test_Simple_Scalar_Modular_Argument
     (The_Test : in out Test_Case'Class)
   is
   begin
      Set_Line ("!%2.3");
      Parse
        (The_Start => 1,
         The_End   => 1);

      Assert
        (The_Result   => Number_Of,
         The_Expected => 1,
         The_Test     => "Test 1 simple scalar modular argument (number_of).");
      Assert
        (The_Result   => Length_Of,
         The_Expected => 2,
         The_Test     => "Test 2 simple scalar modular argument (length of).");
      Assert
        (The_Result   => Image_Of (Boolean_Of),
         The_Expected => "11",
         The_Test     => "Test 3 simple scalar modular argument (boolean of).");
      Assert
        (The_Result   => Image_Of,
         The_Expected => "!%2.3 * 11",
         The_Test     => "Test 4 simple scalar modular argument (image of).");
   end Test_Simple_Scalar_Modular_Argument;

   ------------------------------------
   -- Test_Multiple_Scalar_Arguments --
   ------------------------------------

   procedure Test_Multiple_Scalar_Arguments
     (The_Test : in out Test_Case'Class)
   is
   begin
      Set_Line ("!2.1 !%3.7 !3.-4");
      Parse
        (The_Start => 1,
         The_End   => 3);

      Assert
        (The_Result   => Number_Of,
         The_Expected => 3,
         The_Test     => "Test 1 multiple scalar arguments (number_of).");
      Assert
        (The_Result   => Length_Of,
         The_Expected => 8,
         The_Test     => "Test 2 multiple scalar arguments (length of).");
      Assert
        (The_Result   => Image_Of (Boolean_Of),
         The_Expected => "10111001",
         The_Test     => "Test 3 multiple scalar arguments (boolean of).");
      Assert
        (The_Result   => Image_Of,
         The_Expected => "!2.1 !%3.7 !3.-4 * 10 111 001",
         The_Test     => "Test 4 multiple scalar arguments (image of).");
   end Test_Multiple_Scalar_Arguments;

   ----------------------------------------
   -- Test_Simple_Array_Integer_Argument --
   ----------------------------------------

   procedure Test_Simple_Array_Integer_Argument
     (The_Test : in out Test_Case'Class)
   is
   begin
      Set_Line ("@2.2 0 -1");
      Parse
        (The_Start => 1,
         The_End   => 3);

      Assert
        (The_Result   => Number_Of,
         The_Expected => 1,
         The_Test     => "Test 1 simple array integer argument (number_of).");
      Assert
        (The_Result   => Length_Of,
         The_Expected => 4,
         The_Test     => "Test 2 simple array integer argument (length of).");
      Assert
        (The_Result   => Image_Of (Boolean_Of),
         The_Expected => "0011",
         The_Test     => "Test 3 simple array integer argument (boolean of).");
      Assert
        (The_Result   => Image_Of,
         The_Expected => "@2.2 0 -1 * 00,11",
         The_Test     => "Test 4 simple array integer argument (image of).");
   end Test_Simple_Array_Integer_Argument;

   ----------------------------------------
   -- Test_Simple_Array_Modular_Argument --
   ----------------------------------------

   procedure Test_Simple_Array_Modular_Argument
     (The_Test : in out Test_Case'Class)
   is
   begin
      Set_Line ("@%2.2 0 3");
      Parse
        (The_Start => 1,
         The_End   => 3);

      Assert
        (The_Result   => Number_Of,
         The_Expected => 1,
         The_Test     => "Test 1 simple array modular argument (number_of).");
      Assert
        (The_Result   => Length_Of,
         The_Expected => 4,
         The_Test     => "Test 2 simple array modular argument (length of).");
      Assert
        (The_Result   => Image_Of (Boolean_Of),
         The_Expected => "0011",
         The_Test     => "Test 3 simple array modular argument (boolean of).");
      Assert
        (The_Result   => Image_Of,
         The_Expected => "@%2.2 0 3 * 00,11",
         The_Test     => "Test 4 simple array modular argument (image of).");
   end Test_Simple_Array_Modular_Argument;

   -----------------------------
   -- Test_Multiple_Arguments --
   -----------------------------

   procedure Test_Multiple_Arguments (The_Test : in out Test_Case'Class) is
   begin
      Set_Line ("!2.0 @3.2 -1 -2 !%3.7 !%4.16#F#");
      Parse
        (The_Start => 1,
         The_End   => 6);

      Assert
        (The_Result   => Number_Of,
         The_Expected => 4,
         The_Test     => "Test 1 multiple arguments (number_of).");
      Assert
        (The_Result   => Length_Of,
         The_Expected => 15,
         The_Test     => "Test 2 multiple arguments (length of).");
      Assert
        (The_Result   => Image_Of (Boolean_Of),
         The_Expected => "001110111111111",
         The_Test     => "Test 3 multiple arguments (boolean of).");
      Assert
        (The_Result   => Image_Of,
         The_Expected =>  "!2.0 @3.2 -1 -2 !%3.7 !%4.15 * 00 111,011 111 1111",
         The_Test     => "Test 4 multiple arguments (image of).");
   end Test_Multiple_Arguments;

end Argument_Package.Argument_Test;

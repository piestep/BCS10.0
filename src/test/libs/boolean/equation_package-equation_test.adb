-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Text_IO;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with Pool_Package;
--
with Test_Package;                   use Test_Package;
with Variable_Package.Variable_Test; use Variable_Package.Variable_Test;
with Term_Package.Term_Test;         use Term_Package.Term_Test;
--
with Boolean_Package;
--
with System_Package; use System_Package;
--

package body Equation_Package.Equation_Test is

   MAXIMUM_TEST_VARIABLES : constant Variable_Type := 20;
   MAXIMUM_TEST_TERM : constant Index_Type    := 2**MAXIMUM_TEST_VARIABLES - 1;

   -- Unmaked allocations.

   The_Unmarked_Number_Allocations : SYSNatural;

   -- create set of variables from array of variables

   function Set_Of (The_Members : Variable_Array_Type) return Variable_Set_Type;

   -- Masks
   CONSTANT_MASK  : constant Boolean_Array_Type (1 .. 0) := (1 .. 0 => False);
   SIMPLE_MASK_A0 : constant Boolean_Array_Type (0 .. 1) := (True, False);
   SIMPLE_MASK_B0 : constant Boolean_Array_Type (0 .. 1) := (False, True);
   COMPLEX_MASK   : constant Boolean_Array_Type (0 .. 1) := (0 .. 1 => True);
   MAXIMUM_TEST_MASK : constant Boolean_Array_Type
     (0 .. MAXIMUM_TEST_VARIABLES - 1) :=
     (0 .. MAXIMUM_TEST_VARIABLES - 1 => True);

   -- Number boolean arrays
   CONSTANT_TRUE  : constant Boolean_Array_Type (0 .. 0) := (0 .. 0 => True);
   CONSTANT_FALSE : constant Boolean_Array_Type (0 .. 0) := (0 .. 0 => False);
   SIMPLE_NUMBER  : constant Boolean_Array_Type (0 .. 1) := (False, True);

   -- a0B0 a0b0 -> (a0)
   COMPLEX_NUMBER_A0 : constant Boolean_Array_Type (0 .. 3) :=
     (False, True, False, True);

   -- A0b0 a0b0 -> (b0)
   COMPLEX_NUMBER_B0 : constant Boolean_Array_Type (0 .. 3) :=
     (False, False, True, True);

   -- a0b0c0 ...
   MAXIMUM_TEST_NUMBER : constant Boolean_Array_Type (0 .. MAXIMUM_TEST_TERM) :=
     (0 .. MAXIMUM_TEST_TERM - 1 => False, MAXIMUM_TEST_TERM => True);

   -- A0B0C0 ...
   MAXIMUM_TEST_NUMBER_NOT : constant Boolean_Array_Type
     (0 .. MAXIMUM_TEST_TERM) :=
     (0 => True, 1 .. MAXIMUM_TEST_TERM => False);

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

   -- create boolean equations only using definitions

   procedure Create_Test_Equation
     (The_Equation : out Equation_Type;
      The_Mask     :     Boolean_Array_Type;
      The_Array    :     Boolean_Array_Type)
   is
      The_Number    : Number_Type;
      The_Variables : Variable_Set_Type := EMPTY_SET;
      A_Mask        : Mask_Type;
   begin
      Create (The_Number, False);
      if Count_Of (The_Mask) > 0 then
         A_Mask := (Count_Of (The_Mask), (others => False));
         A_Mask.The_Array (0 .. Count_Of (The_Mask) - 1) :=
           (0 .. Count_Of (The_Mask) - 1 => True);

         Normalize (The_Number, A_Mask);
      end if;

      for I in 0 .. The_Array'Length - 1 loop
         Include (I, The_Number, The_Array (I));
      end loop;

      for I in 0 .. The_Mask'Length - 1 loop
         if The_Mask (I) then
            Include (I, The_Variables);
         else
            Exclude (I, The_Variables);
         end if;
      end loop;

      The_Equation :=
        (The_Variables => The_Variables, The_Number => The_Number);
   end Create_Test_Equation;

   -- return image of boolean equation not using equation pakage operations

   function Image_Of_Test_Equation
     (The_Equation : Equation_Type) return String is
     (Image_Of (The_Equation.The_Variables) &
        " " &
        Boolean_Of (The_Equation.The_Number));

   -- return image of boolean equation not using equation pakage operations

   function Image_Of_Maximum_Test_Variables return String is
     (IMAGE_OF_ALL_VARIABLES (1 .. 3 * (MAXIMUM_TEST_VARIABLES - 1) + 3));

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("Equation.");
   end Name;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
   begin
      The_Unmarked_Number_Allocations :=
        Pool_Package.Unmarked_Allocations (Number_Package.The_Pool);
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
   begin
      null;
   end Tear_Down;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine
        (The_Test,
         Test_Create_With_Boolean'Access,
         "Create_With_Boolean.");
      Register_Routine
        (The_Test,
         Test_Create_With_Variable'Access,
         "Create_With_Variable.");
      Register_Routine (The_Test, Test_Dispose'Access, "Dispose.");
      Register_Routine (The_Test, Test_Copy'Access, "Copy.");
      Register_Routine (The_Test, Test_Mask_Of'Access, "Mask_Of.");
      Register_Routine (The_Test, Test_Normalize'Access, "Normalize.");
      Register_Routine (The_Test, Test_Not_Op'Access, "Not_Op.");
      Register_Routine (The_Test, Test_And_Op'Access, "And_Op.");
      Register_Routine (The_Test, Test_Or_Op'Access, "Or_Op.");
      Register_Routine (The_Test, Test_Xor_Op'Access, "Xor_Op.");
      Register_Routine (The_Test, Test_Mask_Of'Access, "Mask_Of.");
      Register_Routine
        (The_Test,
         Test_Is_Constant_With_Boolean'Access,
         "Is_Constant_With_Boolean.");
      Register_Routine (The_Test, Test_Is_Constant'Access, "Is_Constant.");
      Register_Routine (The_Test, Test_To_Constant'Access, "To_Constant.");
      Register_Routine (The_Test, Test_Solve'Access, "Solve.");
      Register_Routine (The_Test, Test_Length_Of'Access, "Length_Of.");
      Register_Routine (The_Test, Test_Variables_Of'Access, "Variables_Of.");
      Register_Routine (The_Test, Test_Number_Of'Access, "Number_Of.");
      Register_Routine (The_Test, Test_Boolean_Of'Access, "Boolean_Of.");
      Register_Routine (The_Test, Test_Image_Of'Access, "Image_Of.");
   end Register_Tests;

   ------------------------------
   -- Test_Create_With_Boolean --
   ------------------------------

   procedure Test_Create_With_Boolean
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Equation : Equation_Type;
   begin
      -- test true equation
      Create (The_Equation, True);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Equation),
         The_Expected => " 1",
         The_Test     => "(1)  create with boolean.");

      Dispose (The_Equation);

      -- test false equation
      Create (The_Equation, False);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Equation),
         The_Expected => " 0",
         The_Test     => "(2) create with boolean.");

      Dispose (The_Equation);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Create_With_Boolean;

   -------------------------------
   -- Test_Create_With_Variable --
   -------------------------------

   procedure Test_Create_With_Variable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Equation : Equation_Type;
   begin
      -- test 1st variable equation
      Create (The_Equation, 0);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Equation),
         The_Expected => " a0 01",
         The_Test     => "(3) create with variable.");

      Dispose (The_Equation);

      -- test 2nd variable equation
      Create (The_Equation, 1);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Equation),
         The_Expected => " b0 01",
         The_Test     => "(4) create with variable.");

      Dispose (The_Equation);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Create_With_Variable;

   ------------------
   -- Test_Dispose --
   ------------------

   procedure Test_Dispose
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Equ : Equation_Type;
   begin
      -- test simple equation
      Create (The_Equ, 0);
      Dispose (The_Equ);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Dispose;

   ---------------
   -- Test_Copy --
   ---------------

   procedure Test_Copy (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      The_Left  : Equation_Type;
      The_Right : Equation_Type;
   begin
      -- test false equation
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Copy (The_Left, The_Right);
      Dispose (The_Left);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " 0",
         The_Test     => "(5) copy.");

      Dispose (The_Right);

      -- test true equation
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_TRUE);
      Copy (The_Left, The_Right);
      Dispose (The_Left);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " 1",
         The_Test     => "(6) copy.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Left, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Copy (The_Left, The_Right);
      Dispose (The_Left);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 01",
         The_Test     => "(7) copy.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Left, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Copy (The_Left, The_Right);
      Dispose (The_Left);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 0101",
         The_Test     => "(8) copy.");

      Dispose (The_Right);

      -- test complex equation (b0)
      Create_Test_Equation (The_Left, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Copy (The_Left, The_Right);
      Dispose (The_Left);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 0011",
         The_Test     => "(9) copy.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Left, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Copy (The_Left, The_Right);
      Dispose (The_Left);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected =>
           Image_Of_Maximum_Test_Variables &
           " " &
         (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(10) copy.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Copy;

   ------------------
   -- Test_Mask_Of --
   ------------------

   procedure Test_Mask_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

   begin
      -- test empty set and empty set
      Assert
        (The_Result   => Image_Of (Mask_Of (EMPTY_SET, EMPTY_SET)),
         The_Expected => "*",
         The_Test     => "(11) mask of.");

      -- test set (0) and empty set
      Assert
        (The_Result   => Image_Of (Mask_Of (Set_Of ((1 => 0)), EMPTY_SET)),
         The_Expected => "1",
         The_Test     => "(12) mask of.");

      -- test set ( 0 ) and set ( 0 )
      Assert
        (The_Result =>
           Image_Of (Mask_Of (Set_Of ((1 => 0)), Set_Of ((1 => 0)))),
         The_Expected => "0",
         The_Test     => "(13) mask of.");

      -- test set ( 0, 1 ) and set ( 0 )
      Assert
        (The_Result => Image_Of (Mask_Of (Set_Of ((0, 1)), Set_Of ((1 => 0)))),
         The_Expected => "01",
         The_Test     => "(14) mask of.");

      -- test set ( 0, 1, N ) and set ( 1 )
      Assert
        (The_Result =>
           Image_Of
             (Mask_Of (Set_Of ((0, 1, MAX_VARIABLE)), Set_Of ((1 => 1)))),
         The_Expected => "101",
         The_Test     => "(15) mask of.");

      -- test set ( 0, 1, N ) and set ( 0, N )
      Assert
        (The_Result =>
           Image_Of
             (Mask_Of
                  (Set_Of ((0, 1, MAX_VARIABLE)),
                   Set_Of ((0, MAX_VARIABLE)))),
         The_Expected => "010",
         The_Test     => "(16) mask of.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Mask_Of;

   --------------------
   -- Test_Normalize --
   --------------------

   procedure Test_Normalize
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation false with empty mask
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Normalize (The_Right, Set_Of ((1 .. 0 => 0)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " 0",
         The_Test     => "(17) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test constant equation true with empty mask
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Normalize (The_Right, Set_Of ((1 .. 0 => 0)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " 1",
         The_Test     => "(18) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test constant equation true with mask a0
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Normalize (The_Right, Set_Of ((1 => 0)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 11",
         The_Test     => "(19) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test constant equation true with mask b0
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Normalize (The_Right, Set_Of ((1 => 1)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " b0 11",
         The_Test     => "(20) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test simple equation (a0) with mask b0
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Normalize (The_Right, Set_Of ((1 => 1)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 0101",
         The_Test     => "(21) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test simple equation (b0) with mask a0
      Create_Test_Equation (The_Right, SIMPLE_MASK_B0, SIMPLE_NUMBER);
      Normalize (The_Right, Set_Of ((1 => 0)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 0011",
         The_Test     => "(22) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test complex equation (a0) with mask d0
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Normalize (The_Right, Set_Of ((1 => 3)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 d0 01010101",
         The_Test     => "(23) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test complex equation (b0) with mask c0
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Normalize (The_Right, Set_Of ((1 => 2)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 c0 00110011",
         The_Test     => "(24) normalize.",
         The_Text     => " " & Image_Of (The_Right));

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...N0) with mask N0 +1
      Create_Test_Equation
        (The_Right,
         (0 .. MAXIMUM_TEST_VARIABLES - 2 => True),
         (0 .. MAXIMUM_TEST_TERM / 2 - 1 => False,
          MAXIMUM_TEST_TERM / 2          => True));
      Normalize (The_Right, Set_Of ((1 => MAXIMUM_TEST_VARIABLES - 1)));
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected =>
           Image_Of_Maximum_Test_Variables &
           " " &
         (1 .. MAXIMUM_TEST_TERM / 2                     => '0',
          MAXIMUM_TEST_TERM / 2 + 1                      => '1',
          MAXIMUM_TEST_TERM / 2 + 2 .. MAXIMUM_TEST_TERM => '0',
          MAXIMUM_TEST_TERM + 1                          => '1'),
         The_Test => "(25) normalize.",
         The_Text => " " & Image_Of (The_Right));

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Normalize;

   -----------------
   -- Test_Not_Op --
   -----------------

   procedure Test_Not_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      The_Right : Equation_Type;
   begin
      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " 1",
         The_Test     => "(26) not.");

      Dispose (The_Right);

      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " 0",
         The_Test     => "(27) not.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 10",
         The_Test     => "(28) not.");

      Dispose (The_Right);

      -- test simple equation (b0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_B0, SIMPLE_NUMBER);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " b0 10",
         The_Test     => "(29) not.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 1010",
         The_Test     => "(30) not.");

      Dispose (The_Right);

      -- test complex equation (b0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected => " a0 b0 1100",
         The_Test     => "(31) not.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Not_Op (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Right),
         The_Expected =>
           Image_Of_Maximum_Test_Variables &
           " " &
         (1 .. MAXIMUM_TEST_TERM => '1', MAXIMUM_TEST_TERM + 1 => '0'),
         The_Test => "(32) not.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Not_Op;

   -----------------
   -- Test_And_Op --
   -----------------

   procedure Test_And_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      The_Left  : Equation_Type;
      The_Right : Equation_Type;
   begin
      -- test constant equations false false
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      And_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 0",
         The_Test     => "(33). and.");

      Dispose (The_Left);

      -- test constant equations false true
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      And_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 0",
         The_Test     => "(34). and.");

      Dispose (The_Left);

      -- test constant equations true false
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_TRUE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      And_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 0",
         The_Test     => "(35). and.");

      Dispose (The_Left);

      -- test constant equations true true
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_TRUE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      And_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 1",
         The_Test     => "(36). and.");

      Dispose (The_Left);

      -- test simple equations (a0) (b0)
      Create_Test_Equation (The_Left, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Create_Test_Equation (The_Right, SIMPLE_MASK_B0, SIMPLE_NUMBER);
      And_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " a0 b0 0001",
         The_Test     => "(37). and.");

      Dispose (The_Left);

      -- test complex equations (a0) (b0)
      Create_Test_Equation (The_Left, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      And_Op(The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result => Image_Of_Test_Equation(The_Left),
         The_Expected => " a0 b0 0001",
         The_Test => "(38). and.");

      Dispose (The_Left);

      -- test maximum test equations (a0b0c0...) (A0B0C0...)
      Create_Test_Equation (The_Left, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER_NOT);
      And_Op(The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result => Image_Of_Test_Equation(The_Left),
         The_Expected =>
           Image_Of_Maximum_Test_Variables &
           " " &
         (1 .. MAXIMUM_TEST_TERM + 1 => '0'),
         The_Test => "(39). and.");

      Dispose (The_Left);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_And_Op;

   ----------------
   -- Test_Or_Op --
   ----------------

   procedure Test_Or_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      The_Left  : Equation_Type;
      The_Right : Equation_Type;
   begin
      -- test constant equations false false
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Or_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 0",
         The_Test     => "(40). or.");

      Dispose (The_Left);

      -- test constant equations false true
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Or_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 1",
         The_Test     => "(41). or.");

      Dispose (The_Left);

      -- test simple equations (a0) (b0)
      Create_Test_Equation (The_Left, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Create_Test_Equation (The_Right, SIMPLE_MASK_B0, SIMPLE_NUMBER);
      Or_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " a0 b0 0111",
         The_Test     => "(42). or.");

      Dispose (The_Left);

      -- test complex equations (n0) (x0)
      Create_Test_Equation (The_Left, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Or_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " a0 b0 0111",
         The_Test     => "(43) or.");

      Dispose (The_Left);

      -- test maximum test equations (a0b0c0...) (A0B0C0...)
      Create_Test_Equation (The_Left, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Create_Test_Equation
        (The_Right,
         MAXIMUM_TEST_MASK,
         MAXIMUM_TEST_NUMBER_NOT);
      Or_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected =>
           Image_Of_Maximum_Test_Variables &
           " " &
         (1                      => '1',
          2 .. MAXIMUM_TEST_TERM => '0',
          MAXIMUM_TEST_TERM + 1  => '1'),
         The_Test => "(44). or.");

      Dispose (The_Left);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Or_Op;

   -----------------
   -- Test_Xor_Op --
   -----------------

   procedure Test_Xor_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      The_Left  : Equation_Type;
      The_Right : Equation_Type;
   begin
      -- test constant equations false false
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 0",
         The_Test     => "(45). xor.");

      Dispose (The_Left);

      -- test constant equations false true
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_FALSE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 1",
         The_Test     => "(46). xor.");

      Dispose (The_Left);

      -- test constant equations true false
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_TRUE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 1",
         The_Test     => "(47). xor.");

      Dispose (The_Left);

      -- test constant equations true true
      Create_Test_Equation (The_Left, CONSTANT_MASK, CONSTANT_TRUE);
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " 0",
         The_Test     => "(48). xor.");

      Dispose (The_Left);

      -- test simple equations (a0) (b0)
      Create_Test_Equation (The_Left, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Create_Test_Equation (The_Right, SIMPLE_MASK_B0, SIMPLE_NUMBER);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " a0 b0 0110",
         The_Test     => "(49). xor.");

      Dispose (The_Left);

      -- test complex equations (n0) (x0)
      Create_Test_Equation (The_Left, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected => " a0 b0 0110",
         The_Test     => "(50) xor.");

      Dispose (The_Left);

      -- test maximum test equations (a0b0c0...) (A0B0C0...)
      Create_Test_Equation (The_Left, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Create_Test_Equation
        (The_Right,
         MAXIMUM_TEST_MASK,
         MAXIMUM_TEST_NUMBER_NOT);
      Xor_Op (The_Left, The_Right);
      Dispose (The_Right);
      Assert
        (The_Result   => Image_Of_Test_Equation (The_Left),
         The_Expected =>
           Image_Of_Maximum_Test_Variables &
           " " &
         (1                      => '1',
          2 .. MAXIMUM_TEST_TERM => '0',
          MAXIMUM_TEST_TERM + 1  => '1'),
         The_Test => "(51). xor.");

      Dispose (The_Left);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Xor_Op;

   -----------------------------------
   -- Test_Is_Constant_With_Boolean --
   -----------------------------------

   procedure Test_Is_Constant_With_Boolean
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Constant (The_Right, True),
         The_Expected => True,
         The_Test     => "(52) is constant with boolean.");

      Dispose (The_Right);

      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Constant (The_Right, False),
         The_Expected => False,
         The_Test     => "(53) is constant with boolean.");

      Dispose (The_Right);

      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Is_Constant (The_Right, True),
         The_Expected => False,
         The_Test     => "(54) is constant with boolean.");

      Dispose (The_Right);

      -- test constant false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Is_Constant (The_Right, False),
         The_Expected => True,
         The_Test     => "(55) is constant with boolean.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Right, True),
         The_Expected => False,
         The_Test     => "(56) is constant with boolean.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Is_Constant (The_Right, False),
         The_Expected => False,
         The_Test     => "(57) is constant with boolean.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Right, False),
         The_Expected => False,
         The_Test     => "(58) is constant with boolean.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Right, True),
         The_Expected => False,
         The_Test     => "(59) is constant with boolean.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Is_Constant_With_Boolean;

   ----------------------
   -- Test_Is_Constant --
   ----------------------

   procedure Test_Is_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Constant (The_Right),
         The_Expected => True,
         The_Test     => "(60) is constant.");

      Dispose (The_Right);

      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Is_Constant (The_Right),
         The_Expected => True,
         The_Test     => "(61) is constant.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Right),
         The_Expected => False,
         The_Test     => "(62) is constant.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Is_Constant (The_Right),
         The_Expected => False,
         The_Test     => "(63) is constant.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Right),
         The_Expected => False,
         The_Test     => "(64) is constant.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Is_Constant;

   ----------------------
   -- Test_To_Constant --
   ----------------------

   procedure Test_To_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => To_Constant (The_Right),
         The_Expected => True,
         The_Test     => "(65) to constant.");

      Dispose (The_Right);

      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => To_Constant (The_Right),
         The_Expected => False,
         The_Test     => "(66) to constant.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_To_Constant;

   ----------------
   -- Test_Solve --
   ----------------

   procedure Test_Solve (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      The_Right : Equation_Type;
   begin
      -- test constant equation false with no values
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Solve (The_Right, (1 .. 0 => True)),
         The_Expected => False,
         The_Test     => "(67) solve.");

      Dispose (The_Right);

      -- test constant equation true with no values
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Solve (The_Right, (1 .. 0 => False)),
         The_Expected => True,
         The_Test     => "(68) solve.");

      Dispose (The_Right);

      -- test constant equation false with one false value
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Solve (The_Right, (0 .. 0 => True)),
         The_Expected => False,
         The_Test     => "(69) solve.");

      Dispose (The_Right);

      -- test constant equation true with one false value
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Solve (The_Right, (0 .. 0 => False)),
         The_Expected => True,
         The_Test     => "(70) solve.");

      Dispose (The_Right);

      -- test constant equation false with arbitrary values
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Solve (The_Right, (False, True, True)),
         The_Expected => False,
         The_Test     => "(71) solve.");

      Dispose (The_Right);

      -- test constant equation true with arbitrary values
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Solve (The_Right, (False, False, False)),
         The_Expected => True,
         The_Test     => "(72) solve.");

      Dispose (The_Right);

      -- test simple equation (a0) with one true value
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Solve (The_Right, (0 => True)),
         The_Expected => True,
         The_Test     => "(73) solve.");

      Dispose (The_Right);

      -- test simple equation (a0) with one false value
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Solve (The_Right, (0 => False)),
         The_Expected => False,
         The_Test     => "(74) solve.");

      Dispose (The_Right);

      -- test simple equation (a0) with arbitrary values
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Solve (The_Right, (True, False, False)),
         The_Expected => True,
         The_Test     => "(75) solve.");

      Dispose (The_Right);

      -- test simple equation (a0) with arbitrary values
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Solve (The_Right, (False, True, True)),
         The_Expected => False,
         The_Test     => "(76) solve.");

      Dispose (The_Right);

      -- test complex equation (a0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Solve (The_Right, (False, False)),
         The_Expected => False,
         The_Test     => "(77) solve.");

      Dispose (The_Right);

      -- test complex equation (a0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Solve (The_Right, (True, False)),
         The_Expected => True,
         The_Test     => "(78) solve.");

      Dispose (The_Right);

      -- test complex equation (a0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Solve (The_Right, (False, True)),
         The_Expected => False,
         The_Test     => "(79) solve.");

      Dispose (The_Right);

      -- test complex equation (a0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Solve (The_Right, (True, True)),
         The_Expected => True,
         The_Test     => "(80) solve.");

      Dispose (The_Right);

      -- test complex equation (b0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Solve (The_Right, (False, False)),
         The_Expected => False,
         The_Test     => "(81) solve.");

      Dispose (The_Right);

      -- test complex equation (b0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Solve (The_Right, (True, False)),
         The_Expected => False,
         The_Test     => "(82) solve.");

      Dispose (The_Right);

      -- test complex equation (b0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Solve (The_Right, (False, True)),
         The_Expected => True,
         The_Test     => "(83) solve.");

      Dispose (The_Right);

      -- test complex equation (b0) with two values
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Solve (The_Right, (True, True)),
         The_Expected => True,
         The_Test     => "(84) solve.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Solve;

   --------------------
   -- Test_Length_Of --
   --------------------

   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Length_Of (The_Right),
         The_Expected => 1,
         The_Test     => "(85) length of.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Length_Of (The_Right),
         The_Expected => 2,
         The_Test     => "(86) length of.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Length_Of (The_Right),
         The_Expected => 4,
         The_Test     => "(87) length of.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Length_Of (The_Right),
         The_Expected => 2**MAXIMUM_TEST_VARIABLES,
         The_Test     => "(88) length of.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Length_Of;

   -----------------------
   -- Test_Variables_Of --
   -----------------------

   procedure Test_Variables_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Image_Of (Variables_Of (The_Right)),
         The_Expected => "",
         The_Test     => "(89) variables of.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Image_Of (Variables_Of (The_Right)),
         The_Expected => Image_Of (Set_Of ((1 => 0))),
         The_Test     => "(90) variables of.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Image_Of (Variables_Of (The_Right)),
         The_Expected => Image_Of (Set_Of ((0, 1))),
         The_Test     => "(91) variables of.");

      Dispose (The_Right);

      -- test complex equation (b0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Image_Of (Variables_Of (The_Right)),
         The_Expected => Image_Of (Set_Of ((0, 1))),
         The_Test     => "(92) variables of.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Image_Of (Variables_Of (The_Right)),
         The_Expected => Image_Of_Maximum_Test_Variables,
         The_Test     => "(93) variables of.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Variables_Of;

   --------------------
   -- Test_Number_Of --
   --------------------

   procedure Test_Number_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Boolean_Of (Number_Of (The_Right)),
         The_Expected => "1",
         The_Test     => "(94) number of.");

      Dispose (The_Right);

      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Boolean_Of (Number_Of (The_Right)),
         The_Expected => "0",
         The_Test     => "(95) number of.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Boolean_Of (Number_Of (The_Right)),
         The_Expected => "01",
         The_Test     => "(96) number of.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Boolean_Of (Number_Of (The_Right)),
         The_Expected => "0101",
         The_Test     => "(97) number of.");

      Dispose (The_Right);

      -- test complex equation (b0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Boolean_Of (Number_Of (The_Right)),
         The_Expected => "0011",
         The_Test     => "(98) number of.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Boolean_Of (Number_Of (The_Right)),
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(99) number of.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Number_Of;

   ---------------------
   -- Test_Boolean_Of --
   ---------------------

   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Boolean_Of (The_Right),
         The_Expected => "f( ) 1",
         The_Test     => "(100) boolean of.");

      Dispose (The_Right);

      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Boolean_Of (The_Right),
         The_Expected => "f( ) 0",
         The_Test     => "(101) boolean of.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Boolean_Of (The_Right),
         The_Expected => "f( a0 ) 01",
         The_Test     => "(102) boolean of.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Boolean_Of (The_Right),
         The_Expected => "f( a0 b0 ) 0101",
         The_Test     => "(103) boolean of.");

      Dispose (The_Right);

      -- test complex equation (b0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Boolean_Of (The_Right),
         The_Expected => "f( a0 b0 ) 0011",
         The_Test     => "(104) boolean of.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Boolean_Of (The_Right),
         The_Expected =>
           "f(" &
           Image_Of_Maximum_Test_Variables &
           " ) " &
         (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(105) boolean of.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Boolean_Of;

   -------------------
   -- Test_Image_Of --
   -------------------

   procedure Test_Image_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is

      The_Right : Equation_Type;
   begin
      -- test constant equation true
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_TRUE);
      Assert
        (The_Result   => Image_Of (The_Right),
         The_Expected => "f( ) 1",
         The_Test     => "(106) image of.");

      Dispose (The_Right);

      -- test constant equation false
      Create_Test_Equation (The_Right, CONSTANT_MASK, CONSTANT_FALSE);
      Assert
        (The_Result   => Image_Of (The_Right),
         The_Expected => "f( ) 0",
         The_Test     => "(107) image of.");

      Dispose (The_Right);

      -- test simple equation (a0)
      Create_Test_Equation (The_Right, SIMPLE_MASK_A0, SIMPLE_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Right),
         The_Expected => "f( a0 ) a0",
         The_Test     => "(108) image of.");

      Dispose (The_Right);

      -- test complex equation (a0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_A0);
      Assert
        (The_Result   => Image_Of (The_Right),
         The_Expected => "f( a0 b0 ) a0B0 a0b0",
         The_Test     => "(109) image of.");

      Dispose (The_Right);

      -- test complex equation (b0)
      Create_Test_Equation (The_Right, COMPLEX_MASK, COMPLEX_NUMBER_B0);
      Assert
        (The_Result   => Image_Of (The_Right),
         The_Expected => "f( a0 b0 ) A0b0 a0b0",
         The_Test     => "(110) image of.");

      Dispose (The_Right);

      -- test maximum test equation (a0b0c0...)
      Create_Test_Equation (The_Right, MAXIMUM_TEST_MASK, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Right),
         The_Expected =>
           "f(" &
           Image_Of_Maximum_Test_Variables &
           " ) " &
           IMAGE_OF_ALL_TERMS (1 .. 2 * MAXIMUM_TEST_VARIABLES),
         The_Test => "(111) image of.");

      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
   end Test_Image_Of;

end Equation_Package.Equation_Test;

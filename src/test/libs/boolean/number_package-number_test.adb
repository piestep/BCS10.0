-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO;
--
with AUnit.Assertions; use AUnit.Assertions;
--
with Pool_Package;
--
with Test_Package;           use Test_Package;
with Term_Package.Term_Test; use Term_Package.Term_Test;
--
with Boolean_Package; use Boolean_Package;
--

package body Number_Package.Number_Test is

   MAXIMUM_TEST_VARIABLES : constant Variable_Type := 20;
   MAXIMUM_TEST_TERM : constant Index_Type    := 2**MAXIMUM_TEST_VARIABLES - 1;

   CONSTANT_TRUE : constant Number_Array_Type (0 .. 0) := (0 .. 0 => True);
   CONSTANT_FALSE : constant Number_Array_Type (0 .. 0) := (0 .. 0 => False);
   SIMPLE_NUMBER : constant Number_Array_Type (0 .. 1) := (False, True); -- a0
   TWO_VARIABLES_NUMBER_A0 : constant Number_Array_Type (0 .. 3) :=
     (False, False, True, True);
   TWO_VARIABLES_NUMBER_B0 : constant Number_Array_Type (0 .. 3) :=
     (False, True, False, True);

   MAXIMUM_TEST_NUMBER : constant Number_Array_Type (0 .. MAXIMUM_TEST_TERM) :=
     (0 .. MAXIMUM_TEST_TERM - 1 => False, MAXIMUM_TEST_TERM => True);

   -- Unmaked allocations.

   The_Unmarked_Number_Allocations : SYSNatural;

   -- create number only using definitions

   procedure Create_Test_Number
     (The_Number : out Number_Type;
      The_Array  :     Number_Array_Type)
   is
   begin
      The_Number := new Number_Array_Type'(The_Array);
   end Create_Test_Number;

   -- return image of number not using number pakage operations

   function Image_Of_Test_Number
     (The_Number : Number_Type) return String is
     (Image_Of (Boolean_Array_Type (The_Number.all)));

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format("Number_Package.Number_Test!");
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
      Register_Routine (The_Test, Test_Dispose'Access, "Dispose.");
      Register_Routine (The_Test, Test_Create'Access, "Create.");
      Register_Routine
        (The_Test,
         Test_Create_With_Value'Access,
         "Create_With_Value.");
      Register_Routine (The_Test, Test_Copy'Access, "Copy.");
      Register_Routine (The_Test, Test_Not_Op'Access, "Not_Op.");
      Register_Routine (The_Test, Test_And_Op'Access, "And_Op.");
      Register_Routine (The_Test, Test_Or_Op'Access, "Or_Op.");
      Register_Routine (The_Test, Test_Xor_Op'Access, "Xor_Op.");
      Register_Routine (The_Test, Test_Normalize'Access, "Normalize.");
      Register_Routine (The_Test, Test_Include'Access, "Include.");
      Register_Routine
        (The_Test,
         Test_Is_Included_With_FPT'Access,
         "Is_Included_With_FPT.");
      Register_Routine (The_Test, Test_Include_Index'Access, "Include_Index.");
      Register_Routine
        (The_Test,
         Test_Is_Included_With_Index'Access,
         "Is_Included_With_Index.");
      Register_Routine
        (The_Test,
         Test_Is_Constant_Of_Boolean'Access,
         "Is_Constant_Of_Boolean.");
      Register_Routine (The_Test, Test_Is_Constant'Access, "Is_Constant.");
      Register_Routine (The_Test, Test_To_Constant'Access, "To_Constant.");
      Register_Routine (The_Test, Test_Length_Of'Access, "Length_Of.");
      Register_Routine (The_Test, Test_Boolean_Of'Access, "Boolean_Of.");
      Register_Routine (The_Test, Test_Image_Of'Access, "Image_Of.");
      Register_Routine
        (The_Test,
         Test_Image_Of_With_Variables'Access,
         "Image_Of_With_Variables.");
      Register_Routine (The_Test, Test_Pool'Access, "Pool.");
   end Register_Tests;

   ------------------
   -- Test_Dispose --
   ------------------

   procedure Test_Dispose
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test simple characteristic number
      Create (The_Number);
      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(1) Incorrect number allocations.");
   end Test_Dispose;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      The_Number : Number_Type;
   begin
      -- test create simple number (a0)
      Create (The_Number);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "01",
         The_Test     => "(2) create.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(3) Incorrect number allocations.");
   end Test_Create;

   ----------------------------
   -- Test_Create_With_Value --
   ----------------------------

   procedure Test_Create_With_Value
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test create constant number true
      Create (The_Number, True);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "1",
         The_Test     => "(4) create with value.");

      Dispose (The_Number);

      -- test create constant number false
      Create (The_Number, False);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "0",
         The_Test     => "(5) create with value.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(6) Incorrect number allocations.");
   end Test_Create_With_Value;

   ---------------
   -- Test_Copy --
   ---------------

   procedure Test_Copy (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      procedure Test_Copy_Case
        (The_Array    : Number_Array_Type;
         The_Expected : String;
         The_Test     : String)
      is
         The_Left   : Number_Type;
         The_Number : Number_Type;
      begin
         Create_Test_Number (The_Left, The_Array);
         Copy (The_Left, The_Number);
         Dispose (The_Left);
         Assert
           (The_Result   => Image_Of_Test_Number (The_Number),
            The_Expected => The_Expected,
            The_Test     => The_Test);

         Dispose (The_Number);
      end Test_Copy_Case;

   begin
      -- test constant number true
      Test_Copy_Case
        (The_Array    => CONSTANT_TRUE,
         The_Expected => "1",
         The_Test     => "(7) copy.");

      -- test constant number false
      Test_Copy_Case
        (The_Array    => CONSTANT_FALSE,
         The_Expected => "0",
         The_Test     => "(8) copy.");

      -- test simple number, a0
      Test_Copy_Case
        (The_Array    => SIMPLE_NUMBER,
         The_Expected => "01",
         The_Test     => "(9) copy.");

      -- test two variable number, a0
      Test_Copy_Case
        (The_Array    => TWO_VARIABLES_NUMBER_A0,
         The_Expected => "0011",
         The_Test     => "(10) copy.");

      -- test two variable number, a0
      Test_Copy_Case
        (The_Array    => TWO_VARIABLES_NUMBER_B0,
         The_Expected => "0101",
         The_Test     => "(11) copy.");

      -- test maximum test number (a0b0c0 ...)
      Test_Copy_Case
        (The_Array    => MAXIMUM_TEST_NUMBER,
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(12) copy.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(13) Incorrect number allocations.");
   end Test_Copy;

   -----------------
   -- Test_Not_Op --
   -----------------

   procedure Test_Not_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      procedure Test_Not_Case
        (The_Array    : Number_Array_Type;
         The_Expected : String;
         The_Test     : String)
      is
         The_Right : Number_Type;
      begin
         Create_Test_Number (The_Right, The_Array);
         Not_Op (The_Right);
         Assert
           (The_Result   => Image_Of_Test_Number (The_Right),
            The_Expected => The_Expected,
            The_Test     => The_Test);
         Dispose (The_Right);
      end Test_Not_Case;

   begin
      -- test constant number true
      Test_Not_Case
        (The_Array    => CONSTANT_TRUE,
         The_Expected => "0",
         The_Test     => "(14) not.");

      -- test constant number false
      Test_Not_Case
        (The_Array    => CONSTANT_FALSE,
         The_Expected => "1",
         The_Test     => "(15) not.");

      -- test simple number, a0
      Test_Not_Case
        (The_Array    => SIMPLE_NUMBER,
         The_Expected => "10",
         The_Test     => "(16) not.");

      -- test two variable, a0
      Test_Not_Case
        (The_Array    => TWO_VARIABLES_NUMBER_A0,
         The_Expected => "1100",
         The_Test     => "(17) not.");

      -- test two variable, b0
      Test_Not_Case
        (The_Array    => TWO_VARIABLES_NUMBER_B0,
         The_Expected => "1010",
         The_Test     => "(18) not.");

      -- test maximum test number, a0b0c0...
      Test_Not_Case
        (The_Array    => MAXIMUM_TEST_NUMBER,
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM => '1', MAXIMUM_TEST_TERM + 1 => '0'),
         The_Test => "(19) not.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(20) Incorrect number allocations.");
   end Test_Not_Op;

   -----------------
   -- Test_And_Op --
   -----------------

   procedure Test_And_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      procedure Test_And_Case
        (Array_Left   : Number_Array_Type;
         Array_Right  : Number_Array_Type;
         The_Expected : String;
         The_Test     : String)
      is
         The_Left  : Number_Type;
         The_Right : Number_Type;
      begin
         Create_Test_Number (The_Left, Array_Left);
         Create_Test_Number (The_Right, Array_Right);
         And_Op (The_Left, The_Right);
         Dispose (The_Right);
         Assert
           (The_Result   => Image_Of_Test_Number (The_Left),
            The_Expected => The_Expected,
            The_Test     => The_Test);
         Dispose (The_Left);
      end Test_And_Case;

   begin
      -- test constant number false and constant number false
      Test_And_Case
        (Array_Left   => CONSTANT_FALSE,
         Array_Right  => CONSTANT_FALSE,
         The_Expected => "0",
         The_Test     => "(21) and.");

      -- test constant number true and constant number false
      Test_And_Case
        (Array_Left   => CONSTANT_TRUE,
         Array_Right  => CONSTANT_FALSE,
         The_Expected => "0",
         The_Test     => "(22) and.");

      -- test constant number false and constant number true
      Test_And_Case
        (Array_Left   => CONSTANT_FALSE,
         Array_Right  => CONSTANT_TRUE,
         The_Expected => "0",
         The_Test     => "(23) and.");

      -- test constant number true and constant number true
      Test_And_Case
        (Array_Left   => CONSTANT_TRUE,
         Array_Right  => CONSTANT_TRUE,
         The_Expected => "1",
         The_Test     => "(24) and.");

      -- test simple number (a0) and simple number (a0)
      Test_And_Case
        (Array_Left   => SIMPLE_NUMBER,
         Array_Right  => SIMPLE_NUMBER,
         The_Expected => "01",
         The_Test     => "(25) and.");

      -- test two variables number (a0) and two variables number (b0)
      Test_And_Case
        (Array_Left   => TWO_VARIABLES_NUMBER_A0,
         Array_Right  => TWO_VARIABLES_NUMBER_B0,
         The_Expected => "0001",
         The_Test     => "(26) and.");

      -- test maximum test number, a0b0c0...
      Test_And_Case
        (Array_Left   => MAXIMUM_TEST_NUMBER,
         Array_Right  => MAXIMUM_TEST_NUMBER,
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(27) not.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(28) Incorrect number allocations.");
   end Test_And_Op;

   ----------------
   -- Test_Or_Op --
   ----------------

   procedure Test_Or_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      procedure Test_Or_Case
        (Array_Left   : Number_Array_Type;
         Array_Right  : Number_Array_Type;
         The_Expected : String;
         The_Test     : String)
      is
         The_Left  : Number_Type;
         The_Right : Number_Type;
      begin
         Create_Test_Number (The_Left, Array_Left);
         Create_Test_Number (The_Right, Array_Right);
         Or_Op (The_Left, The_Right);
         Dispose (The_Right);
         Assert
           (The_Result   => Image_Of_Test_Number (The_Left),
            The_Expected => The_Expected,
            The_Test     => The_Test);
         Dispose (The_Left);
      end Test_Or_Case;

   begin
      -- test constant number false and constant number false
      Test_Or_Case
        (Array_Left   => CONSTANT_FALSE,
         Array_Right  => CONSTANT_FALSE,
         The_Expected => "0",
         The_Test     => "(29) or.");

      -- test constant number true and constant number false
      Test_Or_Case
        (Array_Left   => CONSTANT_TRUE,
         Array_Right  => CONSTANT_FALSE,
         The_Expected => "1",
         The_Test     => "(30) or.");

      -- test constant false and constant true
      Test_Or_Case
        (Array_Left   => CONSTANT_FALSE,
         Array_Right  => CONSTANT_TRUE,
         The_Expected => "1",
         The_Test     => "(31) or.");

      -- test constant number true and constant number true
      Test_Or_Case
        (Array_Left   => CONSTANT_TRUE,
         Array_Right  => CONSTANT_TRUE,
         The_Expected => "1",
         The_Test     => "(32) or.");

      -- test simple number (a0) and simple number (a0)
      Test_Or_Case
        (Array_Left   => SIMPLE_NUMBER,
         Array_Right  => SIMPLE_NUMBER,
         The_Expected => "01",
         The_Test     => "(33) or.");

      -- test two variables number (a0) and two variables number (b0)
      Test_Or_Case
        (Array_Left   => TWO_VARIABLES_NUMBER_A0,
         Array_Right  => TWO_VARIABLES_NUMBER_B0,
         The_Expected => "0111",
         The_Test     => "(34) or.");

      -- test maximum test number, a0b0c0...
      Test_Or_Case
        (Array_Left   => MAXIMUM_TEST_NUMBER,
         Array_Right  => MAXIMUM_TEST_NUMBER,
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(35) not.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(36) Incorrect number allocations.");
   end Test_Or_Op;

   -----------------
   -- Test_Xor_Op --
   -----------------

   procedure Test_Xor_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      procedure Test_Xor_Case
        (Array_Left   : Number_Array_Type;
         Array_Right  : Number_Array_Type;
         The_Expected : String;
         The_Test     : String)
      is
         The_Left  : Number_Type;
         The_Right : Number_Type;
      begin
         Create_Test_Number (The_Left, Array_Left);
         Create_Test_Number (The_Right, Array_Right);
         Xor_Op (The_Left, The_Right);
         Dispose (The_Right);
         Assert
           (The_Result   => Image_Of_Test_Number (The_Left),
            The_Expected => The_Expected,
            The_Test     => The_Test);
         Dispose (The_Left);
      end Test_Xor_Case;

   begin
      -- test constant number false and constant number false
      Test_Xor_Case
        (Array_Left   => CONSTANT_FALSE,
         Array_Right  => CONSTANT_FALSE,
         The_Expected => "0",
         The_Test     => "(37) xor.");

      -- test constant number true and constant number false
      Test_Xor_Case
        (Array_Left   => CONSTANT_TRUE,
         Array_Right  => CONSTANT_FALSE,
         The_Expected => "1",
         The_Test     => "(38) xor.");

      -- test constant number false and constant number true
      Test_Xor_Case
        (Array_Left   => CONSTANT_FALSE,
         Array_Right  => CONSTANT_TRUE,
         The_Expected => "1",
         The_Test     => "(39) xor.");

      -- test constant number true and constant number true
      Test_Xor_Case
        (Array_Left   => CONSTANT_TRUE,
         Array_Right  => CONSTANT_TRUE,
         The_Expected => "0",
         The_Test     => "(40) xor.");

      -- test simple number (a0) and simple number (a0)
      Test_Xor_Case
        (Array_Left   => SIMPLE_NUMBER,
         Array_Right  => SIMPLE_NUMBER,
         The_Expected => "00",
         The_Test     => "(41) xor.");

      -- test two variables number (a0) and two variables number (b0)
      Test_Xor_Case
        (Array_Left   => TWO_VARIABLES_NUMBER_A0,
         Array_Right  => TWO_VARIABLES_NUMBER_B0,
         The_Expected => "0110",
         The_Test     => "(42) xor.");

      -- test maximum test number, a0b0c0...
      Test_Xor_Case
        (Array_Left   => MAXIMUM_TEST_NUMBER,
         Array_Right  => MAXIMUM_TEST_NUMBER,
         The_Expected => (1 .. MAXIMUM_TEST_TERM + 1 => '0'),
         The_Test     => "(43) not.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(44) Incorrect number allocations.");
   end Test_Xor_Op;

   --------------------
   -- Test_Normalize --
   --------------------

   procedure Test_Normalize
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Right : Number_Type;
   begin
      -- test constant number true normalizing to a one variable number (a0A0)
      Create_Test_Number (The_Right, CONSTANT_TRUE);
      Normalize (The_Right, (1, (True, others => False)));
      Assert
        (The_Result   => Image_Of_Test_Number (The_Right),
         The_Expected => "11",
         The_Test     => "(45) normailize.");
      Dispose (The_Right);

      -- test constant number false normalizing to a one variable number (a0A0)
      Create_Test_Number (The_Right, CONSTANT_FALSE);
      Normalize (The_Right, (1, (True, others => False)));
      Assert
        (The_Result   => Image_Of_Test_Number (The_Right),
         The_Expected => "00",
         The_Test     => "(46) normailize.");
      Dispose (The_Right);

      -- test simple number (a0) normalizing to a two variable number (a0)
      Create_Test_Number (The_Right, SIMPLE_NUMBER);
      Normalize (The_Right, (2, (True, False, others => False)));
      Assert
        (The_Result   => Image_Of_Test_Number (The_Right),
         The_Expected => "0011",
         The_Test     => "(47) normailize.");
      Dispose (The_Right);

      -- test two variable number (a0) normalizing to a
      --  three variable number (a0)
      Create_Test_Number (The_Right, TWO_VARIABLES_NUMBER_A0);
      Normalize (The_Right, (3, (False, False, True, others => False)));
      Assert
        (The_Result   => Image_Of_Test_Number (The_Right),
         The_Expected => "00110011",
         The_Test     => "(48) normailize.");
      Dispose (The_Right);

      -- test two variable number (b0) normalizing to a three variable number
      -- (b0)
      Create_Test_Number (The_Right, TWO_VARIABLES_NUMBER_B0);
      Normalize (The_Right, (3, (True, True, False, others => False)));
      Assert
        (The_Result   => Image_Of_Test_Number (The_Right),
         The_Expected => "00001111",
         The_Test     => "(49) normailize.");
      Dispose (The_Right);

      -- test maximum test number (a0b0c0...) normalizing to a +1 variable
      -- number (a0b0c0...N0N0+1)
      Create_Test_Number (The_Right, MAXIMUM_TEST_NUMBER);
      Normalize
        (The_Right,
         (MAXIMUM_TEST_VARIABLES + 1,
          (0 .. MAXIMUM_TEST_VARIABLES - 1 => False,
           MAXIMUM_TEST_VARIABLES          => True,
           others                          => False)));
      Assert
        (The_Result   => Image_Of_Test_Number (The_Right),
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM                             => '0',
            MAXIMUM_TEST_TERM + 1                              => '1',
            MAXIMUM_TEST_TERM + 2 .. 2 * MAXIMUM_TEST_TERM + 1 => '0',
            2 * MAXIMUM_TEST_TERM + 2 => '1'), --"00010001",
         The_Test => "(50) normailize.");
      Dispose (The_Right);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(51) Incorrect number allocations.");
   end Test_Normalize;

   ------------------
   -- Test_Include --
   ------------------

   procedure Test_Include
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Fpt    : Term_Type;
      The_Number : Number_Type;
   begin
      -- test constant number false include 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Create (The_Fpt, 0);
      Include (The_Fpt, The_Number, True);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "1",
         The_Test     => "(52) include.");

      Dispose (The_Number);

      -- test constant number true exclude 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Create (The_Fpt, 0);
      Include (The_Fpt, The_Number, False);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "0",
         The_Test     => "(53) include.");

      Dispose (The_Number);

      -- test simple number (a0) include 1st fpt (0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Create (The_Fpt, 1, 0);
      Include (The_Fpt, The_Number, True);

      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "11",
         The_Test     => "(54) include.");

      Dispose (The_Number);

      -- test simple number (a0) exclude 2nd fpt (1)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Create (The_Fpt, 1, 1);
      Include (The_Fpt, The_Number, False);

      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "00",
         The_Test     => "(55) include.");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...) exclude last fpt
      -- (MAXIMUM_TEST_TERM)
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Create (The_Fpt, MAXIMUM_TEST_VARIABLES, MAXIMUM_TEST_TERM);
      Include (The_Fpt, The_Number, False);

      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => (1 .. MAXIMUM_TEST_TERM + 1 => '0'),
         The_Test     => "(56) include.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(57) Incorrect number allocations.");
   end Test_Include;

   -------------------------------
   -- Test_Is_Included_With_FPT --
   -------------------------------

   procedure Test_Is_Included_With_FPT
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Fpt    : Term_Type;
      The_Number : Number_Type;
   begin
      -- test constant number false excludes 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Create (The_Fpt, 0);
      Assert
        (The_Result   => Is_Included (The_Fpt, The_Number),
         The_Expected => False,
         The_Test     => "(58) include.");

      Dispose (The_Number);

      -- test constant number true includes 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Create (The_Fpt, 0);
      Assert
        (The_Result   => Is_Included (The_Fpt, The_Number),
         The_Expected => True,
         The_Test     => "(59) include.");

      Dispose (The_Number);

      -- test simple number (a0) excludes 1st fpt (0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Create (The_Fpt, 1, 0);
      Assert
        (The_Result   => Is_Included (The_Fpt, The_Number),
         The_Expected => False,
         The_Test     =>
           "(60) is included. " &
           " " &
           Image_Of (The_Fpt) &
           " " &
           Image_Of_Test_Number (The_Number));

      Dispose (The_Number);

      -- test simple number (a0) includes 2nd fpt (1)
      Create (The_Fpt, 1, 1);
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Included (The_Fpt, The_Number),
         The_Expected => True,
         The_Test     =>
           "(61) is included. " &
           " " &
           Image_Of (The_Fpt) &
           " " &
           Image_Of_Test_Number (The_Number));

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...) includes last fpt
      -- (MAXIMUM_TEST_TERM)
      Create (The_Fpt, MAXIMUM_TEST_VARIABLES, MAXIMUM_TEST_TERM);
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Is_Included (The_Fpt, The_Number),
         The_Expected => True,
         The_Test     =>
           "(62) is included. " &
           " " &
           Image_Of (The_Fpt) &
           " " &
           Image_Of_Test_Number (The_Number));

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(63) Incorrect number allocations.");
   end Test_Is_Included_With_FPT;

   ------------------------
   -- Test_Include_Index --
   ------------------------

   procedure Test_Include_Index
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant number false include 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Include (0, The_Number, True);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "1",
         The_Test     => "(64) include.");

      Dispose (The_Number);

      -- test constant number true exclude 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Include (0, The_Number, False);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "0",
         The_Test     => "(65) include.");

      Dispose (The_Number);

      -- test simple number (a0) include 1st fpt (0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Include (0, The_Number, True);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "11",
         The_Test     => "(66) included with fpt. ");

      Dispose (The_Number);

      -- test simple number (a0) exclude 2nd fpt (1)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Include (1, The_Number, False);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => "00",
         The_Test     => "(67) included with fpt. ");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...) exclude 2nd fpt (1)
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Include (MAXIMUM_TEST_TERM, The_Number, False);
      Assert
        (The_Result   => Image_Of_Test_Number (The_Number),
         The_Expected => (1 .. MAXIMUM_TEST_TERM + 1 => '0'),
         The_Test     => "(68) included with fpt. ");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(69) Incorrect number allocations.");
   end Test_Include_Index;

   ---------------------------------
   -- Test_Is_Included_With_Index --
   ---------------------------------

   procedure Test_Is_Included_With_Index
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant number false excludes 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => Is_Included (0, The_Number),
         The_Expected => False,
         The_Test     => "(70) include.");

      Dispose (The_Number);

      -- test constant number true includes 1st fpt (0)
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Included (0, The_Number),
         The_Expected => True,
         The_Test     => "(71) include.");

      Dispose (The_Number);

      -- test simple number (a0) includes 1st fpt (0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Included (0, The_Number),
         The_Expected => False,
         The_Test     => "(72) is included with index.");

      Dispose (The_Number);

      -- test simple number (a0) includes 2nd fpt (1)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Included (1, The_Number),
         The_Expected => True,
         The_Test     => "(73) is included with index.");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0) includes last fpt (MAXIMUM_TEST_TERM)
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Is_Included (MAXIMUM_TEST_TERM, The_Number),
         The_Expected => True,
         The_Test     => "(74) is included with index.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(75) Incorrect number allocations.");
   end Test_Is_Included_With_Index;

   ---------------------------------
   -- Test_Is_Constant_Of_Boolean --
   ---------------------------------

   procedure Test_Is_Constant_Of_Boolean
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant true number is constant true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Constant (The_Number, True),
         The_Expected => True,
         The_Test     => "(76) is constant of boolean.");

      Dispose (The_Number);

      -- test constant true number is not constant false
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Constant (The_Number, False),
         The_Expected => False,
         The_Test     => "(77) is constant of boolean.");

      Dispose (The_Number);

      -- test simple number (a0) is not constant true
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Number, True),
         The_Expected => False,
         The_Test     => "(78) is constant of boolean.");

      Dispose (The_Number);

      -- test simple number (a0) is not constant false
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Number, False),
         The_Expected => False,
         The_Test     => "(79) is constant of boolean.");

      Dispose (The_Number);

      -- test arbitrary true constant number is constant true
      Create_Test_Number (The_Number, (0 .. 3 => True));
      Assert
        (The_Result   => Is_Constant (The_Number, True),
         The_Expected => True,
         The_Test     => "(80) is constant of boolean.");

      Dispose (The_Number);

      -- test arbitrary false constant number is constant false
      Create_Test_Number (The_Number, (0 .. 3 => False));
      Assert
        (The_Result   => Is_Constant (The_Number, False),
         The_Expected => True,
         The_Test     => "(81) is constant of boolean.");

      Dispose (The_Number);

      -- test true maximum test constant number is constant true
      Create_Test_Number (The_Number, (0 .. MAXIMUM_TEST_TERM => True));
      Assert
        (The_Result   => Is_Constant (The_Number, True),
         The_Expected => True,
         The_Test     => "(82) is constant of boolean.");

      Dispose (The_Number);

      -- test false maximum test constant number is constant false
      Create_Test_Number (The_Number, (0 .. MAXIMUM_TEST_TERM => False));
      Assert
        (The_Result   => Is_Constant (The_Number, False),
         The_Expected => True,
         The_Test     => "(83) is constant of boolean.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(84) Incorrect number allocations.");
   end Test_Is_Constant_Of_Boolean;

   ----------------------
   -- Test_Is_Constant --
   ----------------------

   procedure Test_Is_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(85) is constant.");

      Dispose (The_Number);

      -- test constant false
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(86) is constant.");

      Dispose (The_Number);

      -- test simple number (a0) not constant
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => False,
         The_Test     => "(87) is constant.");

      Dispose (The_Number);

      -- test arbitrary true constant number is constant
      Create_Test_Number (The_Number, (0 .. 3 => True));
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(88) is constant.");

      Dispose (The_Number);

      -- test arbitrary false constant number is constant
      Create_Test_Number (The_Number, (0 .. 3 => False));
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(89) is constant.");

      Dispose (The_Number);

      -- test true maximum test constant number is constant
      Create_Test_Number (The_Number, (0 .. MAXIMUM_TEST_TERM => True));
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(90) is constant.");

      Dispose (The_Number);

      -- test false maximum test constant number is constant
      Create_Test_Number (The_Number, (0 .. MAXIMUM_TEST_TERM => False));
      Assert
        (The_Result   => Is_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(91) is constant.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(92) Incorrect number allocations.");
   end Test_Is_Constant;

   ----------------------
   -- Test_To_Constant --
   ----------------------

   procedure Test_To_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => To_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(93) to constant.");

      Dispose (The_Number);

      -- test constant false
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => To_Constant (The_Number),
         The_Expected => False,
         The_Test     => "(94) to constant.");

      Dispose (The_Number);

      -- test arbitrary true constant number
      Create_Test_Number (The_Number, (0 .. 3 => True));
      Assert
        (The_Result   => To_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(95) to constant.");

      Dispose (The_Number);

      -- test arbitrary false constant number
      Create_Test_Number (The_Number, (0 .. 3 => False));
      Assert
        (The_Result   => To_Constant (The_Number),
         The_Expected => False,
         The_Test     => "(96) to constant.");

      Dispose (The_Number);

      -- test true maximum test constant number
      Create_Test_Number (The_Number, (0 .. MAXIMUM_TEST_TERM => True));
      Assert
        (The_Result   => To_Constant (The_Number),
         The_Expected => True,
         The_Test     => "(97) to constant.");

      Dispose (The_Number);

      -- test false maximum test constant number
      Create_Test_Number (The_Number, (0 .. MAXIMUM_TEST_TERM => False));
      Assert
        (The_Result   => To_Constant (The_Number),
         The_Expected => False,
         The_Test     => "(98) to constant.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(99) Incorrect number allocations.");
   end Test_To_Constant;

   --------------------
   -- Test_Length_Of --
   --------------------

   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant number true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Length_Of (The_Number),
         The_Expected => 1,
         The_Test     => "(100) length of.");

      Dispose (The_Number);

      -- test constant number false
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => Length_Of (The_Number),
         The_Expected => 1,
         The_Test     => "(101) length of.");

      Dispose (The_Number);

      -- test simple number (a0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Length_Of (The_Number),
         The_Expected => 2,
         The_Test     => "(102) length of.");

      Dispose (The_Number);

      -- test two variable number (a0)
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_A0);
      Assert
        (The_Result   => Length_Of (The_Number),
         The_Expected => 4,
         The_Test     => "(103) length of.");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...)
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Length_Of (The_Number),
         The_Expected => MAXIMUM_TEST_TERM + 1,
         The_Test     => "(104) length of.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(105) Incorrect number allocations.");
   end Test_Length_Of;

   ---------------------
   -- Test_Boolean_Of --
   ---------------------

   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      The_Number : Number_Type;
   begin
      -- test constant number true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected => "1",
         The_Test     => "(106) boolean of.");

      Dispose (The_Number);

      -- test constant number false
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected => "0",
         The_Test     => "(107) boolean of.");

      Dispose (The_Number);

      -- test simple number (a0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected => "01",
         The_Test     => "(108) boolean of.");

      Dispose (The_Number);

      -- test two variable number (a0)
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_A0);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected => "0011",
         The_Test     => "(109) boolean of.");

      Dispose (The_Number);

      -- test two variable number (b0)
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_B0);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected => "0101",
         The_Test     => "(110) boolean of.");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...)
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Boolean_Of (The_Number),
         The_Expected =>
           (1 .. MAXIMUM_TEST_TERM => '0', MAXIMUM_TEST_TERM + 1 => '1'),
         The_Test => "(111) boolean of.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(112) Incorrect number allocations.");
   end Test_Boolean_Of;

   -------------------
   -- Test_Image_Of --
   -------------------

   procedure Test_Image_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      IMAGE_OF_TERMS : constant String :=
        IMAGE_OF_ALL_TERMS (1 .. 2 * MAXIMUM_TEST_VARIABLES);
      --        "a0b0c0d0e0f0g0h0i0j0k0l0m0n0o0p0";

      The_Number : Number_Type;
   begin
      -- test constant number true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Image_Of (The_Number, 1),
         The_Expected => " 1",
         The_Test     => "(113) image of.");

      Dispose (The_Number);

      -- test constant number false
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => Image_Of (The_Number, 1),
         The_Expected => " 0",
         The_Test     => "(114) image of.");

      Dispose (The_Number);

      -- test simple number (a0)
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Number, 1),
         The_Expected => " a0",
         The_Test     => "(115) image of.");

      Dispose (The_Number);

      -- test two variable number (a0)
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_A0);
      Assert
        (The_Result   => Image_Of (The_Number, 2),
         The_Expected => " A0b0 a0b0",
         The_Test     => "(116) image of.");

      Dispose (The_Number);

      -- test two variable number (b0)
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_B0);
      Assert
        (The_Result   => Image_Of (The_Number, 2),
         The_Expected => " a0B0 a0b0",
         The_Test     => "(117) image of.");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...)
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Number, MAXIMUM_TEST_VARIABLES),
         The_Expected => " " & IMAGE_OF_TERMS,
         The_Test     => "(118) image of.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(119) Incorrect number allocations.");
   end Test_Image_Of;

   ----------------------------------
   -- Test_Image_Of_With_Variables --
   ----------------------------------

   procedure Test_Image_Of_With_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      IMAGE_OF_TERMS : constant String :=
        IMAGE_OF_ALL_TERMS (3 .. 4) &
        IMAGE_OF_ALL_TERMS (7 .. 2 * MAXIMUM_TEST_VARIABLES + 4);
      --        "a0b0c0d0e0f0g0h0i0j0k0l0m0n0o0p0";

      The_Number : Number_Type;

      function Maximum_Test_Array_Of_Variables return Variable_Array_Type is
         The_Array : Variable_Array_Type (1 .. MAXIMUM_TEST_VARIABLES) :=
           (1 .. MAXIMUM_TEST_VARIABLES => 0);
      begin
         The_Array (1) := 1;
         The_Array (2) := 3;
         for I in 3 .. MAXIMUM_TEST_VARIABLES loop
            The_Array (I) := I + 1;
         end loop;
         return The_Array;
      end Maximum_Test_Array_Of_Variables;

   begin
      -- test constant number true
      Create_Test_Number (The_Number, CONSTANT_TRUE);
      Assert
        (The_Result   => Image_Of (The_Number, (1 => 0)),
         The_Expected => " 1",
         The_Test     => "(120) image of with variable.");

      Dispose (The_Number);

      -- test constant number false
      Create_Test_Number (The_Number, CONSTANT_FALSE);
      Assert
        (The_Result   => Image_Of (The_Number, (1 => 0)),
         The_Expected => " 0",
         The_Test     => "(121) image of with variable.");

      Dispose (The_Number);

      -- test simple number (a0) starting at 1st varaiable
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Number, (1 => 0)),
         The_Expected => " a0",
         The_Test     => "(122) image of with variable.");

      Dispose (The_Number);

      -- test simple number (a0) starting at 3rd varaiable
      Create_Test_Number (The_Number, SIMPLE_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Number, (1 => 2)),
         The_Expected => " c0",
         The_Test     => "(123) image of with variable.");

      Dispose (The_Number);

      -- test two variable number (a0) with 1st and 2nd varaiable
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_A0);
      Assert
        (The_Result   => Image_Of (The_Number, (0, 1)),
         The_Expected => " A0b0 a0b0",
         The_Test     => "(124) image of with variable.");

      Dispose (The_Number);

      -- test two variable number (b0) with 1st and 3rd varaiable
      Create_Test_Number (The_Number, TWO_VARIABLES_NUMBER_B0);
      Assert
        (The_Result   => Image_Of (The_Number, (0, 2)),
         The_Expected => " a0C0 a0c0",
         The_Test     => "(125) image of with variable.");

      Dispose (The_Number);

      -- test maximum test number (a0b0c0...) with 1st and 3rd varaiable
      Create_Test_Number (The_Number, MAXIMUM_TEST_NUMBER);
      Assert
        (The_Result   => Image_Of (The_Number, Maximum_Test_Array_Of_Variables),
         The_Expected => " " & IMAGE_OF_TERMS,
         The_Test     => "(126) image of with variable.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(127) Incorrect number allocations.");
   end Test_Image_Of_With_Variables;

   ---------------
   -- Test_Pool --
   ---------------

   procedure Test_Pool (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      The_Number : Number_Type;
   begin
      Create (The_Number, True);
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations + 1,
         The_Test     => "(128) Test pool.");

      Dispose (The_Number);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(129) Incorrect number allocations.");
   end Test_Pool;

end Number_Package.Number_Test;

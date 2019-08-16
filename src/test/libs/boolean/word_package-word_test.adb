-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Pool_Package;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Package; use Test_Package;
--

package body Word_Package.Word_Test is

   -- Unmaked allocations.

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

   -- Create test word for equations.

   procedure Create_Test_Word
     (The_Word  : in out Word_Type;
      The_Array :        Word_Array_Type)
   is
   begin
      The_Word := new Word_Array_Type (0 .. The_Array'Length - 1);

      for I in 0 .. The_Word'Length - 1 loop
         The_Word (I) := The_Array (I);
      end loop;
   end Create_Test_Word;

   -- return image of boolean word not using word pakage operations

   function Image_Of_Test_Word (The_Word : Word_Type) return String is
      The_String : Unbounded_String := Null_Unbounded_String;
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop
         if I = 0 then
            The_String := The_String & Boolean_Of (The_Word (I));
         else
            The_String :=
              The_String & " " & Boolean_Of (The_Word (I));
         end if;
      end loop;

      return To_String (The_String);
   end Image_Of_Test_Word;

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("word_package.word_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Create_Constant_With_Boolean_Array'Access, "Test_Create_Constant_With_Boolean_Array!");
      Register_Routine(The_Test, Test_Create_Constant_With_Size'Access, "Test_Create_Constant_With_Size!");
      Register_Routine(The_Test, Test_Create_Variable'Access, "Test_Create_Variable!");
      Register_Routine(The_Test, Test_Dispose'Access, "Test_Dispose!");
      Register_Routine(The_Test, Test_Copy'Access, "Test_Copy!");
      Register_Routine(The_Test, Test_Append_With_Equation'Access, "Test_Append_With_Equation!");
      Register_Routine(The_Test, Test_Append_With_Word'Access, "Test_Append_With_Word!");
      Register_Routine(The_Test, Test_Fill'Access, "Test_Fill!");
      Register_Routine(The_Test, Test_Shift_Left'Access, "Test_Shift_Left!");
      Register_Routine(The_Test, Test_Shift_Right'Access, "Test_Shift_Right!");
      Register_Routine(The_Test, Test_Not_Op'Access, "Test_Not_Op!");
      Register_Routine(The_Test, Test_And_Op'Access, "Test_And_Op!");
      Register_Routine(The_Test, Test_Or_Op'Access, "Test_Or_Op!");
      Register_Routine(The_Test, Test_Xor_Op'Access, "Test_Xor_Op!");
      Register_Routine(The_Test, Test_Not_Equal'Access, "Test_Not_Equal!");
      Register_Routine(The_Test, Test_Unsigned_Less_Than'Access, "Test_Unsigned_Less_Than!");
      Register_Routine(The_Test, Test_Signed_Less_Than'Access, "Test_Signed_Less_Than!");
      Register_Routine(The_Test, Test_Unsigned_Greater_Than'Access, "Test_Unsigned_Greater_Than!");
      Register_Routine(The_Test, Test_Signed_Greater_Than'Access, "Test_Signed_Greater_Than!");
      Register_Routine(The_Test, Test_Negate'Access, "Test_Negate!");
      Register_Routine(The_Test, Test_Add'Access, "Test_Add!");
      Register_Routine(The_Test, Test_Subtract'Access, "Test_Subtract!");
      Register_Routine(The_Test, Test_Multiply'Access, "Test_Multiply!");
      Register_Routine(The_Test, Test_Unsigned_Divide'Access, "Test_Unsigned_Divide!");
      Register_Routine(The_Test, Test_Signed_Divide'Access, "Test_Signed_Divide!");
      Register_Routine(The_Test, Test_Unsigned_Remainder'Access, "Test_Unsigned_Remainder!");
      Register_Routine(The_Test, Test_Signed_Remainder'Access, "Test_Signed_Remainder!");
      Register_Routine(The_Test, Test_Assign_Element'Access, "Test_Assign_Element!");
      Register_Routine(The_Test, Test_Access_Element'Access, "Test_Access_Element!");
      Register_Routine(The_Test, Test_If_Else'Access, "Test_If_Else!");
      Register_Routine(The_Test, Test_Convert'Access, "Test_Convert!");
      Register_Routine(The_Test, Test_Normalize'Access, "Test_Normalize!");
      Register_Routine(The_Test, Test_Normalize_With_Variables'Access, "Test_Normalize_With_Variables!");
      Register_Routine(The_Test, Test_Is_Constant'Access, "Test_Is_Constant!");
      Register_Routine(The_Test, Test_To_Constant_Modular'Access, "Test_To_Constant_Modular!");
      Register_Routine(The_Test, Test_To_Constant_Boolean_Array'Access, "Test_To_Constant_Boolean_Array!");
      Register_Routine(The_Test, Test_Variables_Of'Access, "Test_Variables_Of!");
      Register_Routine(The_Test, Test_Length_Of'Access, "Test_Length_Of!");
      Register_Routine(The_Test, Test_Solve'Access, "Test_Solve!");
      Register_Routine(The_Test, Test_Boolean_Of'Access, "Test_Boolean_Of!");
      Register_Routine(The_Test, Test_Image_Of'Access, "Test_Image_Of!");
      Register_Routine(The_Test, Test_Iterate'Access, "Test_Iterate!");
      Register_Routine(The_Test, Test_Pool'Access, "Test_Pool!");
      Register_Routine(The_Test, Test_And_Op'Access, "Test_And_Op!");
      Register_Routine(The_Test, Test_Or_Op'Access, "Test_Or_Op!");
      Register_Routine(The_Test, Test_Xor_Op'Access, "Test_Xor_Op!");
      Register_Routine(The_Test, Test_Not_Equal'Access, "Test_Not_Equal!");
      Register_Routine(The_Test, Test_Unsigned_Less_Than'Access, "Test_Unsigned_Less_Than!");
      Register_Routine(The_Test, Test_Signed_Less_Than'Access, "Test_Signed_Less_Than!");
      Register_Routine(The_Test, Test_Unsigned_Greater_Than'Access, "Test_Unsigned_Greater_Than!");
      Register_Routine(The_Test, Test_Signed_Greater_Than'Access, "Test_Signed_Greater_Than!");
      Register_Routine(The_Test, Test_Add'Access, "Test_Add!");
      Register_Routine(The_Test, Test_Subtract'Access, "Test_Subtract!");
      Register_Routine(The_Test, Test_Multiply'Access, "Test_Multiply!");
      Register_Routine(The_Test, Test_Unsigned_Divide'Access, "Test_Unsigned_Divide!");
      Register_Routine(The_Test, Test_Signed_Divide'Access, "Test_Signed_Divide!");
      Register_Routine(The_Test, Test_Unsigned_Remainder'Access, "Test_Unsigned_Remainder!");
      Register_Routine(The_Test, Test_Signed_Remainder'Access, "Test_Signed_Remainder!");
   end Register_Tests;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
   begin
      The_Unmarked_Number_Allocations :=
        Pool_Package.Unmarked_Allocations (Number_Package.The_Pool);
      The_Unmarked_Word_Allocations :=
        Pool_Package.Unmarked_Allocations (Word_Package.The_Pool);
   end Set_Up;

   generic
      Constant_Results : String;  -- constant word results
      Variable_Results : String;  -- variable word results
      The_Message      : String;
      with procedure Operation
        (The_Left  : in out Word_Type;
         The_Right : in     Word_Type);
   procedure Test_Binary_Operation;

   procedure Test_Binary_Operation is

      The_Word1 : Word_Type;
      The_Word2 : Word_Type;
      The_Equ1  : Equation_Type;
      The_Equ2  : Equation_Type;
   begin
      -- test constant words (false) and (false)
      Create (The_Equ1, False);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, False);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (1 .. 1),
         The_Test     => "(1) " & The_Message);

      Dispose (The_Word1);

      -- test constant words (true) and (false)
      Create (The_Equ1, True);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, False);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (2 .. 2),
         The_Test     => "(2) " & The_Message);

      Dispose (The_Word1);

      -- test constant words (false) and (true)
      Create (The_Equ1, False);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, True);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (3 .. 3),
         The_Test     => "(3) " & The_Message);

      Dispose (The_Word1);

      -- test constant words (true) and (true)
      Create (The_Equ1, True);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, True);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (4 .. 4),
         The_Test     => "(4) " & The_Message);

      Dispose (The_Word1);

      -- test variable words (b0,a0) and (a0,b0)
      Create (The_Equ1, 0);
      Normalize (The_Equ1, Set_Of ((1 => 1)));
      Create (The_Equ2, 1);
      Normalize (The_Equ2, Set_Of ((1 => 0)));
      The_Word1 := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, 1);
      Normalize (The_Equ1, Set_Of ((1 => 0)));
      Create (The_Equ2, 0);
      Normalize (The_Equ2, Set_Of ((1 => 1)));
      The_Word2 := new Word_Array_Type'(The_Equ1, The_Equ2);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( a0 b0 ) " & Variable_Results (1 .. 4) &
           " f( a0 b0 ) " & Variable_Results (5 .. 8),
         The_Test     => "(5) " & The_Message);

      Dispose (The_Word1);
   end Test_Binary_Operation;

   generic
      Constant_Results        : String;  -- constant word results
      Simple_Variable_Results : String;  -- simple word results
      Variable_Results        : String;  -- variable word results
      The_Message             : String;
      with procedure Operation
        (The_Left  : in out Word_Type;
         The_Right : in     Word_Type);
   procedure Test_Comparison_Operation;

   procedure Test_Comparison_Operation is
      The_Word1 : Word_Type;
      The_Word2 : Word_Type;
      The_Equ1  : Equation_Type;
      The_Equ2  : Equation_Type;
   begin
      -- test constant words (false) and (false)
      Create (The_Equ1, False);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, False);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (1 .. 1),
         The_Test     => "(6) " & The_Message);

      Dispose (The_Word1);

      -- test constant words (true) and (false)
      Create (The_Equ1, True);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, False);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (2 .. 2),
         The_Test     => "(7) " & The_Message);

      Dispose (The_Word1);

      -- test constant words (false) and (true)
      Create (The_Equ1, False);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, True);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (3 .. 3),
         The_Test     => "(8) " & The_Message);

      Dispose (The_Word1);

      -- test constant words (true) and (true)
      Create (The_Equ1, True);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, True);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1);

      Operation (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) " & Constant_Results (4 .. 4),
         The_Test     => "(9) " & The_Message);

      Dispose (The_Word1);

      -- test simple words (a0) and (a0)
      Create (The_Equ1, 0);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ2, 0);
      The_Word2 := new Word_Array_Type'(0 => The_Equ2);

      Not_Equal (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( a0 ) " & Simple_Variable_Results (1 .. 2),
         The_Test     => "(10) " & The_Message);

      Dispose (The_Word1);

      -- test simple words (a0) and (b0)
      Create (The_Equ1, 0);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ2, 1);
      The_Word2 := new Word_Array_Type'(0 => The_Equ2);

      Not_Equal (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( a0 b0 ) " & Simple_Variable_Results (3 .. 6),
         The_Test     => "(11) " & The_Message);

      Dispose (The_Word1);

      -- test variable words (b0,a0) and (b0,a0)
      Create (The_Equ1, 0);
      Normalize (The_Equ1, Set_Of ((1 => 1)));
      Create (The_Equ2, 1);
      Normalize (The_Equ2, Set_Of ((1 => 0)));
      The_Word1 := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, 0);
      Normalize (The_Equ1, Set_Of ((1 => 1)));
      Create (The_Equ2, 1);
      Normalize (The_Equ2, Set_Of ((1 => 0)));
      The_Word2 := new Word_Array_Type'(The_Equ1, The_Equ2);

      Not_Equal (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( a0 b0 ) " & Variable_Results (1 .. 4),
         The_Test     => "(12) " & The_Message);

      Dispose (The_Word1);

      -- test variable words (b0,a0) and (a0,b0)
      Create (The_Equ1, 0);
      Normalize (The_Equ1, Set_Of ((1 => 1)));
      Create (The_Equ2, 1);
      Normalize (The_Equ2, Set_Of ((1 => 0)));
      The_Word1 := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, 1);
      Normalize (The_Equ1, Set_Of ((1 => 0)));
      Create (The_Equ2, 0);
      Normalize (The_Equ2, Set_Of ((1 => 1)));
      The_Word2 := new Word_Array_Type'(The_Equ1, The_Equ2);

      Not_Equal (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( a0 b0 ) " & Variable_Results (5 .. 8),
         The_Test     => "(13) " & The_Message);

      Dispose (The_Word1);

   end Test_Comparison_Operation;

   ---------------------------------------------
   -- Test_Create_Constant_With_Boolean_Array --
   ---------------------------------------------

   procedure Test_Create_Constant_With_Boolean_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
   begin
      -- test boolean array false
      Create_Constant (The_Word, Boolean_Array_Type'(0 => False));
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0",
         The_Test     => "(14) create constant with boolean array.");

      Dispose (The_Word);

      -- test boolean array true
      Create_Constant (The_Word, Boolean_Array_Type'(0 => True));
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 1",
         The_Test     => "(15) create constant with boolean array.");

      Dispose (The_Word);

      -- test boolean array false and true
      Create_Constant (The_Word, Boolean_Array_Type'(False, True));
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0 f( ) 1",
         The_Test     => "(16) create constant with boolean array.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(17) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(18) Incorrect number allocations.");
   end Test_Create_Constant_With_Boolean_Array;

   ------------------------------------
   -- Test_Create_Constant_With_Size --
   ------------------------------------

   procedure Test_Create_Constant_With_Size
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
   begin
      -- test size 1 and constant 0
      Create_Constant (The_Word, 1, 0);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0",
         The_Test     => "(19) create constant with size.");

      Dispose (The_Word);

      -- test size 1 and constant 1
      Create_Constant (The_Word, 1, 1);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 1",
         The_Test     => "(20) create constant with size.");

      Dispose (The_Word);

      -- test size 2 and constant 2
      Create_Constant (The_Word, 2, 2);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0 f( ) 1",
         The_Test     => "(21) create constant with size.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(22) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(23) Incorrect number allocations.");
   end Test_Create_Constant_With_Size;

   --------------------------
   -- Test_Create_Variable --
   --------------------------

   procedure Test_Create_Variable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
   begin
      -- test size 1 and equation (a0)
      Create_Variable (The_Word, 1, 0);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01",
         The_Test     => "(24) create variable.");

      Dispose (The_Word);

      -- test size 2 and equation (a0) (b0)
      Create_Variable (The_Word, 2, 0);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01 f( b0 ) 01",
         The_Test     => "(25) create variable.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(26) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(27) Incorrect number allocations.");
   end Test_Create_Variable;

   ------------------
   -- Test_Dispose --
   ------------------

   procedure Test_Dispose
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
   begin
      -- test constant word (0)
      Create_Constant (The_Word, 1, 0);
      Dispose (The_Word);

      -- test simple word (a0)
      Create_Variable (The_Word, 1, 0);
      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(28) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(29) Incorrect number allocations.");
   end Test_Dispose;

   ---------------
   -- Test_Copy --
   ---------------

   procedure Test_Copy (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is

      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      To_Word  : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Copy (The_Word, To_Word);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 1",
         The_Test     => "(30) copy.");

      Dispose (To_Word);
      Dispose (The_Word);

      -- test simple word (a0)
      Create (The_Equ1, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Copy (The_Word, To_Word);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01",
         The_Test     => "(31) copy.");

      Dispose (To_Word);
      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Copy (The_Word, To_Word);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01 f( b0 ) 01",
         The_Test     => "(32) copy.");

      Dispose (To_Word);
      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(33) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(34) Incorrect number allocations.");
   end Test_Copy;

   -------------------------------
   -- Test_Append_With_Equation --
   -------------------------------

   procedure Test_Append_With_Equation
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type := EMPTY_WORD;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test append word (a0) to empty word
      Create (The_Equ1, 0);
      Append (The_Word, The_Equ1);

      Dispose (The_Equ1);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01",
         The_Test     => "(35) append with equation.");

      -- test append equation (b0) to simple word (a0)
      Create (The_Equ2, 1);
      Append (The_Word, The_Equ2);
      Dispose (The_Equ2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01 f( b0 ) 01",
         The_Test     => "(36) append with equation.");

      Dispose (The_Word);

      -- test append equation (a0) to variable word (true,false)
      Create (The_Equ1, True);
      Create (The_Equ2, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Create (The_Equ1, 0);
      Append (The_Word, The_Equ1);
      Dispose (The_Equ1);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 1 f( ) 0 f( a0 ) 01",
         The_Test     => "(37) append with equation.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(38) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(39) Incorrect number allocations.");
   end Test_Append_With_Equation;

   ---------------------------
   -- Test_Append_With_Word --
   ---------------------------

   procedure Test_Append_With_Word
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word1 : Word_Type := EMPTY_WORD;
      The_Word2 : Word_Type;
      The_Equ1  : Equation_Type;
      The_Equ2  : Equation_Type;
   begin
      -- test append word (false) to null word
      Create (The_Equ1, False);
      Create (The_Equ2, 0);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Append (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) 0 f( a0 ) 01",
         The_Test     => "(40) append with word.");

      Dispose (The_Word1);

      -- test append variable word (true,a0) to variable word (false,b0)
      Create (The_Equ1, True);
      Create (The_Equ2, 0);
      The_Word1 := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Create (The_Equ1, False);
      Create (The_Equ2, 1);
      The_Word2 := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Append (The_Word1, The_Word2);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) 1 f( a0 ) 01 f( ) 0 f( b0 ) 01",
         The_Test     => "(41) append with word.");

      Dispose (The_Word1);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(42) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(43) Incorrect number allocations.");
   end Test_Append_With_Word;

   ---------------
   -- Test_Fill --
   ---------------

   procedure Test_Fill (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type := EMPTY_WORD;
      The_Equ  : Equation_Type;
   begin
      -- test empty word and constant word (true)
      Create (The_Equ, True);
      Fill (The_Word, 1, The_Equ);
      Dispose (The_Equ);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 1",
         The_Test     => "(44) fill.");

      Dispose (The_Word);

      -- test arbitrary word [previous] and constant word (true)
      Create (The_Equ, True);
      Fill (The_Word, 2, The_Equ);
      Dispose (The_Equ);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 1 f( ) 1",
         The_Test     => "(45) fill.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(46) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(47) Incorrect number allocations.");
   end Test_Fill;

   ---------------------
   -- Test_Shift_Left --
   ---------------------

   procedure Test_Shift_Left
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
      The_Equ3 : Equation_Type;
   begin
      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Shift_Left (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0",
         The_Test     => "(48) shift left.");

      Dispose (The_Word);

      -- test variable word (a0,true)
      Create (The_Equ1, 0);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Shift_Left (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0 f( a0 ) 01",
         The_Test     => "(49) shift left.");

      Dispose (The_Word);

      -- test variable word (a0,b0,c0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      Create (The_Equ3, 2);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2, The_Equ3);

      Shift_Left (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0 f( a0 ) 01 f( b0 ) 01",
         The_Test     => "(50) shift left.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(51) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(52) Incorrect number allocations.");
   end Test_Shift_Left;

   ----------------------
   -- Test_Shift_Right --
   ----------------------

   procedure Test_Shift_Right
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
      The_Equ3 : Equation_Type;
   begin
      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Shift_Right (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0",
         The_Test     => "(53) shift right.");

      Dispose (The_Word);

      -- test variable word (true,a0)
      Create (The_Equ1, True);
      Create (The_Equ2, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1, 1 => The_Equ2);

      Shift_Right (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( a0 ) 01 f( ) 0",
         The_Test     => "(54) shift right.");

      Dispose (The_Word);

      -- test variable word (a0,b0,c0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      Create (The_Equ3, 2);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2, The_Equ3);

      Shift_Right (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( b0 ) 01 f( c0 ) 01 f( ) 0",
         The_Test     => "(55) shift right.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(56) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(57) Incorrect number allocations.");
   end Test_Shift_Right;

   -----------------
   -- Test_Not_Op --
   -----------------

   procedure Test_Not_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Not_Op (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( ) 0",
         The_Test     => "(58) not.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Not_Op (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( a0 ) 10 f( b0 ) 10",
         The_Test     => "(59) not.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(60) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(61) Incorrect number allocations.");
   end Test_Not_Op;

   -----------------
   -- Test_And_Op --
   -----------------

   procedure Test_And_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      procedure Test_And_Op is new Test_Binary_Operation
        ("0001",      -- constant word results
         "00010001",  -- variable word results
         "and.",
         And_Op);
   begin

      Test_And_Op;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(62) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(63) Incorrect number allocations.");
   end Test_And_Op;

   ----------------
   -- Test_Or_Op --
   ----------------

   procedure Test_Or_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      procedure Test_Or_Op is new Test_Binary_Operation
        ("0111",      -- constant word results
         "01110111",  -- variable word results
         "or.",
         Or_Op);
   begin

      Test_Or_Op;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(64) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(65) Incorrect number allocations.");
   end Test_Or_Op;

   -----------------
   -- Test_Xor_Op --
   -----------------

   procedure Test_Xor_Op (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      procedure Test_Xor_Op is new Test_Binary_Operation
        ("0110",      -- constant word results
         "01100110",  -- variable word results
         "xor.",
         Xor_Op);
   begin

      Test_Xor_Op;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(66) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(67) Incorrect number allocations.");
   end Test_Xor_Op;

   --------------------
   -- Test_Not_Equal --
   --------------------

   procedure Test_Not_Equal
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      procedure Test_Not_Equal is new Test_Comparison_Operation
        ("0110",      -- constant word results
         "000110",    -- simple word results
         "00000110",  -- variable word results
         "not equal.",
         Not_Equal);

   begin

      Test_Not_Equal;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(68) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(69) Incorrect number allocations.");
   end Test_Not_Equal;

   -----------------------------
   -- Test_Unsigned_Less_Than --
   -----------------------------

   procedure Test_Unsigned_Less_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 < 0 - 0
      -- 1 < 0 - 0
      -- 0 < 1 - 1
      -- 1 < 1 - 0
      -- x < x  - 00
      -- x < y  - 0010

      procedure Test_Unsigned_Less_Than is new Test_Comparison_Operation
        ("0010",      -- constant word results
         "000110",    -- simple word results
         "00000110",  -- variable word results
         "unsigned less than.",
         Unsigned_Less_Than);

   begin

      Test_Unsigned_Less_Than;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(70) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(71) Incorrect number allocations.");
   end Test_Unsigned_Less_Than;

   ---------------------------
   -- Test_Signed_Less_Than --
   ---------------------------

   procedure Test_Signed_Less_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      --  0 <  0 - 0
      -- -1 <  0 - 1
      --  0 < -1 - 0
      -- -1 < -1 - 0
      --  x < x  - 00
      --  x < y  - 0110

      procedure Test_Signed_Less_Than is new Test_Comparison_Operation
        ("0100",      -- constant word results
         "000110",    -- simple word results
         "00000110",  -- variable word results
         "signed less than.",
         Signed_Less_Than);

   begin

      Test_Signed_Less_Than;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(72) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(73) Incorrect number allocations.");
   end Test_Signed_Less_Than;

   --------------------------------
   -- Test_Unsigned_Greater_Than --
   --------------------------------

   procedure Test_Unsigned_Greater_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 > 0 - 0
      -- 1 > 0 - 1
      -- 0 > 1 - 0
      -- 1 > 1 - 0
      -- x > x  - 00
      -- x > y  - 0110

      procedure Test_Unsigned_Greater_Than is new Test_Comparison_Operation
        ("0100",      -- constant word results
         "000110",    -- simple word results
         "00000110",  -- variable word results
         "unsigned greater than.",
         Unsigned_Greater_Than);

   begin

      Test_Unsigned_Greater_Than;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(74) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(75) Incorrect number allocations.");
   end Test_Unsigned_Greater_Than;

   ------------------------------
   -- Test_Signed_Greater_Than --
   ------------------------------

   procedure Test_Signed_Greater_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      --  0 >  0 - 0
      -- -1 >  0 - 0
      --  0 > -1 - 1
      -- -1 > -1 - 0
      --  x >  x - 00
      --  x >  y - 0110

      procedure Test_Signed_Greater_Than is new Test_Comparison_Operation
        ("0010",      -- constant word results
         "000110",    -- simple word results
         "00000110",  -- variable word results
         "signed greater than.",
         Signed_Greater_Than);
   begin

      Test_Signed_Greater_Than;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(76) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(77) Incorrect number allocations.");
   end Test_Signed_Greater_Than;

   -----------------
   -- Test_Negate --
   -----------------

   procedure Test_Negate (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Negate (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( ) 0",
         The_Test     => "(78) negate.");

      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Negate (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( ) 1",
         The_Test     => "(79) negate.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Negate (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word ),
         The_Expected => "f( a0 ) 01 f( a0 b0 ) 0110",
         The_Test     => "(80) negate.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(81) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(82) Incorrect number allocations.");
   end Test_Negate;

   --------------
   -- Test_Add --
   --------------

   procedure Test_Add (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      -- 0 + 0 - 0
      -- 1 + 0 - 1
      -- 0 + 1 - 1
      -- 1 + 1 - 0
      -- x + x - 0110
      -- x + y - 0111

      procedure Test_Add is new Test_Binary_Operation
        ("0110",      -- constant word results
         "01100111",  -- variable word results
         "add.",
         Add);
   begin
      Test_Add;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(83) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(84) Incorrect number allocations.");
   end Test_Add;

   -------------------
   -- Test_Subtract --
   -------------------

   procedure Test_Subtract
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 - 0 - 0
      -- 1 - 0 - 1
      -- 0 - 1 - 1
      -- 1 - 1 - 0
      -- x - x - 0110
      -- x - y - 0100

      procedure Test_Subtract is new Test_Binary_Operation
        ("0110",      -- constant word results
         "01100100",  -- variable word results
         "subtract.",
         Subtract);

   begin

      Test_Subtract;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(85) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(86) Incorrect number allocations.");
   end Test_Subtract;

   -------------------
   -- Test_Multiply --
   -------------------

   procedure Test_Multiply
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 * 0 - 0
      -- 1 * 0 - 0
      -- 0 * 1 - 0
      -- 1 * 1 - 1
      -- x * x - 0001
      -- x * y - 0110

      procedure Test_Multiply is new Test_Binary_Operation
        ("0001",      -- constant word results
         "00010110",  -- variable word results
         "multiply.",
         Multiply);

   begin

      Test_Multiply;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(87) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(88) Incorrect number allocations.");
   end Test_Multiply;

   --------------------------
   -- Test_Unsigned_Divide --
   --------------------------

   procedure Test_Unsigned_Divide
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 / 0 - 1
      -- 1 / 0 - 1
      -- 0 / 1 - 0
      -- 1 / 1 - 1
      -- x / x - 1001
      -- x / y - 1010

      procedure Test_Unsigned_Divide is new Test_Binary_Operation
        ("1101",      -- constant word results
         "10011010",  -- variable word results
         "unsigned divide.",
         Unsigned_Divide);

   begin

      Test_Unsigned_Divide;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(89) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(90) Incorrect number allocations.");
   end Test_Unsigned_Divide;

   ------------------------
   -- Test_Signed_Divide --
   ------------------------

   procedure Test_Signed_Divide
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      --  0 /  0 - 1
      -- -1 /  0 - 1
      --  0 / -1 - 0
      -- -1 / -1 - 1
      --  x /  x - 1001
      --  x /  y - 1010

      procedure Test_Signed_Divide is new Test_Binary_Operation
        ("1101",      -- constant word results
         "10011010",  -- variable word results
         "signed divide.",
         Signed_Divide);

   begin
      Test_Signed_Divide;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(91) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(92) Incorrect number allocations.");
   end Test_Signed_Divide;

   -----------------------------
   -- Test_Unsigned_Remainder --
   -----------------------------

   procedure Test_Unsigned_Remainder
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 rem 0 - 0
      -- 1 rem 0 - 1
      -- 0 rem 1 - 0
      -- 1 rem 1 - 0
      -- x rem x - 0100
      -- x rem y - 0000

      procedure Test_Unsigned_Remainder is new Test_Binary_Operation
        ("0100",      -- constant word results
         "01000000",  -- variable word results
         "unsigned remainder.",
         Unsigned_Remainder);

   begin

      Test_Unsigned_Remainder;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(93) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(94) Incorrect number allocations.");
   end Test_Unsigned_Remainder;

   ---------------------------
   -- Test_Signed_Remainder --
   ---------------------------

   procedure Test_Signed_Remainder
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      -- 0 rem 0 - 0
      -- 1 rem 0 - 1
      -- 0 rem 1 - 0
      -- 1 rem 1 - 0
      -- x rem x - 0100
      -- x rem y - 0000

      procedure Test_Signed_Remainder is new Test_Binary_Operation
        ("0100",      -- constant word results
         "01000000",  -- variable word results
         "signed remainder.",
         Signed_Remainder);

   begin

      Test_Signed_Remainder;

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(95) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(96) Incorrect number allocations.");
   end Test_Signed_Remainder;

   -------------------------
   -- Test_Assign_Element --
   -------------------------

   procedure Test_Assign_Element
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Array : Word_Type;
      The_Index : Word_Type;
      The_Word  : Word_Type;
      The_Equ1  : Equation_Type;
      The_Equ2  : Equation_Type;

   begin
      -- test constant words
      -- array (false,false), index (false), and value (true)

      Create (The_Equ1, False);
      Create (The_Equ2, False);
      The_Array := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, False);
      The_Index := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assign_Element (The_Array, The_Word, The_Index);
      Dispose (The_Word);
      Dispose (The_Index);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Array ),
         The_Expected => "f( ) 1 f( ) 0",
         The_Test     => "(97) assign element.");

      -- test constant words
      -- array [previous], index (true), and value (true)

      Create (The_Equ1, True);
      The_Index := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assign_Element (The_Array, The_Word, The_Index);
      Dispose (The_Word);
      Dispose (The_Index);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Array ),
         The_Expected => "f( ) 1 f( ) 1",
         The_Test     => "(98) assign element.");

      Dispose (The_Array);

      -- test variable words
      -- array (a0,b0), index (c0), and value (d0)

      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Array := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, 2);
      The_Index := new Word_Array_Type'(0 => The_Equ1);

      Create (The_Equ1, 3);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assign_Element (The_Array, The_Word, The_Index);
      Dispose (The_Word);
      Dispose (The_Index);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Array ),
         The_Expected => "f( a0 c0 d0 ) 00011101 f( b0 c0 d0 ) 01000111",
         The_Test     => "(99) assign element.");

      Dispose (The_Array);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(100) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(101) Incorrect number allocations.");
   end Test_Assign_Element;

   -------------------------
   -- Test_Access_Element --
   -------------------------

   procedure Test_Access_Element
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Array : Word_Type;
      The_Index : Word_Type;
      The_Equ1  : Equation_Type;
      The_Equ2  : Equation_Type;

   begin
      -- test constant words array (true,true) and index (false)
      Create (The_Equ1, True);
      Create (The_Equ2, True);
      The_Array := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, False);
      The_Index := new Word_Array_Type'(0 => The_Equ1);

      Access_Element (The_Array, The_Index, 1);
      Dispose (The_Index);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Array),
         The_Expected => "f( ) 1",
         The_Test     => "(102) access element.");

      Dispose (The_Array);

      -- test constant words array (true,true) and index (true)
      Create (The_Equ1, True);
      Create (The_Equ2, True);
      The_Array := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, True);
      The_Index := new Word_Array_Type'(0 => The_Equ1);

      Access_Element (The_Array, The_Index, 1);
      Dispose (The_Index);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Array),
         The_Expected => "f( ) 1",
         The_Test     => "(103) access element.");

      Dispose (The_Array);

      -- test variable words array (a0,b0) and index (c0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Array := new Word_Array_Type'(The_Equ1, The_Equ2);

      Create (The_Equ1, 2);
      The_Index := new Word_Array_Type'(0 => The_Equ1);

      Access_Element (The_Array, The_Index, 1);
      Dispose (The_Index);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Array),
         The_Expected => "f( a0 b0 c0 ) 01010011",
         The_Test     => "(104) access element.");

      Dispose (The_Array);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(105) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(106) Incorrect number allocations.");
   end Test_Access_Element;

   ------------------
   -- Test_If_Else --
   ------------------

   procedure Test_If_Else
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Condition : Word_Type;
      The_Word1     : Word_Type;
      The_Word2     : Word_Type;
      The_Equ       : Equation_Type;

   begin
      -- test constant words, condition (true), then (false), and else (true)
      Create (The_Equ, True);
      The_Condition := new Word_Array_Type'(0 => The_Equ);

      Create (The_Equ, False);
      The_Word1 := new Word_Array_Type'(0 => The_Equ);

      Create (The_Equ, True);
      The_Word2 := new Word_Array_Type'(0 => The_Equ);

      If_Else (The_Condition, The_Word1, The_Word2);
      Dispose (The_Condition);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) 0",
         The_Test     => "(107) if else.");

      Dispose (The_Word1);

      -- test constant words, condition (false), then (false), and else (true)
      Create (The_Equ, False);
      The_Condition := new Word_Array_Type'(0 => The_Equ);

      Create (The_Equ, False);
      The_Word1 := new Word_Array_Type'(0 => The_Equ);

      Create (The_Equ, True);
      The_Word2 := new Word_Array_Type'(0 => The_Equ);

      If_Else (The_Condition, The_Word1, The_Word2);
      Dispose (The_Condition);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( ) 1",
         The_Test     => "(108) if else.");

      Dispose (The_Word1);

      -- test variable words, condition (a0), then (b0), and else (c0)
      Create (The_Equ, 0);
      The_Condition := new Word_Array_Type'(0 => The_Equ);

      Create (The_Equ, 1);
      The_Word1 := new Word_Array_Type'(0 => The_Equ);

      Create (The_Equ, 2);
      The_Word2 := new Word_Array_Type'(0 => The_Equ);

      If_Else (The_Condition, The_Word1, The_Word2);
      Dispose (The_Condition);
      Dispose (The_Word2);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word1),
         The_Expected => "f( a0 b0 c0 ) 00011011",
         The_Test     => "(109) if else.");

      Dispose (The_Word1);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(110) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(111) Incorrect number allocations.");
   end Test_If_Else;

   ------------------
   -- Test_Convert --
   ------------------

   procedure Test_Convert
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
      The_Equ3 : Equation_Type;
   begin
      -- test variable word (a0,b0,c0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      Create (The_Equ3, 2);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2, The_Equ3);

      -- test variable word [previous] to same size
      Convert (The_Word, 3);
      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01 f( b0 ) 01 f( c0 ) 01",
         The_Test     => "(112) convert.");

      -- test variable word [previous] to size - 2
      Convert (The_Word, 1);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01",
         The_Test     => "(113) convert.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      -- test variable word [previous] to size + 2
      Convert (The_Word, 4);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 01 f( b0 ) 01 f( ) 0 f( ) 0",
         The_Test     => "(114) convert.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(115) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(116) Incorrect number allocations.");
   end Test_Convert;

   --------------------
   -- Test_Normalize --
   --------------------

   procedure Test_Normalize
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false,true)
      Create (The_Equ1, False);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( ) 0 f( ) 1",
         The_Test     => "(117) normailze.");

      Dispose (The_Word);

      -- test variable word (false,a0)
      Create (The_Equ1, False);
      Create (The_Equ2, 0);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 00 f( a0 ) 01",
         The_Test     => "(118) normailze.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word);

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 b0 ) 0101 f( a0 b0 ) 0011",
         The_Test     => "(119) normailze.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(120) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(121) Incorrect number allocations.");
   end Test_Normalize;

   -----------------------------------
   -- Test_Normalize_With_Variables --
   -----------------------------------

   procedure Test_Normalize_With_Variables
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constnat word (false,true) and variables (a0)
      Create (The_Equ1, False);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word, Set_Of ((1 => 0)));

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 00 f( a0 ) 11",
         The_Test     => "(122) normalize with variables.");

      Dispose (The_Word);

      -- test variable word (false,a0) and variables (a0)
      Create (The_Equ1, False);
      Create (The_Equ2, 0);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word, Set_Of ((1 => 0)));

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 ) 00 f( a0 ) 01",
         The_Test     => "(123) normalize with variables.");

      Dispose (The_Word);

      -- test variable word (false,a0) and variables (a0)
      Create (The_Equ1, False);
      Create (The_Equ2, 0);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word, Set_Of ((1 => 1)));

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( b0 ) 00 f( a0 b0 ) 0101",
         The_Test     => "(124) normalize with variables.");

      Dispose (The_Word);

      -- test variable word (a0,b0) and variables (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Normalize (The_Word, Set_Of ((0, 1)));

      Assert
        (The_Result   => Image_Of_Test_Word (The_Word),
         The_Expected => "f( a0 b0 ) 0101 f( a0 b0 ) 0011",
         The_Test     => "(125) normalize with variables.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(126) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(127) Incorrect number allocations.");
   end Test_Normalize_With_Variables;

   ----------------------
   -- Test_Is_Constant --
   ----------------------

   procedure Test_Is_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Is_Constant (The_Word),
         The_Expected => True,
         The_Test     => "(128) is constant.");

      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Is_Constant (The_Word),
         The_Expected => True,
         The_Test     => "(129) is constant.");

      Dispose (The_Word);

      -- test constant word (false,false)
      Create (The_Equ1, False);
      Create (The_Equ2, False);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Is_Constant (The_Word),
         The_Expected => True,
         The_Test     => "(130) is constant.");

      Dispose (The_Word);

      -- test constant word (true,true)
      Create (The_Equ1, True);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Is_Constant (The_Word),
         The_Expected => True,
         The_Test     => "(131) is constant.");

      Dispose (The_Word);

      -- test variable word (false,a0)
      Create (The_Equ1, False);
      Create (The_Equ2, 0);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Is_Constant (The_Word),
         The_Expected => False,
         The_Test     => "(132) is constant.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Is_Constant (The_Word),
         The_Expected => False,
         The_Test     => "(133) is constant.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(134) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(135) Incorrect number allocations.");
   end Test_Is_Constant;

   ------------------------------
   -- Test_To_Constant_Modular --
   ------------------------------

   procedure Test_To_Constant_Modular
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => To_Constant (The_Word),
         The_Expected => 0,
         The_Test     => "(136) to constant.");

      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => To_Constant (The_Word),
         The_Expected => 1,
         The_Test     => "(137) to constant.");

      Dispose (The_Word);

      -- test constant word (false,false)
      Create (The_Equ1, False);
      Create (The_Equ2, False);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => To_Constant (The_Word),
         The_Expected => 0,
         The_Test     => "(138) to constant.");

      Dispose (The_Word);

      -- test constant word (true,true)
      Create (The_Equ1, True);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => To_Constant (The_Word),
         The_Expected => 3,
         The_Test     => "(139) to constant.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(140) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(141) Incorrect number allocations.");
   end Test_To_Constant_Modular;

   ------------------------------------
   -- Test_To_Constant_Boolean_Array --
   ------------------------------------

   procedure Test_To_Constant_Boolean_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Boolean_Package.Image_Of (To_Constant (The_Word)),
         The_Expected => "0",
         The_Test     => "(142) to constant boolean array.");
      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Boolean_Package.Image_Of (To_Constant (The_Word)),
         The_Expected => "1",
         The_Test     => "(143) to constant boolean array.");

      Dispose (The_Word);

      -- test constant word (false,false)
      Create (The_Equ1, False);
      Create (The_Equ2, False);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Boolean_Package.Image_Of (To_Constant (The_Word)),
         The_Expected => "00",
         The_Test     => "(144) to constant boolean array.");

      Dispose (The_Word);

      -- test constant word (true,true)
      Create (The_Equ1, True);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Boolean_Package.Image_Of (To_Constant (The_Word)),
         The_Expected => "11",
         The_Test     => "(145) to constant boolean array.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(146) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(147) Incorrect number allocations.");
   end Test_To_Constant_Boolean_Array;

   -----------------------
   -- Test_Variables_Of --
   -----------------------

   procedure Test_Variables_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false,true)
      Create (The_Equ1, False);
      Create (The_Equ2, True);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Image_Of (Variables_Of (The_Word)),
         The_Expected => Image_Of (EMPTY_SET),
         The_Test     => "(148) variables of.");

      Dispose (The_Word);

      -- test variable word (false,a0)
      Create (The_Equ1, False);
      Create (The_Equ2, 0);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Image_Of (Variables_Of (The_Word)),
         The_Expected => Image_Of (Set_Of ((1 => 0))),
         The_Test     => "(149) variables of.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Image_Of (Variables_Of (The_Word)),
         The_Expected => Image_Of (Set_Of ((0, 1))),
         The_Test     => "(150) variables of.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(151) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(152) Incorrect number allocations.");
   end Test_Variables_Of;

   --------------------
   -- Test_Length_Of --
   --------------------

   procedure Test_Length_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Length_Of (The_Word),
         The_Expected => 1,
         The_Test     => "(153) length of.");

      Dispose (The_Word);

      -- test constant word (false,false)
      Create (The_Equ1, False);
      Create (The_Equ2, False);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Length_Of (The_Word),
         The_Expected => 2,
         The_Test     => "(154) length of.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Length_Of (The_Word),
         The_Expected => 2,
         The_Test     => "(155) length of.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(156) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(157) Incorrect number allocations.");
   end Test_Length_Of;

   ----------------
   -- Test_Solve --
   ----------------

   procedure Test_Solve (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false) and no values
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, (1 .. 0 => False))),
         The_Expected => "0",
         The_Test     => "(158) solve.");

      Dispose (The_Word);

      -- test constant word (true) and no values
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, (1 .. 0 => False))),
         The_Expected => "1",
         The_Test     => "(159) solve.");

      Dispose (The_Word);

      -- test variable word (a0)
      Create (The_Equ1, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      --  and value (false)
      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, (1 .. 0 => False))),
         The_Expected => "0",
         The_Test     => "(160).0 solve.");

      --  and value (true)
      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, (0 .. 0 => True))),
         The_Expected => "1",
         The_Test     => "(161).1 solve.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      -- and values (false,false)
      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, Boolean_Array_Type'(False, False))),
         The_Expected => "00",
         The_Test     => "(162).0 solve.");

      -- values (true,false)
      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, Boolean_Array_Type'(True, False))),
         The_Expected => "10",
         The_Test     => "(163).2 solve.");

      -- and values (false,true)
      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, Boolean_Array_Type'(False, True))),
         The_Expected => "01",
         The_Test     => "(164).3solve.");

      -- values (true,true)
      Assert
        (The_Result =>
           Boolean_Package.Image_Of
             (Solve (The_Word, Boolean_Array_Type'(True, True))),
         The_Expected => "11",
         The_Test     => "(165).4 solve.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(166) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(167) Incorrect number allocations.");
   end Test_Solve;

   ---------------------
   -- Test_Boolean_Of --
   ---------------------

   procedure Test_Boolean_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Boolean_Of (The_Word),
         The_Expected => " f( ) 0",
         The_Test     => "(168) boolean of.");

      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Boolean_Of (The_Word),
         The_Expected => " f( ) 1",
         The_Test     => "(169) boolean of.");

      Dispose (The_Word);

      -- test simple word (a0)
      Create (The_Equ1, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Boolean_Of (The_Word),
         The_Expected => " f( a0 ) 01",
         The_Test     => "(170) boolean of.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Boolean_Of (The_Word),
         The_Expected => " f( a0 ) 01 f( b0 ) 01",
         The_Test     => "(171) boolean of.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(172) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(173) Incorrect number allocations.");
   end Test_Boolean_Of;

   -------------------
   -- Test_Image_Of --
   -------------------

   procedure Test_Image_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Image_Of (The_Word),
         The_Expected => " f( ) 0",
         The_Test     => "(174) image of.");

      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Image_Of (The_Word),
         The_Expected => " f( ) 1",
         The_Test     => "(175) image of.");

      Dispose (The_Word);

      -- test simple word (a0)
      Create (The_Equ1, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result   => Image_Of (The_Word),
         The_Expected => " f( a0 ) a0",
         The_Test     => "(176) image of.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      Assert
        (The_Result   => Image_Of (The_Word),
         The_Expected => " f( a0 ) a0 f( b0 ) b0",
         The_Test     => "(177) image of.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(178) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(179) Incorrect number allocations.");
   end Test_Image_Of;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_String : Unbounded_String;

      procedure Process (The_Equation : Equation_Type) is
      begin
         The_String := The_String & Image_Of (The_Equation) & ";";
      end Process;

      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
      The_Equ2 : Equation_Type;
   begin
      -- test constant word (false)
      Create (The_Equ1, False);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      The_String := Null_Unbounded_String;
      Iterate (The_Word, Process'Access);

      Assert
        (The_Result   => To_String (The_String),
         The_Expected => "f( ) 0;",
         The_Test     => "(180) iterate.");

      Dispose (The_Word);

      -- test constant word (true)
      Create (The_Equ1, True);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      The_String := Null_Unbounded_String;
      Iterate (The_Word, Process'Access);

      Assert
        (The_Result   => To_String (The_String),
         The_Expected => "f( ) 1;",
         The_Test     => "(181) iterate.");

      Dispose (The_Word);

      -- test simple word (a0)
      Create (The_Equ1, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      The_String := Null_Unbounded_String;
      Iterate (The_Word, Process'Access);

      Assert
        (The_Result   => To_String (The_String),
         The_Expected => "f( a0 ) a0;",
         The_Test     => "(182) iterate.");

      Dispose (The_Word);

      -- test variable word (a0,b0)
      Create (The_Equ1, 0);
      Create (The_Equ2, 1);
      The_Word := new Word_Array_Type'(The_Equ1, The_Equ2);

      The_String := Null_Unbounded_String;
      Iterate (The_Word, Process'Access);

      Assert
        (The_Result   => To_String (The_String),
         The_Expected => "f( a0 ) a0;f( b0 ) b0;",
         The_Test     => "(183) iterate.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(184) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(185) Incorrect number allocations.");
   end Test_Iterate;

   ---------------
   -- Test_Pool --
   ---------------

   procedure Test_Pool (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      The_Word : Word_Type;
      The_Equ1 : Equation_Type;
   begin
      -- test simple variable (a0)
      Create (The_Equ1, 0);
      The_Word := new Word_Array_Type'(0 => The_Equ1);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations + 1,
         The_Test     => "(186) Test pool.");

      Dispose (The_Word);

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "(187) Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "(188) Incorrect number allocations.");
   end Test_Pool;

end Word_Package.Word_Test;

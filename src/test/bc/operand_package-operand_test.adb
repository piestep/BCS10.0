-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Text_IO;
--
with Ada.Tags;              use Ada.Tags;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Package; use Test_Package;
--
with Pool_Package;
--
with System_Package; use System_Package;
--

package body Operand_Package.Operand_Test is

   The_Type       : Type_Pointer;
   The_Identifier : Identifier_Pointer;

   A_Constant    : Operand_Pointer;
   A_Variable    : Operand_Pointer;
   An_Identifier : Operand_Pointer;
   An_Array      : Operand_Pointer;

   The_Unmarked_Type_Allocations       : SYSNatural;
   The_Unmarked_Identifier_Allocations : SYSNatural;
   The_Unmarked_Operand_Allocations    : SYSNatural;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("operand_package.operand_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Copy'Access, "test_copy!");
      Register_Routine(The_Test, Test_Dispose'Access, "test_dispose!");
      Register_Routine(The_Test, Test_Is_Constant'Access, "test_is_constant!");
      Register_Routine(The_Test, Test_Is_Variable'Access, "test_is_variable!");
      Register_Routine(The_Test, Test_Is_Identifier'Access, "test_is_identifier!");
      Register_Routine(The_Test, Test_Is_Array'Access, "test_is_array!");
      Register_Routine(The_Test, Test_Constant_Operation_Uniary'Access, "test_constant_operation_uniary!");
      Register_Routine(The_Test, Test_Constant_Operation_Binary'Access, "test_constant_operation_binary!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      The_Type :=
        new Signed_Type'
          (The_Previous => Type_Package.The_Last,
           The_Base  => Integer_Type,
           The_First => -2,
           The_Last  => 1,
           The_Size  => 2);
      Type_Package.The_Last := The_Type;

      The_Identifier :=
        new Variable_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String  => To_Unbounded_String ("VARIABLE"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Identifier_Package.The_Last := The_Identifier;

      A_Constant := new Constant_Operand'(The_Type => The_Type, The_Value => 0);
      A_Variable    := new Variable_Operand'(The_Type => The_Type);
      An_Identifier :=
        new Identifier_Operand'
          (The_Type => The_Type, The_Identifier => The_Identifier);
      An_Array :=
        new Array_Operand'
          (The_Type       => The_Type,
           The_Identifier => The_Identifier,
           The_Index      =>
              new Constant_Operand'(The_Type => The_Type, The_Value => 0));

      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);
      The_Unmarked_Operand_Allocations :=
        Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      Dispose(A_Constant);
      Dispose(A_Variable);
      Dispose(An_Identifier);
      Dispose(An_Array);

      Type_Package.Clear;
      Identifier_Package.Clear;

      Ada.Text_IO.Put_Line("Operand_Package.Operand_Test");
      Ada.Text_IO.Put_Line
        ("Type_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Type_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Identifier_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Operand_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool)));
   end Tear_Down_Case;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (The_Result   : Tag;
      The_Expected : Tag;
      The_Test     : String;
      The_Text     : String := "")
   is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test & " " & The_Text);
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (The_Result   : Type_Pointer;
      The_Expected : Type_Pointer;
      The_Test     : String;
      The_Text     : String := "")
   is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test & " " & The_Text);
   end Assert;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (The_Result   : Identifier_Pointer;
      The_Expected : Identifier_Pointer;
      The_Test     : String;
      The_Text     : String := "")
   is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test & " " & The_Text);
   end Assert;

   ---------------
   -- Test_Copy --
   ---------------

   procedure Test_Copy (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Operand : Operand_Pointer;
   begin
      The_Operand := Copy (A_Constant);
      Assert
        (The_Result   => The_Operand'Tag,
         The_Expected => A_Constant'Tag,
         The_Test     => "Test copy (constant operand).");
      Assert
        (The_Result   => The_Operand.The_Type,
         The_Expected => A_Constant.The_Type,
         The_Test     => "Test copy (constant type).");
      Assert
        (The_Result   => Constant_Operand (The_Operand.all).The_Value,
         The_Expected => Constant_Operand (A_Constant.all).The_Value,
         The_Test     => "Test copy (value).");

      Dispose (The_Operand);

      The_Operand := Copy (A_Variable);
      Assert
        (The_Result   => The_Operand'Tag,
         The_Expected => A_Variable'Tag,
         The_Test     => "Test copy (variable operand).");
      Assert
        (The_Result   => The_Operand.The_Type,
         The_Expected => A_Variable.The_Type,
         The_Test     => "Test copy (variable type).");

      Dispose (The_Operand);

      The_Operand := Copy (An_Identifier);
      Assert
        (The_Result   => The_Operand'Tag,
         The_Expected => An_Identifier'Tag,
         The_Test     => "Test copy (identifier operand).");
      Assert
        (The_Result   => The_Operand.The_Type,
         The_Expected => An_Identifier.The_Type,
         The_Test     => "Test copy (identifier type).");
      Assert
        (The_Result   => Identifier_Operand (The_Operand.all).The_Identifier,
         The_Expected => Identifier_Operand (An_Identifier.all).The_Identifier,
         The_Test     => "Test copy (identifier).");

      Dispose (The_Operand);

      The_Operand := Copy (An_Array);
      Assert
        (The_Result   => The_Operand'Tag,
         The_Expected => An_Array'Tag,
         The_Test     => "Test copy (array operand).");
      Assert
        (The_Result   => The_Operand.The_Type,
         The_Expected => An_Array.The_Type,
         The_Test     => "Test copy (array type).");
      Assert
        (The_Result   => Array_Operand (The_Operand.all).The_Index'Tag,
         The_Expected => Array_Operand (An_Array.all).The_Index'Tag,
         The_Test     => "Test copy (index).");

      Dispose (The_Operand);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Copy;

   ------------------
   -- Test_Dispose --
   ------------------

   procedure Test_Dispose (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      The_Operand : Operand_Pointer;
   begin
      The_Operand :=
        new Constant_Operand'(The_Type => The_Type, The_Value => 0);
      Dispose (The_Operand);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Test dispose (constant).");

      The_Operand := new Variable_Operand'(The_Type => The_Type);
      Dispose (The_Operand);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Test dispose (variable).");

      The_Operand :=
        new Identifier_Operand'
          (The_Type => The_Type, The_Identifier => The_Identifier);
      Dispose (The_Operand);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Test dispose (identifier).");

      The_Operand :=
        new Array_Operand'
          (The_Type       => The_Type,
           The_Identifier => The_Identifier,
           The_Index      =>
              new Constant_Operand'(The_Type => The_Type, The_Value => 0));
      Dispose (The_Operand);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Test dispose (array).");
   end Test_Dispose;

   ----------------------
   -- Test_Is_Constant --
   ----------------------

   procedure Test_Is_Constant (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Constant (A_Constant),
         The_Expected => True,
         The_Test     => "Test is constant (constant).");
      Assert
        (The_Result   => Is_Constant (A_Variable),
         The_Expected => False,
         The_Test     => "Test is constant (variable).");
      Assert
        (The_Result   => Is_Constant (An_Identifier),
         The_Expected => False,
         The_Test     => "Test is constant (identifier).");
      Assert
        (The_Result   => Is_Constant (An_Array),
         The_Expected => False,
         The_Test     => "Test is constant (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Is_Constant;

   ----------------------
   -- Test_Is_Variable --
   ----------------------

   procedure Test_Is_Variable (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Variable (A_Constant),
         The_Expected => False,
         The_Test     => "Test is variable (constant).");
      Assert
        (The_Result   => Is_Variable (A_Variable),
         The_Expected => True,
         The_Test     => "Test is variable (variable).");
      Assert
        (The_Result   => Is_Variable (An_Identifier),
         The_Expected => False,
         The_Test     => "Test is variable (identifier).");
      Assert
        (The_Result   => Is_Variable (An_Array),
         The_Expected => False,
         The_Test     => "Test is variable (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Is_Variable;

   ------------------------
   -- Test_Is_Identifier --
   ------------------------

   procedure Test_Is_Identifier (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Identifier (A_Constant),
         The_Expected => False,
         The_Test     => "Test is identifier (constant).");
      Assert
        (The_Result   => Is_Identifier (A_Variable),
         The_Expected => False,
         The_Test     => "Test is identifier (variable).");
      Assert
        (The_Result   => Is_Identifier (An_Identifier),
         The_Expected => True,
         The_Test     => "Test is identifier (identifier).");
      Assert
        (The_Result   => Is_Identifier (An_Array),
         The_Expected => False,
         The_Test     => "Test is identifier (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Is_Identifier;

   -------------------
   -- Test_Is_Array --
   -------------------

   procedure Test_Is_Array (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Array (A_Constant),
         The_Expected => False,
         The_Test     => "Test is array (constant).");
      Assert
        (The_Result   => Is_Array (A_Variable),
         The_Expected => False,
         The_Test     => "Test is array (variable).");
      Assert
        (The_Result   => Is_Array (An_Identifier),
         The_Expected => False,
         The_Test     => "Test is array (identifier).");
      Assert
        (The_Result   => Is_Array (An_Array),
         The_Expected => True,
         The_Test     => "Test is array (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Is_Array;

   ------------------------------------
   -- Test_Constant_Operation_Uniary --
   ------------------------------------

   procedure Test_Constant_Operation_Uniary
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Constant_Operation (Not_Symbol, 0),
         The_Expected => 1,
         The_Test     => "Test constant uniary operation (not 0).");
      Assert
        (The_Result   => Constant_Operation (Not_Symbol, 1),
         The_Expected => 0,
         The_Test     => "Test constant uniary operation (not 1).");

      Assert
        (The_Result   => Constant_Operation (Plus_Symbol, 1),
         The_Expected => 1,
         The_Test     => "Test constant uniary operation (+ 1).");
      Assert
        (The_Result   => Constant_Operation (Minus_Symbol, 1),
         The_Expected => -1,
         The_Test     => "Test constant uniary operation (- 1).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Constant_Operation_Uniary;

   ------------------------------------
   -- Test_Constant_Operation_Binary --
   ------------------------------------

   procedure Test_Constant_Operation_Binary
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Constant_Operation (Equal_Symbol, 0, 0),
         The_Expected => Boolean_True,
         The_Test     => "Test constant binary operation (equal 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Equal_Symbol, 0, 1),
         The_Expected => Boolean_False,
         The_Test     => "Test constant binary operation (equal 0, 1).");

      Assert
        (The_Result   => Constant_Operation (Not_Equal_Symbol, 0, 0),
         The_Expected => Boolean_False,
         The_Test     => "Test constant binary operation (not equal 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Not_Equal_Symbol, 0, 1),
         The_Expected => Boolean_True,
         The_Test     => "Test constant binary operation (not equal 0, 1).");

      Assert
        (The_Result   => Constant_Operation (Less_Than_Symbol, 0, 0),
         The_Expected => Boolean_False,
         The_Test     => "Test constant binary operation (less than 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Less_Than_Symbol, 0, 1),
         The_Expected => Boolean_True,
         The_Test     => "Test constant binary operation (less than 0, 1).");

      Assert
        (The_Result   => Constant_Operation (Less_Than_Equal_Symbol, 0, 0),
         The_Expected => Boolean_True,
         The_Test => "Test constant binary operation (less than equal 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Less_Than_Equal_Symbol, 0, 1),
         The_Expected => Boolean_True,
         The_Test => "Test constant binary operation (less than equal 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Less_Than_Equal_Symbol, 1, 0),
         The_Expected => Boolean_False,
         The_Test => "Test constant binary operation (less than equal 1, 0).");

      Assert
        (The_Result   => Constant_Operation (Greater_Than_Symbol, 0, 0),
         The_Expected => Boolean_False,
         The_Test     => "Test constant binary operation (greater than 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Greater_Than_Symbol, 1, 0),
         The_Expected => Boolean_True,
         The_Test     => "Test constant binary operation (greater than 1, 0).");

      Assert
        (The_Result   => Constant_Operation (Greater_Than_Equal_Symbol, 0, 0),
         The_Expected => Boolean_True,
         The_Test     =>
           "Test constant binary operation (greater than equal 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Greater_Than_Equal_Symbol, 0, 1),
         The_Expected => Boolean_False,
         The_Test     =>
           "Test constant binary operation (greater than equal 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Greater_Than_Equal_Symbol, 1, 0),
         The_Expected => Boolean_True,
         The_Test     =>
           "Test constant binary operation (greater than equal 1, 0).");

      Assert
        (The_Result   => Constant_Operation (And_Symbol, 0, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (and 0, 0).");
      Assert
        (The_Result   => Constant_Operation (And_Symbol, 1, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (and 1, 0).");
      Assert
        (The_Result   => Constant_Operation (And_Symbol, 0, 1),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (and 0, 1).");
      Assert
        (The_Result   => Constant_Operation (And_Symbol, 1, 1),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (and 1, 1).");

      Assert
        (The_Result   => Constant_Operation (Or_Symbol, 0, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (or 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Or_Symbol, 1, 0),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (or 1, 0).");
      Assert
        (The_Result   => Constant_Operation (Or_Symbol, 0, 1),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (or 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Or_Symbol, 1, 1),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (or 1, 1).");

      Assert
        (The_Result   => Constant_Operation (Xor_Symbol, 0, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (xor 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Xor_Symbol, 1, 0),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (xor 1, 0).");
      Assert
        (The_Result   => Constant_Operation (Xor_Symbol, 0, 1),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (xor 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Xor_Symbol, 1, 1),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (xor 1, 1).");

      Assert
        (The_Result   => Constant_Operation (Plus_Symbol, 0, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (plus 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Plus_Symbol, 0, 1),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (plus 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Plus_Symbol, 1, 1),
         The_Expected => 2,
         The_Test     => "Test constant binary operation (plus 1, 1).");

      Assert
        (The_Result   => Constant_Operation (Minus_Symbol, 0, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (minus 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Minus_Symbol, 0, 1),
         The_Expected => -1,
         The_Test     => "Test constant binary operation (minus 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Minus_Symbol, 1, 1),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (minus 1, 1).");

      Assert
        (The_Result   => Constant_Operation (Times_Symbol, 0, 0),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (times 0, 0).");
      Assert
        (The_Result   => Constant_Operation (Times_Symbol, 0, 1),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (times 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Times_Symbol, 1, 1),
         The_Expected => 1,
         The_Test     => "Test constant binary operation (times 1, 1).");
      Assert
        (The_Result   => Constant_Operation (Times_Symbol, 2, 3),
         The_Expected => 6,
         The_Test     => "Test constant binary operation (times 2, 3).");

      Assert
        (The_Result   => Constant_Operation (Divide_Symbol, 0, 1),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (divide 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Divide_Symbol, 2, 1),
         The_Expected => 2,
         The_Test     => "Test constant binary operation (divide 2, 1).");
      Assert
        (The_Result   => Constant_Operation (Divide_Symbol, 6, 3),
         The_Expected => 2,
         The_Test     => "Test constant binary operation (divide 6, 3).");

      Assert
        (The_Result   => Constant_Operation (Mod_Symbol, 0, 1),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (mod 0, 1).");
      Assert
        (The_Result   => Constant_Operation (Mod_Symbol, 2, 2),
         The_Expected => 0,
         The_Test     => "Test constant binary operation (mod 2, 2).");
      Assert
        (The_Result   => Constant_Operation (Mod_Symbol, 3, 4),
         The_Expected => 3,
         The_Test     => "Test constant binary operation (mod 3, 4).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Operand_Package.The_Pool) =
             The_Unmarked_Operand_Allocations,
         "Incorrect operand allocations.");
   end Test_Constant_Operation_Binary;

end Operand_Package.Operand_Test;

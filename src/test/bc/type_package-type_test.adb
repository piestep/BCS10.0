-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

--
with AUnit.Assertions;
--
with Test_Package; use Test_Package;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Pool_Package;
--
with System_Package; use System_Package;
--

package body Type_Package.Type_Test is

   The_Unmarked_Type_Allocations : Natural;

   A_Boolean  : Type_Pointer;
   An_Integer : Type_Pointer;
   A_Modular  : Type_Pointer;
   An_Array   : Type_Pointer;

   -- Used for is compatianle testing.

   The_Boolean : Type_Pointer;
   The_Integer : Type_Pointer;
   The_Array   : Type_Pointer;
   The_Modular : Type_Pointer;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("type_package.type_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(The_Test, Test_Is_Array'Access, "test_is_array!");
      Register_Routine(The_Test, Test_Is_Scalar'Access, "test_is_scalar!");
      Register_Routine(The_Test, Test_Is_Discrete'Access, "test_is_discrete!");
      Register_Routine(The_Test, Test_Is_Signed'Access, "test_is_signed!");
      Register_Routine(The_Test, Test_Is_Modular'Access, "test_is_modular!");
      Register_Routine(The_Test, Test_Is_Boolean'Access, "test_is_boolean!");
      Register_Routine(The_Test, Test_Is_Integer'Access, "test_is_integer!");
      Register_Routine(The_Test, Test_Base_Of'Access, "test_base_of!");
      Register_Routine(The_Test, Test_First_Of'Access, "test_first_of!");
      Register_Routine(The_Test, Test_Last_Of'Access, "test_last_of!");
      Register_Routine(The_Test, Test_Size_Of'Access, "test_size_of!");
      Register_Routine(The_Test, Test_Is_Within'Access, "test_is_within!");
      Register_Routine(The_Test, Test_Is_Compatiable'Access, "test_is_compatiable!");
      Register_Routine(The_Test, Test_Best_Of'Access, "test_best_of!");
      Register_Routine(The_Test, Test_Size_Of_Integer'Access, "test_size_of_integer!");
      Register_Routine(The_Test, Test_Size_Of_Modular'Access, "test_size_of_modular!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      A_Boolean :=
        new Discrete_Type'
          (The_Base  => Universal_Boolean,
           The_First => Boolean_False,
           The_Last  => Boolean_True,
           The_Size  => 1);

      An_Integer :=
        new Signed_Type'
          (The_Base  => Integer_Type,
           The_First => -2,
           The_Last  => 1,
           The_Size  => 2);

      A_Modular :=
        new Modular_Type'
          (The_Base    => Universal_Integer,
           The_First   => 0,
           The_Last    => 3,
           The_Size    => 2,
           The_Modulas => 4);

      An_Array :=
        new Array_Type'
          (The_Base    => null,
           The_Index   => A_Modular,
           The_Element => An_Integer,
           The_First   => 1,
           The_Last    => 3);

      -- Used for is compatianle testing.
      The_Boolean :=
        new Discrete_Type'
          (The_Base  => Universal_Boolean,
           The_First => Boolean_False,
           The_Last  => Boolean_True,
           The_Size  => 1);

      The_Integer :=
        new Signed_Type'
          (The_Base  => Integer_Type,
           The_First => -2,
           The_Last  => 1,
           The_Size  => 2);

      The_Array :=
        new Array_Type'
          (The_Base    => null,
           The_Index   => A_Modular,
           The_Element => An_Integer,
           The_First   => 1,
           The_Last    => 3);

      The_Modular :=
        new Modular_Type'
          (The_Base    => null,
           The_First   => 0,
           The_Last    => 3,
           The_Size    => 2,
           The_Modulas => 4);

      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down_Case;

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

   -------------------
   -- Test_Is_Array --
   -------------------

   procedure Test_Is_Array
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Array (A_Boolean),
         The_Expected => False,
         The_Test     => "Test is array (boolean).");

      Assert
        (The_Result   => Is_Array (An_Integer),
         The_Expected => False,
         The_Test     => "Test is array (integer).");

      Assert
        (The_Result   => Is_Array (A_Modular),
         The_Expected => False,
         The_Test     => "Test is array (modular).");

      Assert
        (The_Result   => Is_Array (An_Array),
         The_Expected => True,
         The_Test     => "Test is array (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Array;

   --------------------
   -- Test_Is_Scalar --
   --------------------

   procedure Test_Is_Scalar
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Scalar (A_Boolean),
         The_Expected => True,
         The_Test     => "Test is scalar (boolean).");

      Assert
        (The_Result   => Is_Scalar (An_Integer),
         The_Expected => True,
         The_Test     => "Test is scalar (integer).");

      Assert
        (The_Result   => Is_Scalar (A_Modular),
         The_Expected => True,
         The_Test     => "Test is scalar (modular).");

      Assert
        (The_Result   => Is_Scalar (An_Array),
         The_Expected => False,
         The_Test     => "Test is scalar (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Scalar;

   ----------------------
   -- Test_Is_Discrete --
   ----------------------

   procedure Test_Is_Discrete
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Discrete (A_Boolean),
         The_Expected => True,
         The_Test     => "Test is discrete (boolean).");

      Assert
        (The_Result   => Is_Discrete (An_Integer),
         The_Expected => False,
         The_Test     => "Test is discrete (integer).");

      Assert
        (The_Result   => Is_Discrete (A_Modular),
         The_Expected => False,
         The_Test     => "Test is discrete (modular).");

      Assert
        (The_Result   => Is_Discrete (An_Array),
         The_Expected => False,
         The_Test     => "Test is discrete (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Discrete;

   --------------------
   -- Test_Is_Signed --
   --------------------

   procedure Test_Is_Signed
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Signed (A_Boolean),
         The_Expected => False,
         The_Test     => "Test is signed (boolean).");

      Assert
        (The_Result   => Is_Signed (An_Integer),
         The_Expected => True,
         The_Test     => "Test is signed (integer).");

      Assert
        (The_Result   => Is_Signed (A_Modular),
         The_Expected => False,
         The_Test     => "Test is signed (modular).");

      Assert
        (The_Result   => Is_Signed (An_Array),
         The_Expected => False,
         The_Test     => "Test is signed (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Signed;

   ---------------------
   -- Test_Is_Modular --
   ---------------------

   procedure Test_Is_Modular
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Modular (A_Boolean),
         The_Expected => False,
         The_Test     => "Test is modular (boolean).");

      Assert
        (The_Result   => Is_Modular (An_Integer),
         The_Expected => False,
         The_Test     => "Test is modular (integer).");

      Assert
        (The_Result   => Is_Modular (A_Modular),
         The_Expected => True,
         The_Test     => "Test is modular (modular).");

      Assert
        (The_Result   => Is_Modular (An_Array),
         The_Expected => False,
         The_Test     => "Test is modular (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Modular;

   ---------------------
   -- Test_Is_Boolean --
   ---------------------

   procedure Test_Is_Boolean
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Boolean (null),
         The_Expected => True,
         The_Test     => "Test is boolean (null).");

      Assert
        (The_Result   => Is_Boolean (A_Boolean),
         The_Expected => True,
         The_Test     => "Test is boolean (boolean).");

      Assert
        (The_Result   => Is_Boolean (An_Integer),
         The_Expected => False,
         The_Test     => "Test is boolean (integer).");

      Assert
        (The_Result   => Is_Boolean (A_Modular),
         The_Expected => False,
         The_Test     => "Test is boolean (modular).");

      Assert
        (The_Result   => Is_Boolean (An_Array),
         The_Expected => False,
         The_Test     => "Test is boolean (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Boolean;

   ---------------------
   -- Test_Is_Integer --
   ---------------------

   procedure Test_Is_Integer
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Integer (null),
         The_Expected => True,
         The_Test     => "Test is integer (null).");

      Assert
        (The_Result   => Is_Integer (A_Boolean),
         The_Expected => False,
         The_Test     => "Test is integer (boolean).");

      Assert
        (The_Result   => Is_Integer (An_Integer),
         The_Expected => True,
         The_Test     => "Test is integer (integer).");

      Assert
        (The_Result   => Is_Integer (A_Modular),
         The_Expected => True,
         The_Test     => "Test is integer (modular).");

      Assert
        (The_Result   => Is_Integer (An_Array),
         The_Expected => False,
         The_Test     => "Test is integer (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Integer;

   ------------------
   -- Test_Base_Of --
   ------------------

   procedure Test_Base_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Base_Of (null),
         The_Expected => null,
         The_Test     => "Test base of (null).");

      Assert
        (The_Result   => Base_Of (A_Boolean),
         The_Expected => Universal_Boolean,
         The_Test     => "Test base of (boolean).");

      Assert
        (The_Result   => Base_Of (An_Integer),
         The_Expected => Universal_Integer,
         The_Test     => "Test base of (Integer).");

      Assert
        (The_Result   => Base_Of (A_Modular),
         The_Expected => Universal_Integer,
         The_Test     => "Test base of (modular).");

      Assert
        (The_Result   => Base_Of (An_Array),
         The_Expected => An_Array,
         The_Test     => "Test base of (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Base_Of;

   -------------------
   -- Test_First_Of --
   -------------------

   procedure Test_First_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => First_Of (null),
         The_Expected => 0,
         The_Test     => "Test first of (null).");

      Assert
        (The_Result   => First_Of (A_Boolean),
         The_Expected => 0,
         The_Test     => "Test first of (boolean).");

      Assert
        (The_Result   => First_Of (A_Modular),
         The_Expected => 0,
         The_Test     => "Test first of (modular).");

      Assert
        (The_Result   => First_Of (An_Integer),
         The_Expected => -2,
         The_Test     => "Test first of (integer).");

      Assert
        (The_Result   => First_Of (An_Array),
         The_Expected => 1,
         The_Test     => "Test first of (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_First_Of;

   ------------------
   -- Test_Last_Of --
   ------------------

   procedure Test_Last_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Last_Of (null),
         The_Expected => 0,
         The_Test     => "Test last of (null).");

      Assert
        (The_Result   => Last_Of (A_Boolean),
         The_Expected => 1,
         The_Test     => "Test last of (boolean).");

      Assert
        (The_Result   => Last_Of (A_Modular),
         The_Expected => 3,
         The_Test     => "Test last of (modular).");

      Assert
        (The_Result   => Last_Of (An_Integer),
         The_Expected => 1,
         The_Test     => "Test last of (integer).");

      Assert
        (The_Result   => Last_Of (An_Array),
         The_Expected => 3,
         The_Test     => "Test last of (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Last_Of;

   ------------------
   -- Test_Size_Of --
   ------------------

   procedure Test_Size_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Size_Of (null),
         The_Expected => 0,
         The_Test     => "Test size of (null).");

      Assert
        (The_Result   => Size_Of (A_Boolean),
         The_Expected => 1,
         The_Test     => "Test size of (boolean).");

      Assert
        (The_Result   => Size_Of (A_Modular),
         The_Expected => 2,
         The_Test     => "Test size of (modular).");

      Assert
        (The_Result   => Size_Of (An_Integer),
         The_Expected => 2,
         The_Test     => "Test size of (integer).");

      Assert
        (The_Result   => Size_Of (An_Array),
         The_Expected => 6,
         The_Test     => "Test size of (array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Size_Of;

   --------------------
   -- Test_Is_Within --
   --------------------

   procedure Test_Is_Within
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Within (-1, null),
         The_Expected => True,
         The_Test     => "Test is within (-1, null).");
      Assert
        (The_Result   => Is_Within (0, null),
         The_Expected => True,
         The_Test     => "Test is within (0, null).");
      Assert
        (The_Result   => Is_Within (1, null),
         The_Expected => True,
         The_Test     => "Test is within (1, null).");

      Assert
        (The_Result   => Is_Within (-1, Universal_Boolean),
         The_Expected => False,
         The_Test     => "Test is within (-1, universal boolean).");
      Assert
        (The_Result   => Is_Within (0, Universal_Boolean),
         The_Expected => True,
         The_Test     => "Test is within (0, universal boolean).");
      Assert
        (The_Result   => Is_Within (1, Universal_Boolean),
         The_Expected => True,
         The_Test     => "Test is within (1, universal boolean).");
      Assert
        (The_Result   => Is_Within (2, Universal_Boolean),
         The_Expected => False,
         The_Test     => "Test is within (2, universal boolean).");

      Assert
        (The_Result   => Is_Within (-1, A_Boolean),
         The_Expected => False,
         The_Test     => "Test is within (-1, boolean).");
      Assert
        (The_Result   => Is_Within (0, A_Boolean),
         The_Expected => True,
         The_Test     => "Test is within (0, boolean).");
      Assert
        (The_Result   => Is_Within (1, A_Boolean),
         The_Expected => True,
         The_Test     => "Test is within (1, boolean).");
      Assert
        (The_Result   => Is_Within (2, A_Boolean),
         The_Expected => False,
         The_Test     => "Test is within (2, boolean).");

      Assert
        (The_Result   => Is_Within (-1, Universal_Integer),
         The_Expected => True,
         The_Test     => "Test is within (-1, universal integer).");
      Assert
        (The_Result   => Is_Within (0, Universal_Integer),
         The_Expected => True,
         The_Test     => "Test is within (0, universal integer).");
      Assert
        (The_Result   => Is_Within (1, Universal_Integer),
         The_Expected => True,
         The_Test     => "Test is within (1, universal integer).");

      Assert
        (The_Result   => Is_Within (-3, An_Integer),
         The_Expected => False,
         The_Test     => "Test is within (-2, integer).");
      Assert
        (The_Result   => Is_Within (-2, An_Integer),
         The_Expected => True,
         The_Test     => "Test is within (-1, integer).");
      Assert
        (The_Result   => Is_Within (0, An_Integer),
         The_Expected => True,
         The_Test     => "Test is within (0, integer).");
      Assert
        (The_Result   => Is_Within (1, An_Integer),
         The_Expected => True,
         The_Test     => "Test is within (1, integer).");
      Assert
        (The_Result   => Is_Within (2, An_Integer),
         The_Expected => False,
         The_Test     => "Test is within (2, integer).");

      Assert
        (The_Result   => Is_Within (-1, A_Modular),
         The_Expected => False,
         The_Test     => "Test is within (-1, modular).");
      Assert
        (The_Result   => Is_Within (0, A_Modular),
         The_Expected => True,
         The_Test     => "Test is within (0, modular).");
      Assert
        (The_Result   => Is_Within (1, A_Modular),
         The_Expected => True,
         The_Test     => "Test is within (1, modular).");
      Assert
        (The_Result   => Is_Within (4, A_Modular),
         The_Expected => False,
         The_Test     => "Test is within (4, modular).");

      Assert
        (The_Result   => Is_Within (0, An_Array),
         The_Expected => False,
         The_Test     => "Test is within (-1, array).");
      Assert
        (The_Result   => Is_Within (1, An_Array),
         The_Expected => True,
         The_Test     => "Test is within (1, array).");
      Assert
        (The_Result   => Is_Within (3, An_Array),
         The_Expected => True,
         The_Test     => "Test is within (3, array).");
      Assert
        (The_Result   => Is_Within (4, An_Array),
         The_Expected => False,
         The_Test     => "Test is within (4, array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Within;

   -------------------------
   -- Test_Is_Compatiable --
   -------------------------

   procedure Test_Is_Compatiable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Is_Compatiable (A_Boolean, null),
         The_Expected => True,
         The_Test     => "Test is compatiable (boolean, null).");
      Assert
        (The_Result   => Is_Compatiable (null, A_Boolean),
         The_Expected => True,
         The_Test     => "Test is compatiable (null, boolean).");

      Assert
        (The_Result   => Is_Compatiable (Universal_Boolean, A_Boolean),
         The_Expected => True,
         The_Test     => "Test is compatiable (universal boolean, boolean).");
      Assert
        (The_Result   => Is_Compatiable (A_Boolean, Universal_Boolean),
         The_Expected => True,
         The_Test     => "Test is compatiable (boolean, universal boolean).");

      Assert
        (The_Result   => Is_Compatiable (A_Boolean, The_Boolean),
         The_Expected => False,
         The_Test     => "Test is compatiable (boolean, boolean).");
      Assert
        (The_Result   => Is_Compatiable (A_Boolean, An_Integer),
         The_Expected => False,
         The_Test     => "Test is compatiable (boolean, integer).");
      Assert
        (The_Result   => Is_Compatiable (A_Boolean, A_Modular),
         The_Expected => False,
         The_Test     => "Test is compatiable (boolean, modular).");
      Assert
        (The_Result   => Is_Compatiable (A_Boolean, An_Array),
         The_Expected => False,
         The_Test     => "Test is compatiable (boolean, array).");

      Assert
        (The_Result   => Is_Compatiable (An_Integer, null),
         The_Expected => True,
         The_Test     => "Test is compatiable (integer, null).");
      Assert
        (The_Result   => Is_Compatiable (null, An_Integer),
         The_Expected => True,
         The_Test     => "Test is compatiable (null, integer).");

      Assert
        (The_Result   => Is_Compatiable (Universal_Integer, An_Integer),
         The_Expected => True,
         The_Test     => "Test is compatiable (universal integer, integer).");
      Assert
        (The_Result   => Is_Compatiable (An_Integer, Universal_Integer),
         The_Expected => True,
         The_Test     => "Test is compatiable (integer, universal integer).");

      Assert
        (The_Result   => Is_Compatiable (An_Integer, A_Boolean),
         The_Expected => False,
         The_Test     => "Test is compatiable (integer, boolean).");
      Assert
        (The_Result   => Is_Compatiable (An_Integer, The_Integer),
         The_Expected => False,
         The_Test     => "Test is compatiable (integer, integer).");
      Assert
        (The_Result   => Is_Compatiable (An_Integer, A_Modular),
         The_Expected => False,
         The_Test     => "Test is compatiable (integer, modular).");
      Assert
        (The_Result   => Is_Compatiable (An_Integer, An_Array),
         The_Expected => False,
         The_Test     => "Test is compatiable (integer, array).");

      Assert
        (The_Result   => Is_Compatiable (A_Modular, null),
         The_Expected => True,
         The_Test     => "Test is compatiable (modular, null).");
      Assert
        (The_Result   => Is_Compatiable (null, A_Modular),
         The_Expected => True,
         The_Test     => "Test is compatiable (null, modular).");

      Assert
        (The_Result   => Is_Compatiable (A_Modular, A_Boolean),
         The_Expected => False,
         The_Test     => "Test is compatiable (modular, boolean).");
      Assert
        (The_Result   => Is_Compatiable (A_Modular, An_Integer),
         The_Expected => False,
         The_Test     => "Test is compatiable (modular, integer).");
      Assert
        (The_Result   => Is_Compatiable (A_Modular, The_Modular),
         The_Expected => False,
         The_Test     => "Test is compatiable (modular, modular).");
      Assert
        (The_Result   => Is_Compatiable (A_Modular, An_Array),
         The_Expected => False,
         The_Test     => "Test is compatiable (modular, array).");

      Assert
        (The_Result   => Is_Compatiable (An_Array, null),
         The_Expected => True,
         The_Test     => "Test is compatiable (array, null).");
      Assert
        (The_Result   => Is_Compatiable (null, An_Array),
         The_Expected => True,
         The_Test     => "Test is compatiable (null, array).");

      Assert
        (The_Result   => Is_Compatiable (An_Array, A_Boolean),
         The_Expected => False,
         The_Test     => "Test is compatiable (array, boolean).");
      Assert
        (The_Result   => Is_Compatiable (An_Array, An_Integer),
         The_Expected => False,
         The_Test     => "Test is compatiable (array, integer).");
      Assert
        (The_Result   => Is_Compatiable (An_Array, A_Modular),
         The_Expected => False,
         The_Test     => "Test is compatiable (array, modular).");
      Assert
        (The_Result   => Is_Compatiable (An_Array, The_Array),
         The_Expected => False,
         The_Test     => "Test is compatiable (array, array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Is_Compatiable;

   ------------------
   -- Test_Best_Of --
   ------------------

   procedure Test_Best_Of
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Best_Of (Universal_Boolean, Boolean_Type),
         The_Expected => Boolean_Type,
         The_Test     => "Test best of (boolean, universal, boolean type).");
      Assert
        (The_Result   => Best_Of (Boolean_Type, Universal_Boolean),
         The_Expected => Boolean_Type,
         The_Test     => "Test best of (boolean type, universal boolean).");
      Assert
        (The_Result   => Best_Of (A_Boolean, Boolean_Type),
         The_Expected => A_Boolean,
         The_Test     => "Test best of (boolean, boolean type).");
      --        Assert
      --          (The_Result => Best_Of (Boolean_Type, A_Boolean),
      --           The_Expected => A_Boolean,
      --            The_Test     => "Test best of (boolean type, boolean).");

      Assert
        (The_Result   => Best_Of (Universal_Integer, Integer_Type),
         The_Expected => Integer_Type,
         The_Test     => "Test best of (universal integer, integer type).");
      Assert
        (The_Result   => Best_Of (Integer_Type, Universal_Integer),
         The_Expected => Integer_Type,
         The_Test     => "Test best of (integer type, universal integer).");
      Assert
        (The_Result   => Best_Of (An_Integer, Integer_Type),
         The_Expected => An_Integer,
         The_Test     => "Test best of (integer, integer type).");
      --        Assert
      --          (The_Result => Best_Of (Integer_Type, An_Integer),
      --           The_Expected => An_Integer,
      --            The_Test     => "Test best of (integer type, integer).");

      Assert
        (The_Result   => Best_Of (An_Array, A_Modular),
         The_Expected => An_Array,
         The_Test     => "Test best of (array, modular).");
      Assert
        (The_Result   => Best_Of (A_Modular, An_Array),
         The_Expected => A_Modular,
         The_Test     => "Test best of (modular, array).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Best_Of;

   --------------------------
   -- Test_Size_Of_Integer --
   --------------------------

   procedure Test_Size_Of_Integer
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Size_Of (0, 3),
         The_Expected => 2,
         The_Test     => "Test size of. (0, 3)");

      Assert
        (The_Result   => Size_Of (0, 10),
         The_Expected => 4,
         The_Test     => "Test size of. (0, 4)");

      Assert
        (The_Result   => Size_Of (-5, 3),
         The_Expected => 4,
         The_Test     => "Test size of. (-5, 3)");

      Assert
        (The_Result   => Size_Of (2, 4),
         The_Expected => 2,
         The_Test     => "Test size of. (2, 4)");

      Assert
        (The_Result   => Size_Of (-128, 127),
         The_Expected => 8,
         The_Test     => "Test size of. (-128, 127)");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Size_Of_Integer;

   --------------------------
   -- Test_Size_Of_Modular --
   --------------------------

   procedure Test_Size_Of_Modular
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result   => Size_Of (0),
         The_Expected => 1,
         The_Test     => "Test modular size of. (0)");

      Assert
        (The_Result   => Size_Of (1),
         The_Expected => 1,
         The_Test     => "Test modular size of. (1)");

      Assert
        (The_Result   => Size_Of (3),
         The_Expected => 2,
         The_Test     => "Test modular size of. (3)");

      Assert
        (The_Result   => Size_Of (8),
         The_Expected => 3,
         The_Test     => "Test modular size of. (8)");

      Assert
        (The_Result   => Size_Of (255),
         The_Expected => 8,
         The_Test     => "Test modular size of. (255)");

      Assert
        (The_Result   => Size_Of (128),
         The_Expected => 7,
         The_Test     => "Test size of. (128)");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Size_Of_Modular;

end Type_Package.Type_Test;

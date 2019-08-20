-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Test_Package; use Test_Package;
--

package body Identifier_Package.Identifier_Test is

   A_Package   : Identifier_Pointer;
   A_Procedure : Identifier_Pointer;
   A_Type      : Identifier_Pointer;
   A_Constant  : Identifier_Pointer;
   A_Variable  : Identifier_Pointer;
   An_Index     : Identifier_Pointer;
   A_Parameter : Identifier_Pointer;

   The_Unmarked_Identifier_Allocations : Natural;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("identifier_package.identifier_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(The_Test, Test_Is_Package'Access, "test_is_package!");
      Register_Routine(The_Test, Test_Is_Procedure'Access, "test_is_procedure!");
      Register_Routine(The_Test, Test_Is_Type'Access, "test_is_type!");
      Register_Routine(The_Test, Test_Is_Typed'Access, "test_is_typed!");
      Register_Routine(The_Test, Test_Is_Constant'Access, "test_is_constant!");
      Register_Routine(The_Test, Test_Is_Addressable'Access, "test_is_addressable!");
      Register_Routine(The_Test, Test_Is_Variable'Access, "test_is_variable!");
      Register_Routine(The_Test, Test_Is_Index'Access, "test_is_index!");
      Register_Routine(The_Test, Test_Is_Parameter'Access, "test_is_parameter!");
  end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      A_Package :=
        new Package_Identifier'(The_String => To_Unbounded_String ("PACKAGE"));
      A_Procedure :=
        new Procedure_Identifier'
          (The_String => To_Unbounded_String ("PROCEDUE"));
      A_Type :=
        new Type_Identifier'
          (The_String => To_Unbounded_String ("TYPE"), The_Type => null);
      A_Constant :=
        new Constant_Identifier'
          (The_String => To_Unbounded_String ("CONSTANT"),
           The_Type   => null,
           The_Value  => 0);
      A_Variable :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("VARIABLE"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      An_Index :=
        new Index_Identifier'
          (The_String  => To_Unbounded_String ("INDEX"),
           The_Type    => null,
           The_Address => 0);
      A_Parameter :=
        new Parameter_Identifier'
          (The_String  => To_Unbounded_String ("PARAMETER"),
           The_Type    => null,
           The_Address => 0,
           Is_In       => True,
           Is_Out      => True);

      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      null;
   end Tear_Down_Case;

   ---------------------
   -- Test_Is_Package --
   ---------------------

   procedure Test_Is_Package
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Package (A_Package),
         The_Expected => True,
         The_Test     => "Test is package (package).");

      Assert
        (The_Result => Is_Package (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is package (procedure).");

      Assert
        (The_Result => Is_Package (A_Type),
         The_Expected => False,
         The_Test     => "Test is package (type).");

      Assert
        (The_Result => Is_Package (A_Constant),
         The_Expected => False,
         The_Test     => "Test is package (constant).");

      Assert
        (The_Result => Is_Package (A_Variable),
         The_Expected => False,
         The_Test     => "Test is package (variable).");

      Assert
        (The_Result => Is_Package (An_Index),
         The_Expected => False,
         The_Test     => "Test is package (index).");

      Assert
        (The_Result => Is_Package (A_Parameter),
         The_Expected => False,
         The_Test     => "Test is package (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Package;

   -----------------------
   -- Test_Is_Procedure --
   -----------------------

   procedure Test_Is_Procedure
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Procedure (A_Package),
         The_Expected => False,
         The_Test     => "Test is procedure (package).");

      Assert
        (The_Result => Is_Procedure (A_Procedure),
         The_Expected => True,
         The_Test     => "Test is procedure (procedure).");

      Assert
        (The_Result => Is_Procedure (A_Type),
         The_Expected => False,
         The_Test     => "Test is procedure (type).");

      Assert
        (The_Result => Is_Procedure (A_Constant),
         The_Expected => False,
         The_Test     => "Test is procedure (constant).");

      Assert
        (The_Result => Is_Procedure (A_Variable),
         The_Expected => False,
         The_Test     => "Test is procedure (variable).");

      Assert
        (The_Result => Is_Procedure (An_Index),
         The_Expected => False,
         The_Test     => "Test is procedure (index).");

      Assert
        (The_Result => Is_Procedure (A_Parameter),
         The_Expected => False,
         The_Test     => "Test is procedure (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Procedure;

   ------------------
   -- Test_Is_Type --
   ------------------

   procedure Test_Is_Type
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Type (A_Package),
         The_Expected => False,
         The_Test     => "Test is type (package).");

      Assert
        (The_Result => Is_Type (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is type (procedure).");

      Assert
        (The_Result => Is_Type (A_Type),
         The_Expected => True,
         The_Test     => "Test is type (type).");

      Assert
        (The_Result => Is_Type (A_Constant),
         The_Expected => False,
         The_Test     => "Test is type (constant).");

      Assert
        (The_Result => Is_Type (A_Variable),
         The_Expected => False,
         The_Test     => "Test is type (variable).");

      Assert
        (The_Result => Is_Type (An_Index),
         The_Expected => False,
         The_Test     => "Test is type (index).");

      Assert
        (The_Result => Is_Type (A_Parameter),
         The_Expected => False,
         The_Test     => "Test is type (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Type;

   -------------------
   -- Test_Is_Typed --
   -------------------

   procedure Test_Is_Typed
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Typed (A_Package),
         The_Expected => False,
         The_Test     => "Test is typed (package).");

      Assert
        (The_Result => Is_Typed (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is typed (procedure).");

      Assert
        (The_Result => Is_Typed (A_Type),
         The_Expected => False,
         The_Test     => "Test is typed (type).");

      Assert
        (The_Result => Is_Typed (A_Constant),
         The_Expected => True,
         The_Test     => "Test is typed (constant).");

      Assert
        (The_Result => Is_Typed (A_Variable),
         The_Expected => True,
         The_Test     => "Test is typed (variable).");

      Assert
        (The_Result => Is_Typed (An_Index),
         The_Expected => True,
         The_Test     => "Test is typed (index).");

      Assert
        (The_Result => Is_Typed (A_Parameter),
         The_Expected => True,
         The_Test     => "Test is typed (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Typed;

   ----------------------
   -- Test_Is_Constant --
   ----------------------

   procedure Test_Is_Constant
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Constant (A_Package),
         The_Expected => False,
         The_Test     => "Test is constant (package).");

      Assert
        (The_Result => Is_Constant (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is constant (procedure).");

      Assert
        (The_Result => Is_Constant (A_Type),
         The_Expected => False,
         The_Test     => "Test is constant (type).");

      Assert
        (The_Result => Is_Constant (A_Constant),
         The_Expected => True,
         The_Test     => "Test is constant (constant).");

      Assert
        (The_Result => Is_Constant (A_Variable),
         The_Expected => False,
         The_Test     => "Test is constant (variable).");

      Assert
        (The_Result => Is_Constant (An_Index),
         The_Expected => False,
         The_Test     => "Test is constant (index).");

      Assert
        (The_Result => Is_Constant (A_Parameter),
         The_Expected => False,
         The_Test     => "Test is constant (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Constant;

   -------------------------
   -- Test_Is_Addressable --
   -------------------------

   procedure Test_Is_Addressable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Addressable (A_Package),
         The_Expected => False,
         The_Test     => "Test is addressable (package).");

      Assert
        (The_Result => Is_Addressable (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is addressable (procedure).");

      Assert
        (The_Result => Is_Addressable (A_Type),
         The_Expected => False,
         The_Test     => "Test is addressable (type).");

      Assert
        (The_Result => Is_Addressable (A_Constant),
         The_Expected => False,
         The_Test     => "Test is addressable (constant).");

      Assert
        (The_Result => Is_Addressable (A_Variable),
         The_Expected => True,
         The_Test     => "Test is addressable (variable).");

      Assert
        (The_Result => Is_Addressable (An_Index),
         The_Expected => True,
         The_Test     => "Test is addressable (index).");

      Assert
        (The_Result => Is_Addressable (A_Parameter),
         The_Expected => True,
         The_Test     => "Test is addressable (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Addressable;

   ----------------------
   -- Test_Is_Variable --
   ----------------------

   procedure Test_Is_Variable
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Variable (A_Package),
         The_Expected => False,
         The_Test     => "Test is variable (package).");

      Assert
        (The_Result => Is_Variable (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is variable (procedure).");

      Assert
        (The_Result => Is_Variable (A_Type),
         The_Expected => False,
         The_Test     => "Test is variable (type).");

      Assert
        (The_Result => Is_Variable (A_Constant),
         The_Expected => False,
         The_Test     => "Test is variable (constant).");

      Assert
        (The_Result => Is_Variable (A_Variable),
         The_Expected => True,
         The_Test     => "Test is variable (variable).");

      Assert
        (The_Result => Is_Variable (An_Index),
         The_Expected => False,
         The_Test     => "Test is variable (index).");

      Assert
        (The_Result => Is_Variable (A_Parameter),
         The_Expected => False,
         The_Test     => "Test is variable (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Variable;

   -------------------
   -- Test_Is_Index --
   -------------------

   procedure Test_Is_Index
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Index (A_Package),
         The_Expected => False,
         The_Test     => "Test is index (package).");

      Assert
        (The_Result => Is_Index (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is index (procedure).");

      Assert
        (The_Result => Is_Index (A_Type),
         The_Expected => False,
         The_Test     => "Test is index (type).");

      Assert
        (The_Result => Is_Index (A_Constant),
         The_Expected => False,
         The_Test     => "Test is index (constant).");

      Assert
        (The_Result => Is_Index (A_Variable),
         The_Expected => False,
         The_Test     => "Test is index (variable).");

      Assert
        (The_Result => Is_Index (An_Index),
         The_Expected => True,
         The_Test     => "Test is index (index).");

      Assert
        (The_Result => Is_Index (A_Parameter),
         The_Expected => False,
         The_Test     => "Test is index (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Index;

   -----------------------
   -- Test_Is_Parameter --
   -----------------------

   procedure Test_Is_Parameter
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert
        (The_Result => Is_Parameter (A_Package),
         The_Expected => False,
         The_Test     => "Test is parameter (package).");

      Assert
        (The_Result => Is_Parameter (A_Procedure),
         The_Expected => False,
         The_Test     => "Test is parameter (procedure).");

      Assert
        (The_Result => Is_Parameter (A_Type),
         The_Expected => False,
         The_Test     => "Test is parameter (type).");

      Assert
        (The_Result => Is_Parameter (A_Constant),
         The_Expected => False,
         The_Test     => "Test is parameter (constant).");

      Assert
        (The_Result => Is_Parameter (A_Variable),
         The_Expected => False,
         The_Test     => "Test is parameter (variable).");

      Assert
        (The_Result => Is_Parameter (An_Index),
         The_Expected => False,
         The_Test     => "Test is parameter (index).");

      Assert
        (The_Result => Is_Parameter (A_Parameter),
         The_Expected => True,
         The_Test     => "Test is parameter (parameter).");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
         The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Parameter;

end Identifier_Package.Identifier_Test;

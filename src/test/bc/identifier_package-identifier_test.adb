-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Unchecked_Deallocation;
--
with Ada.Text_IO;
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
   An_Index    : Identifier_Pointer;
   A_Parameter : Identifier_Pointer;

   The_Unmarked_Type_Allocations : Natural;
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
      Register_Routine(The_Test, Test_Dispose'Access, "test_dispose!");
      Register_Routine(The_Test, Test_Clear'Access, "test_clear!");
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
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);
   begin
      Ada.Text_IO.Put_Line("Identifier_Package.Identifier_Test");
      Ada.Text_IO.Put_Line
        ("Type_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Type_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Identifier_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations
           (Identifier_Package.The_Pool)));
      null;
   end Tear_Down_Case;

   ------------
   -- Set_Up --
   ------------

   overriding procedure Set_Up (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

      The_Type : Type_Pointer;
   begin
      --        The_Unmarked_Type_Allocations :=
      --          Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      --        The_Unmarked_Identifier_Allocations :=
      --          Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);
      --        Ada.Text_IO.Put_Line
      --          ("set up1: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));

      A_Package :=
        new Package_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String => To_Unbounded_String ("PACKAGE"));
      Identifier_Package.The_Last := A_Package;

      A_Procedure :=
        new Procedure_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String => To_Unbounded_String ("PROCEDUE"));
      Identifier_Package.The_Last := A_Procedure;

      The_Type := new Signed_Type'
        (The_Previous => Type_Package.The_Last,
         The_Base  => Integer_Type,
         The_First => -2,
         The_Last  => 1,
         The_Size  => 2);
      Type_Package.The_Last := The_Type;

      A_Type :=
        new Type_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String => To_Unbounded_String ("TYPE"),
           The_Type => The_Type);
      Identifier_Package.The_Last := A_Type;

      A_Constant :=
        new Constant_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String => To_Unbounded_String ("CONSTANT"),
           The_Type   => Type_Package.Integer_Type,
           The_Value  => 0);
      Identifier_Package.The_Last := A_Constant;

      A_Variable :=
        new Variable_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String  => To_Unbounded_String ("VARIABLE"),
           The_Type    => Type_Package.Integer_Type,
           The_Address => 0,
           The_Value   => 0);
      Identifier_Package.The_Last := A_Variable;

      An_Index :=
        new Index_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String  => To_Unbounded_String ("INDEX"),
           The_Type    => Type_Package.Integer_Type,
           The_Address => 0);
      Identifier_Package.The_Last := An_Index;

      A_Parameter :=
        new Parameter_Identifier'
          (The_Previous => Identifier_Package.The_Last,
           The_String  => To_Unbounded_String ("PARAMETER"),
           The_Type    => Type_Package.Integer_Type,
           The_Address => 0,
           Is_In       => True,
           Is_Out      => True);
      Identifier_Package.The_Last := A_Parameter;

      --        Ada.Text_IO.Put_Line
      --          ("set up2: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   overriding procedure Tear_Down (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      --        Ada.Text_IO.Put_Line
      --          ("tear down1: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));

      Identifier_Package.Clear;

      --        Ada.Text_IO.Put_Line
      --          ("tear down2: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));
      Type_Package.Clear;
   end Tear_Down;

   ------------------
   -- Test_Dispose --
   ------------------

   procedure Test_Dispose
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
      The_Identifier                      : Identifier_Pointer;
      The_Type                            : Type_Pointer;
   begin
      --        Ada.Text_IO.Put_Line
      --          ("test dispose 1: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

      The_Identifier :=
        new Package_Identifier'
          (The_Previous => null,
           The_String => To_Unbounded_String ("PACKAGE"));
      Dispose (The_Identifier);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (package).");

      The_Identifier :=
        new Procedure_Identifier'
          (The_Previous => null,
           The_String => To_Unbounded_String ("PROCEDUE"));
      Dispose (The_Identifier);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (procedure).");

      The_Type := new Signed_Type'
        (The_Previous => null,
         The_Base  => Integer_Type,
         The_First => -2,
         The_Last  => 1,
         The_Size  => 2);

      The_Identifier :=
        new Type_Identifier'
          (The_Previous => null,
           The_String => To_Unbounded_String ("TYPE"),
           The_Type => The_Type);
      Dispose (The_Identifier);
      Dispose (The_Type);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (type).");

      The_Identifier :=
        new Constant_Identifier'
          (The_Previous => null,
           The_String => To_Unbounded_String ("CONSTANT"),
           The_Type   => Integer_Type,
           The_Value  => 0);
      Dispose (The_Identifier);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (constant).");

      The_Identifier :=
        new Variable_Identifier'
          (The_Previous => null,
           The_String  => To_Unbounded_String ("VARIABLE"),
           The_Type    => Integer_Type,
           The_Address => 0,
           The_Value   => 0);
      Dispose (The_Identifier);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (variable).");

      The_Identifier :=
        new Index_Identifier'
          (The_Previous => null,
           The_String  => To_Unbounded_String ("INDEX"),
           The_Type    => Integer_Type,
           The_Address => 0);
      Dispose (The_Identifier);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (index).");

      The_Identifier :=
        new Parameter_Identifier'
          (The_Previous => null,
           The_String  => To_Unbounded_String ("PARAMETER"),
           The_Type    => Integer_Type,
           The_Address => 0,
           Is_In       => True,
           Is_Out      => True);
      Dispose (The_Identifier);

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test dispose (parameter).");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
   end Test_Dispose;

   ----------------
   -- Test_Clear --
   ----------------

   procedure Test_Clear
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      --        Ada.Text_IO.Put_Line
      --          ("test clear 1: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));

      Identifier_Package.Clear;
      --        Ada.Text_IO.Put_Line
      --          ("test clear 2: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Test clear.");

      Type_Package.Clear;

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      --        Ada.Text_IO.Put_Line
      --          ("test clear 3: " &
      --             SYSNatural'Image(Pool_Package.Unmarked_Allocations
      --             (Identifier_Package.The_Pool)));
   end Test_Clear;

   ---------------------
   -- Test_Is_Package --
   ---------------------

   procedure Test_Is_Package
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
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

      The_Unmarked_Type_Allocations       : Natural;
      The_Unmarked_Identifier_Allocations : Natural;
   begin
      The_Unmarked_Type_Allocations :=
        Pool_Package.Unmarked_Allocations (Type_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);

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
        (Pool_Package.Unmarked_Allocations (Type_Package.The_Pool) =
             The_Unmarked_Type_Allocations,
         "Incorrect type allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
   end Test_Is_Parameter;

end Identifier_Package.Identifier_Test;

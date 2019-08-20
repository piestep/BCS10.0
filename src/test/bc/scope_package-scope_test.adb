-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with AUnit.Assertions;
--
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO;            use Ada.Text_IO;
--
with Test_Package; use Test_Package;
--
with System_Package;     use System_Package;
with BC_Package;         use BC_Package;
with Identifier_Package; use Identifier_Package;
with Type_Package;       use Type_Package;
--
with Scope_Package.Dump_Package;
--

package body Scope_Package.Scope_Test is

   Variable_A       : Identifier_Pointer;
   Variable_B       : Identifier_Pointer;
   Variable_B_PRIME : Identifier_Pointer;
   Constant_C       : Identifier_Pointer;
   Type_D           : Identifier_Pointer;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("scope_package.scope_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(The_Test, Test_Scope'Access, "test_scope!");
  end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      Variable_A :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("VARIABLE_A"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Variable_B :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("VARIABLE_B"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Constant_C :=
        new Constant_Identifier'
          (The_String => To_Unbounded_String ("CONSTANT_C"),
           The_Type   => null,
           The_Value  => 0);
      Variable_B_PRIME :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("VARIABLE_B"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 1);
      Type_D :=
        new Type_Identifier'
          (The_String => To_Unbounded_String ("TYPE_A"), The_Type => null);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);
   begin
      null;

   end Tear_Down_Case;

   ----------------
   -- Test_Scope --
   ----------------

   procedure Test_Scope (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

   begin
      -- open scope 1
      Open;
      Scope_Package.Enter (Variable_A);

      -- assert scope 1
      Assert
        (The_Result   => Is_Identifier (Variable_A.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o1.1, a).");

      -- open scope 2
      Open;
      Scope_Package.Enter (Variable_B);
      Scope_Package.Enter (Constant_C);

      -- assert scope 2
      Assert
        (The_Result   => Is_Identifier (Constant_C.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o2.2, c).");
      Assert
        (The_Result   => Look_Up (Constant_C.The_String).The_String,
         The_Expected => Constant_C.The_String,
         The_Test     => "Test look up (o2.2, c).");

      Assert
        (The_Result   => Is_Identifier (Variable_B.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o2.2, b).");
      Assert
        (The_Result =>
           Variable_Identifier (Look_Up (Variable_B.The_String).all).The_Value,
         The_Expected => Variable_Identifier (Variable_B.all).The_Value,
         The_Test     => "Test look up (o2.2, b).");

      -- assert scope 1
      Assert
        (The_Result   => Is_Identifier (Variable_A.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o2.1, a).");
      Assert
        (The_Result   => Look_Up (Variable_A.The_String).The_String,
         The_Expected => Variable_A.The_String,
         The_Test     => "Test look up (o2.1, a).");

      -- open scope 3
      Open;
      Scope_Package.Enter (Variable_B_PRIME);
      Scope_Package.Enter (Type_D);

--          Scope_Package.Dump_Package.Dump;

      -- assert scope 3
      Assert
        (The_Result   => Is_Identifier (Type_D.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o3.3, d).");
      Assert
        (The_Result   => Look_Up (Type_D.The_String).The_String,
         The_Expected => Type_D.The_String,
         The_Test     => "Test look up (o3.3, d).");

      Assert
        (The_Result   => Is_Identifier (Variable_B.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o3.3, b).");
      Assert
        (The_Result   => Look_Up (Variable_B.The_String).The_String,
         The_Expected => Variable_B_PRIME.The_String,
         The_Test     => "Test look up (o3.3, b).");
      Assert
        (The_Result =>
           Variable_Identifier (Look_Up (Variable_B.The_String).all).The_Value,
         The_Expected => Variable_Identifier (Variable_B_PRIME.all).The_Value,
         The_Test     => "Test up (o3.3, b).");

      -- assert scope 2
      Assert
        (The_Result   => Is_Identifier (Constant_C.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (o3.2, c).");
      Assert
        (The_Result   => Look_Up (Constant_C.The_String).The_String,
         The_Expected => Constant_C.The_String,
         The_Test     => "Test look up (o3.2, c).");

      -- assert scope 1
      Assert
        (The_Result   => Is_Identifier (Variable_A.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (scope 1, a).");
      Assert
        (The_Result   => Look_Up (Variable_A.The_String).The_String,
         The_Expected => Variable_A.The_String,
         The_Test     => "Test look up (o3.1, a).");

      -- close scope 3
      Close;

      -- assert scope 2
      Assert
        (The_Result   => Is_Identifier (Constant_C.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (c3.2, c).");
      Assert
        (The_Result   => Look_Up (Constant_C.The_String).The_String,
         The_Expected => Constant_C.The_String,
         The_Test     => "Test look up (c3.2, c).");

      Assert
        (The_Result   => Is_Identifier (Variable_B.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (c3.2, b).");
      Assert
        (The_Result =>
           Variable_Identifier (Look_Up (Variable_B.The_String).all).The_Value,
         The_Expected => Variable_Identifier (Variable_B.all).The_Value,
         The_Test     => "Test look up (c3.2, b).");

      -- assert scope 1
      Assert
        (The_Result   => Is_Identifier (Variable_A.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (c3.1, a).");
      Assert
        (The_Result   => Look_Up (Variable_A.The_String).The_String,
         The_Expected => Variable_A.The_String,
         The_Test     => "Test look up (c3.1, a).");

      -- close scope 2
      Close;

      -- assert scope 1
      Assert
        (The_Result   => Is_Identifier (Variable_A.The_String),
         The_Expected => True,
         The_Test     => "Test is identifier (c2.1, a).");
      Assert
        (The_Result   => Look_Up (Variable_A.The_String).The_String,
         The_Expected => Variable_A.The_String,
         The_Test     => "Test look up (c2.1, a).");

      -- close scope 1
      Close;

   end Test_Scope;

end Scope_Package.Scope_Test;

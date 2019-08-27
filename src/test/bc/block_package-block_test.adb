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
with Pool_Package;
--
with Test_Package; use Test_Package;
--
with System_Package;     use System_Package;
with Identifier_Package; use Identifier_Package;
with Boolean_Package;    use Boolean_Package;
with Number_Package;     use Number_Package;
with Word_Package;       use Word_Package;
--
with Type_Package;

package body Block_Package.Block_Test is

   Identifier_A : Identifier_Pointer;
   Identifier_B : Identifier_Pointer;
   Identifier_C : Identifier_Pointer;
   Identifier_D : Identifier_Pointer;
   Identifier_E : Identifier_Pointer;

   The_Unmarked_Block_Allocations      : Natural;
   The_Unmarked_Identifier_Allocations : Natural;
   The_Unmarked_Number_Allocations     : Natural;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("block_package.block_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use Registration;
   begin
      Register_Routine(The_Test, Test_Simple_Block'Access, "test_simple_block!");
      Register_Routine(The_Test, Test_If_Then_Block'Access, "test_if_then_block!");
      Register_Routine(The_Test, Test_If_Then_Else_Block'Access, "test_if_then_else_block!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

   begin
      Identifier_A :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("A"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Identifier_B :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("B"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Identifier_C :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("C"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Identifier_D :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("D"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);
      Identifier_E :=
        new Variable_Identifier'
          (The_String  => To_Unbounded_String ("E"),
           The_Type    => null,
           The_Address => 0,
           The_Value   => 0);

      The_Unmarked_Block_Allocations :=
        Pool_Package.Unmarked_Allocations (Block_Package.The_Pool);
      The_Unmarked_Identifier_Allocations :=
        Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool);
      The_Unmarked_Number_Allocations :=
        Pool_Package.Unmarked_Allocations (Number_Package.The_Pool);
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   overriding procedure Tear_Down_Case (The_Test : in out Test) is
      pragma Unreferenced (The_Test);

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Identifier_Record'Class,
         Identifier_Pointer);

   begin
      Deallocate (Identifier_A);
      Deallocate (Identifier_B);
      Deallocate (Identifier_C);
      Deallocate (Identifier_D);
      Deallocate (Identifier_E);

      Ada.Text_IO.Put_Line("Identifier_Package.Identifier_Test");
      Ada.Text_IO.Put_Line
        ("Block_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Block_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Identifier_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool)));
      Ada.Text_IO.Put_Line
        ("Number_Allocations: " &
           SYSNatural'Image(Pool_Package.Unmarked_Allocations (Number_Package.The_Pool)));
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

   ----------------
   -- Test_Block --
   ----------------

   procedure Test_Simple_Block (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      Word_A       : Word_Type;
      Word_B       : Word_Type;
      Word_B_Prime : Word_Type;
      Word_C       : Word_Type;

   begin

      -- open block 1
      Open;

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_A, 2, 0);
      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_B, 2, 2);

      Assign (Identifier_A, Word_A);
      Assign (Identifier_B, Word_B);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (o1, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A),
         The_Test     => "Test look up (o1, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (o1, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B),
         The_Test     => "Test look up (o1, b).");

      -- open block 2
      Open;

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_B_Prime, 2, 4);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_C, 2, 6);

      Assign (Identifier_B, Word_B_Prime);
      Assign (Identifier_C, Word_C);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => False,
         The_Test     => "Test not is assigned (o2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (o2, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A),
         The_Test     => "Test look up (o2, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (o2, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B_Prime),
         The_Test     => "Test look up (o2, b).");

      Assert
        (The_Result   => Is_Assigned (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, c).");
      Assert
        (The_Result   => Is_Entry (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is entry (o2, c).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_C)),
         The_Expected => Boolean_Of (Word_C),
         The_Test     => "Test look up (o2, c).");

      -- close block 2
      Close;

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (c2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (c2, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A),
         The_Test     => "Test look up (c2, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (c2, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (c2, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B_Prime),
         The_Test     => "Test look up (c2, b).");

      -- close block 1
      Close;

      AUnit.Assertions.Assert (The_Table = null, "table not null.");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Block_Package.The_Pool) =
             The_Unmarked_Block_Allocations,
         "Incorrect block allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Number_Package.The_Pool) =
             The_Unmarked_Number_Allocations,
         "Incorrect number allocations.");
   end Test_Simple_Block;

   ------------------------
   -- Test_If_Then_Block --
   ------------------------

   procedure Test_If_Then_Block (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      A_Condition : Word_Type;
      Word_A      : Word_Type;
      Word_A_Then : Word_Type;
      Word_B      : Word_Type;
      Word_C      : Word_Type;
      Tmp         : Word_Type;
   begin
      -- 1 word/1 equations 1 numbers
      Create_Variable (A_Condition, 1, 0);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_A, 2, 1);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_A_Then, 2, 3);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_B, 2, 5);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_C, 2, 7);

      -- open block 1
      Open;
      Assign (Identifier_A, Word_A);
      Assign (Identifier_B, Word_B);

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (o1, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B),
         The_Test     => "Test look up (o1, b).");

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (o1, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A),
         The_Test     => "Test look up (o1, a).");

      -- open block 2
      Open;
      Assign (Identifier_A, Word_A_Then);
      Copy (Word_B, Tmp);
      Assign (Identifier_B, Tmp);
      Assign (Identifier_C, Word_C);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (o2, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A_Then),
         The_Test     => "Test look up (o2, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (o2, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B),
         The_Test     => "Test look up (o2, b).");

      Assert
        (The_Result   => Is_Assigned (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, c).");
      Assert
        (The_Result   => Is_Entry (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is entry (o2, c).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_C)),
         The_Expected => Boolean_Of (Word_C),
         The_Test     => "Test look up (o2, c).");

      -- close block 2
      Close (A_Condition);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (c2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (c2, a).");
      AUnit.Assertions.Assert
        (Boolean_Of (Look_Up (Identifier_A)) =
           " f( a0 b0 d0 ) 00100111 f( a0 c0 e0 ) 00100111",
         "Test look up (c2, a).");

      -- close block 1
      Close;

      AUnit.Assertions.Assert (The_Table = null, "table not null.");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Block_Package.The_Pool) =
             The_Unmarked_Block_Allocations,
         "Incorrect block allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Number_Package.The_Pool) =
             The_Unmarked_Number_Allocations,
         "Incorrect number allocations.");
   end Test_If_Then_Block;

   -----------------------------
   -- Test_If_Then_Else_Block --
   -----------------------------

   procedure Test_If_Then_Else_Block (The_Test : in out Test_Case'Class) is
      pragma Unreferenced (The_Test);

      A_Condition : Word_Type;
      Word_A      : Word_Type;
      Word_A_Then : Word_Type;
      Word_A_Else : Word_Type;
      Word_B      : Word_Type;
      Word_B_Then : Word_Type;
      Word_C      : Word_Type;
      Word_C_Else : Word_Type;
      Word_D      : Word_Type;
      Tmp         : Word_Type;

      The_Branch : Block;
   begin
      -- 1 word/1 equations 1 numbers
      Create_Variable (A_Condition, 1, 0);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_A, 2, 1);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_A_Then, 2, 3);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_A_Else, 2, 5);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_B, 2, 7);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_B_Then, 2, 9);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_C, 2, 11);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_C_Else, 2, 15);

      -- 1 word/2 equations 2 numbers
      Create_Variable (Word_D, 2, 15);

      -- open block 1
      Open;
      Assign (Identifier_A, Word_A);
      Assign (Identifier_B, Word_B);
      Assign (Identifier_C, Word_C);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (o1, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A),
         The_Test     => "Test look up (o1, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (o1, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B),
         The_Test     => "Test look up (o1, b).");

      Assert
        (The_Result   => Is_Assigned (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is assigned (o1, c).");
      Assert
        (The_Result   => Is_Entry (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is entry (o1, c).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_C)),
         The_Expected => Boolean_Of (Word_C),
         The_Test     => "Test look up (o1, c).");

      -- open block 2
      Open;
      Assign (Identifier_A, Word_A_Then);
      Assign (Identifier_B, Word_B_Then);
      Assign (Identifier_D, Word_D);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (o2, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A_Then),
         The_Test     => "Test look up (o2, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (o2, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => Boolean_Of (Word_B_Then),
         The_Test     => "Test look up (o2, b).");

      Assert
        (The_Result   => Is_Assigned (Identifier_D),
         The_Expected => True,
         The_Test     => "Test is assigned (o2, d).");
      Assert
        (The_Result   => Is_Entry (Identifier_D),
         The_Expected => True,
         The_Test     => "Test is entry (o2, d).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_D)),
         The_Expected => Boolean_Of (Word_D),
         The_Test     => "Test look up (o2, d).");

      -- branch block 2
      Branch (The_Branch);
      Assign (Identifier_A, Word_A_Else);
      Assign (Identifier_C, Word_C_Else);
      Copy (Word_D, Tmp);
      Assign (Identifier_D, Tmp);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (b2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (b2, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => Boolean_Of (Word_A_Else),
         The_Test     => "Test look up (b2, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is assigned (b2, c).");
      Assert
        (The_Result   => Is_Entry (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is entry (b2, c).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_C)),
         The_Expected => Boolean_Of (Word_C_Else),
         The_Test     => "Test look up (b2, c).");

      Assert
        (The_Result   => Is_Assigned (Identifier_D),
         The_Expected => True,
         The_Test     => "Test is assigned (b2, d).");
      Assert
        (The_Result   => Is_Entry (Identifier_D),
         The_Expected => True,
         The_Test     => "Test is entry (b2, d).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_D)),
         The_Expected => Boolean_Of (Word_D),
         The_Test     => "Test look up (b2, d).");

      -- close block 2
      Close (A_Condition, The_Branch);

      Assert
        (The_Result   => Is_Assigned (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is assigned (c2, a).");
      Assert
        (The_Result   => Is_Entry (Identifier_A),
         The_Expected => True,
         The_Test     => "Test is entry (c2, a).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_A)),
         The_Expected => " f( a0 d0 f0 ) 00011011 f( a0 e0 g0 ) 00011011",
         The_Test     => "Test look up (c2, a).");

      Assert
        (The_Result   => Is_Assigned (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is assigned (c2, b).");
      Assert
        (The_Result   => Is_Entry (Identifier_B),
         The_Expected => True,
         The_Test     => "Test is entry (c2, b).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_B)),
         The_Expected => " f( a0 h0 j0 ) 00100111 f( a0 i0 k0 ) 00100111",
         The_Test     => "Test look up (c2, b).");

      Assert
        (The_Result   => Is_Assigned (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is assigned (c2, c).");
      Assert
        (The_Result   => Is_Entry (Identifier_C),
         The_Expected => True,
         The_Test     => "Test is entry (c2, c).");
      Assert
        (The_Result   => Boolean_Of (Look_Up (Identifier_C)),
         The_Expected => " f( a0 l0 p0 ) 00011011 f( a0 m0 q0 ) 00011011",
         The_Test     => "Test look up (c2, c).");

      -- close block 1
      Close;

      AUnit.Assertions.Assert (The_Table = null, "table not null.");

      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Block_Package.The_Pool) =
             The_Unmarked_Block_Allocations,
         "Incorrect block allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Identifier_Package.The_Pool) =
             The_Unmarked_Identifier_Allocations,
         "Incorrect identifier allocations.");
      AUnit.Assertions.Assert
        (Pool_Package.Unmarked_Allocations (Number_Package.The_Pool) =
             The_Unmarked_Number_Allocations,
         "Incorrect number allocations.");
   end Test_If_Then_Else_Block;

end Block_Package.Block_Test;

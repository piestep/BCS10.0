pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Ada.Text_IO; use Ada.Text_IO;
--
with Test_Package; use Test_Package;
--
with Pool_Package;
--
with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
with Number_Package;   use Number_Package;
with Word_Package;     use Word_Package;
--

package body Word_Test is

   DEFAULT_DISPLAY_TEST_NOT                   : Boolean := False;
   DEFAULT_DISPLAY_TEST_OR                    : Boolean := False;
   DEFAULT_DISPLAY_TEST_AND                   : Boolean := False;
   DEFAULT_DISPLAY_TEST_NOT_EQUAL             : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_LESS_THAN    : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_LESS_THAN      : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_GREATER_THAN : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_GREATER_THAN   : Boolean := False;
   DEFAULT_DISPLAY_TEST_NEGATE                : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_ADD          : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_ADD            : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_SUBTRACT     : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_SUBTRACT       : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_MULTIPLY     : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_MULTIPLY       : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_DIVIDE       : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_DIVIDE         : Boolean := False;
   DEFAULT_DISPLAY_TEST_UNSIGNED_REMAINDER    : Boolean := False;
   DEFAULT_DISPLAY_TEST_SIGNED_REMAINDER      : Boolean := False;
   DEFAULT_DISPLAY_TEST_ASSIGN_ELEMENT        : Boolean := False;
   DEFAULT_DISPLAY_TEST_ACCESS_ELEMENT        : Boolean := False;
   DEFAULT_DISPLAY_TEST_IF_ELSE               : Boolean := False;

   DEFAULT_VARIABLE_SIZE        : constant := 3;
   DEFAULT_SIGNED_VARIABLE_SIZE : constant := 5;
   DEFAULT_INDEX_SIZE           : constant := 3;
   DEFAULT_ELEMENT_SIZE         : constant := 3;

   -- Unmaked allocations.

   The_Unmarked_Number_Allocations : SYSNatural;
   The_Unmarked_Word_Allocations   : SYSNatural;

   ----------
   -- Name --
   ----------

   overriding function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format("Word_Test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine(The_Test, Test_Not'Access, "Test_Not!");
      Register_Routine(The_Test, Test_And'Access, "Test_And!");
      Register_Routine(The_Test, Test_Or'Access, "Test_Or!");
      Register_Routine(The_Test, Test_Xor'Access, "Test_Xor!");
      Register_Routine(The_Test, Test_Not_Equal'Access, "Test_Not_Equal!");
      Register_Routine(The_Test, Test_Unsigned_Less_Than'Access, "Test_Unsigned_Less_Than!");
      Register_Routine(The_Test, Test_Signed_Less_Than'Access, "Test_Signed_Less_Than!");
      Register_Routine(The_Test, Test_Unsigned_Greater_Than'Access, "Test_Unsigned_Greater_Than!");
      Register_Routine(The_Test, Test_Signed_Greater_Than'Access, "Test_Signed_Greater_Than!");
      Register_Routine(The_Test, Test_Negate'Access, "Test_Negate!");
      Register_Routine(The_Test, Test_Unsigned_Add'Access, "Test_Unsigned_Add!");
      Register_Routine(The_Test, Test_Signed_Add'Access, "Test_Signed_Add!");
      Register_Routine(The_Test, Test_Unsigned_Subtract'Access, "Test_Unsigned_Subtract!");
      Register_Routine(The_Test, Test_Signed_Subtract'Access, "Test_Signed_Subtract!");
      Register_Routine(The_Test, Test_Unsigned_Multiply'Access, "Test_Unsigned_Multiply!");
      Register_Routine(The_Test, Test_Signed_Multiply'Access, "Test_Signed_Multiply!");
      Register_Routine(The_Test, Test_Unsigned_Divide'Access, "Test_Unsigned_Divide!");
      Register_Routine(The_Test, Test_Signed_Divide'Access, "Test_Signed_Divide!");
      Register_Routine(The_Test, Test_Unsigned_Remainder'Access, "Test_Unsigned_Remainder!");
      Register_Routine(The_Test, Test_Signed_Remainder'Access, "Test_Signed_Remainder!");
      Register_Routine(The_Test, Test_Assign_Element'Access, "Test_Assign_Element!");
      Register_Routine(The_Test, Test_Access_Element'Access, "Test_Access_Element!");
      Register_Routine(The_Test, Test_If_Else'Access, "Test_If_Else!");
   end Register_Tests;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (The_Test : in out Test) is
   begin
      The_Unmarked_Number_Allocations :=
        Pool_Package.Unmarked_Allocations (Number_Package.The_Pool);
      The_Unmarked_Word_Allocations :=
        Pool_Package.Unmarked_Allocations (Word_Package.The_Pool);
   end Set_Up_Case;

   ----------------------------
   -- Test_Unsigned_Unary_Op --
   ----------------------------

   generic
      The_Title : String;
      with procedure Boolean_Op (The_Left : Word_Type);
      with function Unary_Op (I : in BCModular) return BCModular;
   function Test_Unsigned_Unary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean;

   -- Not

   function Test_Unsigned_Unary_Op
     (Dump     : Boolean;
      The_Size : Positive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass       : Boolean := True;
      The_Right  : Word_Type;
      The_Values : Boolean_Array_Type (0 .. The_Size - 1);
      The_Result : Boolean_Array_Type (0 .. The_Size - 1);
      The_Mask   : BCModular;

      function Op (The_Right : in BCModular) return BCModular is
      begin
         return Unary_Op (The_Right) and
           BCModular_At
             (0,
              The_Size,
              Boolean_Array_Type'(0 .. The_Size - 1 => True));
      end Op;

   begin
      if Dump then
         Ada.Text_IO.Put_Line (The_Title);
         Ada.Text_IO.Put_Line ("<operand>   <result>   <expected>");
      end if;

      Create_Variable (The_Right, The_Size, 0);

      Boolean_Op (The_Right);

      The_Mask :=
        BCModular_At
          (0,
           The_Size,
           Boolean_Array_Type'(0 .. The_Size - 1 => True));

      for I in BCModular range 0 .. 2**(The_Size) - 1 loop

         The_Values (0 .. The_Size - 1) :=
           BCModular_To_Array (I) (0 .. The_Size - 1);

         if Op (I) >= 0 and Op (I) <= 2**The_Size - 1 then

            The_Result := Solve (The_Right, The_Values);

            if Dump or Op (I) /= BCModular_At (0, The_Size, The_Result) then
               Ada.Text_IO.Put
                 (BCModular'Image (I) &     -- operand
                    " (" &
                    Image_Of (The_Values) &  -- operand
                    ")   " &
                    BCModular'Image
                    (BCModular_At (0, The_Size, The_Result)) &     -- result
                    " (" &
                    Image_Of (The_Result) &  -- result
                    ")   " &
                    BCModular'Image (Op (I)) & -- expected
                    " (" &
                    Image_Of
                    (BCModular_To_Array (Op (I))
                     (0 .. The_Size - 1)) & -- expected
                    ")");

               if Op (I) /= BCModular_At (0, The_Size, The_Result) then
                  Pass := False;
                  Ada.Text_IO.Put (" *");
               end if;
               New_Line;
            end if;
         end if;
      end loop;
      Dispose (The_Right);

      return Pass;
   end Test_Unsigned_Unary_Op;

   -----------------------------
   -- Test_Unsigned_Binary_Op --
   -----------------------------

   generic
      The_Title     : String;
      Not_Zero_Left : Boolean := False;
      with procedure Boolean_Op
        (The_Left  : in out Word_Type;
         The_Right : in     Word_Type);
      with function Binary_Op (I, J : in BCModular) return BCModular;
   function Test_Unsigned_Binary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean;

   -- And, Or, Xor
   -- unsigned + - * / rem

   function Test_Unsigned_Binary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass       : Boolean := True;
      The_Left   : Word_Type;
      The_Right  : Word_Type;
      The_Values : Boolean_Array_Type (0 .. 2 * The_Size - 1);
      The_Result : Boolean_Array_Type (0 .. The_Size - 1);

      function Op (The_Left, The_Right : in BCModular) return BCModular is
      begin
         return Binary_Op (The_Left, The_Right) and
           BCModular_At
             (0,
              The_Size,
              Boolean_Array_Type'(0 .. The_Size - 1 => True));
      end Op;
   begin
      if Dump then
         Ada.Text_IO.Put_Line (The_Title);
         Ada.Text_IO.Put_Line ("<operand> <operand>   <result>   <expected>");
      end if;

      Create_Variable (The_Left, The_Size, 0);
      Create_Variable (The_Right, The_Size, Variable_Type (The_Size));

      Boolean_Op (The_Left, The_Right);
      Dispose (The_Right);

      for I in BCModular range 0 .. 2**The_Size - 1 loop
         for J in BCModular range 0 .. 2**The_Size - 1 loop
            if not (Not_Zero_Left and J = 0)
              and then (Op (I, J) <= 2**The_Size - 1)
            then

               The_Values (0 .. The_Size - 1) :=
                 BCModular_To_Array (I) (0 .. The_Size - 1);
               The_Values (The_Size .. 2 * The_Size - 1) :=
                 BCModular_To_Array (J) (0 .. The_Size - 1);

               The_Result := Solve (The_Left, The_Values);

               if Dump or
                 Op (I, J) /= BCModular_At (0, The_Size, The_Result)
               then
                  Ada.Text_IO.Put
                    (BCModular'Image (I) &  -- operand a
                       " (" &
                       Image_Of (The_Values (0 .. The_Size - 1)) &  -- operand a
                       ") " &
                       BCModular'Image (J) &  -- operand b
                       " (" &
                       Image_Of
                       (The_Values
                            (The_Size .. 2 * The_Size - 1)) & -- operand b
                       ")   " &
                       BCModular'Image
                       (BCModular_At (0, The_Size, The_Result)) & -- result
                       " (" &
                       Image_Of (The_Result) &      -- result
                       ")   " &
                       BCModular'Image (Op (I, J)) & -- expected
                       " (" &
                       Image_Of
                       (BCModular_To_Array (Op (I, J))
                        (0 .. The_Size - 1)) &        -- expected
                       ")");

                  if Op (I, J) /= BCModular_At (0, The_Size, The_Result) then
                     Pass := False;
                     Ada.Text_IO.Put (" **");
                  end if;
                  New_Line;
               end if;
            end if;
         end loop;
      end loop;
      Dispose (The_Left);

      return Pass;
   end Test_Unsigned_Binary_Op;

   ---------------------------------
   -- Test_Unsigned_Comparison_Op --
   ---------------------------------

   generic
      The_Title : String;
      with procedure Boolean_Op
        (The_Left  : in out Word_Type;
         The_Right : in     Word_Type);
      with function Binary_Op (I, J : in BCModular) return Boolean;
   function Test_Unsigned_Comparison_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean;

   -- unsigned < >

   function Test_Unsigned_Comparison_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass       : Boolean := True;
      The_Left   : Word_Type;
      The_Right  : Word_Type;
      The_Values : Boolean_Array_Type (0 .. 2 * The_Size - 1);
      The_Result : Boolean_Array_Type (0 .. 0);
   begin
      if Dump then
         Ada.Text_IO.Put_Line (The_Title);
         Ada.Text_IO.Put_Line ("<operand> <operand>   <result>   <expected>");
      end if;

      Create_Variable (The_Left, The_Size, 0);
      Create_Variable (The_Right, The_Size, Variable_Type (The_Size));

      Boolean_Op (The_Left, The_Right);
      Dispose (The_Right);

      for I in BCModular range 0 .. 2**The_Size - 1 loop
         for J in BCModular range 0 .. 2**The_Size - 1 loop

            The_Values (0 .. The_Size - 1) :=
              BCModular_To_Array (I) (0 .. The_Size - 1);
            The_Values (The_Size .. 2 * The_Size - 1) :=
              BCModular_To_Array (J) (0 .. The_Size - 1);

            The_Result := Solve (The_Left, The_Values);

            if Dump or
              Boolean'Pos (Binary_Op (I, J)) /=
                Integer'Value (Image_Of (The_Result))
            then
               Ada.Text_IO.Put
                 (BCModular'Image (I) &  -- operand a
                    " (" &
                    Image_Of (The_Values (0 .. The_Size - 1)) &  -- operand a
                    ") " &
                    BCModular'Image (J) &  -- operand b
                    " (" &
                    Image_Of
                    (The_Values (The_Size .. 2 * The_Size - 1)) & -- operand b
                    ")   " &
                    BCModular'Image (BCModular_At (0, 1, The_Result)) & -- result
                    " (" &
                    Image_Of (The_Result) &      -- result
                    ")   " &
                    Integer'Image (Boolean'Pos (Binary_Op (I, J))) -- expected
                 );

               if Boolean'Pos (Binary_Op (I, J)) /=
                 Integer'Value (Image_Of (The_Result))
               then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;
               New_Line;
            end if;
         end loop;
      end loop;
      Dispose (The_Left);

      return Pass;
   end Test_Unsigned_Comparison_Op;

   -------------------------------
   -- Test_Signed_Comparison_Op --
   -------------------------------

   generic
      The_Title : String;
      with procedure Boolean_Op
        (The_Left  : in out Word_Type;
         The_Right : in     Word_Type);
      with function Binary_Op (I, J : in Integer) return Boolean;

      -- signed < >

   function Test_Signed_Comparison_Op
     (display  : Boolean;
      The_Size : Positive := DEFAULT_VARIABLE_SIZE) return Boolean;

   function Test_Signed_Comparison_Op
     (display  : Boolean;
      The_Size : Positive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass       : Boolean := True;
      The_Left   : Word_Type;
      The_Right  : Word_Type;
      The_Values : Boolean_Array_Type (0 .. 2 * The_Size - 1);
      The_Result : Boolean_Array_Type (0 .. 0);
   begin
      if display then
         Ada.Text_IO.Put_Line (The_Title);
         Ada.Text_IO.Put_Line ("<operand> <operand>   <result>   <expected>");
      end if;

      Create_Variable (The_Left, The_Size, 0);
      Create_Variable (The_Right, The_Size, Variable_Type (The_Size));

      Boolean_Op (The_Left, The_Right);
      Dispose (The_Right);

      for I in -2**(The_Size - 1) .. 2**(The_Size - 1) - 1 loop
         for J in -2**(The_Size - 1) .. 2**(The_Size - 1) - 1 loop

            The_Values (0 .. The_Size - 1) :=
              BCInteger_To_Array (I) (0 .. The_Size - 1);
            The_Values (The_Size .. 2 * The_Size - 1) :=
              BCInteger_To_Array (J) (0 .. The_Size - 1);

            The_Result := Solve (The_Left, The_Values);

            if display or
              Boolean'Pos (Binary_Op (I, J)) /=
                Integer'Value (Image_Of (The_Result))
            then
               Ada.Text_IO.Put
                 (BCInteger'Image (I) &  -- operand a
                    " (" &
                    Image_Of (The_Values (0 .. The_Size - 1)) &  -- operand a
                    ") " &
                    BCInteger'Image (J) &  -- operand b
                    " (" &
                    Image_Of
                    (The_Values (The_Size .. 2 * The_Size - 1)) & -- operand b
                    ")   " &
                    BCModular'Image (BCModular_At (0, 1, The_Result)) & -- result
                    " (" &
                    Image_Of (The_Result) &      -- result
                    ")   " &
                    Integer'Image (Boolean'Pos (Binary_Op (I, J))) -- expected
                 );

               if Boolean'Pos (Binary_Op (I, J)) /=
                 Integer'Value (Image_Of (The_Result))
               then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;
               New_Line;
            end if;
         end loop;
      end loop;
      Dispose (The_Left);

      return Pass;
   end Test_Signed_Comparison_Op;

   ----------------------------
   -- Signed Unary Operation --
   ----------------------------

   generic
      The_Title : String;
      with procedure Boolean_Op (The_Left : Word_Type);
      with function Unary_Op (I : in BCInteger) return BCInteger;
   function Test_Signed_Unary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean;

   -- negate

   function Test_Signed_Unary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass       : Boolean := True;
      The_Right  : Word_Type;
      The_Values : Boolean_Array_Type (0 .. The_Size - 1);
      The_Result : Boolean_Array_Type (0 .. The_Size - 1);
   begin
      if Dump then
         Ada.Text_IO.Put_Line (The_Title);
         Ada.Text_IO.Put_Line ("<operand>   <result>   <expected>");
      end if;

      Create_Variable (The_Right, The_Size, 0);
      Boolean_Op (The_Right);

      for I in BCInteger range -2**(The_Size - 1) .. 2**(The_Size - 1) - 1 loop

         The_Values (0 .. The_Size - 1) :=
           BCInteger_To_Array (I) (0 .. The_Size - 1);

         if Unary_Op (I) >= -2**(The_Size - 1) and
           Unary_Op (I) <= 2**(The_Size - 1) - 1
         then

            The_Result := Solve (The_Right, The_Values);

            if Dump or
              Unary_Op (I) /= BCInteger_At (0, The_Size, The_Result)
            then
               Ada.Text_IO.Put
                 (BCInteger'Image (I) &     -- operand
                    " (" &
                    Image_Of (The_Values) &  -- operand
                    ")   " &
                    BCInteger'Image
                    (BCInteger_At (0, The_Size, The_Result)) &     -- result
                    " (" &
                    Image_Of (The_Result) &  -- result
                    ")   " &
                    BCInteger'Image (Unary_Op (I)) & -- expected
                    " (" &
                    Image_Of
                    (BCInteger_To_Array (Unary_Op (I))
                     (0 .. The_Size - 1)) & -- expected
                    ")");

               if Unary_Op (I) /= BCInteger_At (0, The_Size, The_Result) then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;
               New_Line;
            end if;
         end if;
      end loop;
      Dispose (The_Right);

      return Pass;
   end Test_Signed_Unary_Op;

   ---------------------------
   -- Test_Signed_Binary_Op --
   ---------------------------

   generic
      The_Title     : String;
      Not_Zero_Left : Boolean := False;
      with procedure Boolean_Op
        (The_Left  : in out Word_Type;
         The_Right : in     Word_Type);
      with function Binary_Op (I, J : in Integer) return Integer;

      -- signed + - * / rem

   function Test_Signed_Binary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean;

   function Test_Signed_Binary_Op
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass       : Boolean := True;
      The_Left   : Word_Type;
      The_Right  : Word_Type;
      The_Values : Boolean_Array_Type (0 .. 2 * The_Size - 1);
      The_Result : Boolean_Array_Type (0 .. The_Size - 1);
   begin
      if Dump then
         Ada.Text_IO.Put_Line (The_Title);
         Ada.Text_IO.Put_Line ("<operand> <operand>   <result>   <expected>");
      end if;

      Create_Variable (The_Left, The_Size, 0);
      Create_Variable (The_Right, The_Size, Variable_Type (The_Size));

      Boolean_Op (The_Left, The_Right);
      Dispose (The_Right);

      for I in BCInteger range -2**(The_Size - 1) .. 2**(The_Size - 1) - 1 loop
         for J in BCInteger range -2**(The_Size - 1) .. 2**(The_Size - 1) - 1
         loop

            if not (Not_Zero_Left and J = 0)
              and then
                (Binary_Op (I, J) >= -2**(The_Size - 1) and
                       Binary_Op (I, J) <= 2**(The_Size - 1) - 1)
            then

               The_Values (0 .. The_Size - 1) :=
                 BCInteger_To_Array (I) (0 .. The_Size - 1);
               The_Values (The_Size .. 2 * The_Size - 1) :=
                 BCInteger_To_Array (J) (0 .. The_Size - 1);

               The_Result := Solve (The_Left, The_Values);

               if Dump or
                 Binary_Op (I, J) /= BCInteger_At (0, The_Size, The_Result)
               then
                  Ada.Text_IO.Put
                    (BCInteger'Image (I) &  -- operand a
                       " (" &
                       Image_Of (The_Values (0 .. The_Size - 1)) &  -- operand a
                       ") " &
                       BCInteger'Image (J) &  -- operand b
                       " (" &
                       Image_Of
                       (The_Values
                            (The_Size .. 2 * The_Size - 1)) & -- operand b
                       ")   " &
                       BCInteger'Image
                       (BCInteger_At (0, The_Size, The_Result)) & -- result
                       " (" &
                       Image_Of (The_Result) &      -- result
                       ")   " &
                       BCInteger'Image (Binary_Op (I, J)) & -- expected
                       " (" &
                       Image_Of
                       (BCInteger_To_Array (Binary_Op (I, J))
                        (0 .. The_Size - 1)) &        -- expected
                       ")");

                  if Binary_Op (I, J) /=
                    BCInteger_At (0, The_Size, The_Result)
                  then
                     Pass := False;
                     Ada.Text_IO.Put (" **");
                  end if;
                  New_Line;
               end if;
            end if;
         end loop;
      end loop;
      Dispose (The_Left);

      return Pass;
   end Test_Signed_Binary_Op;

   -------------------------
   -- Test_Assign_Element --
   -------------------------

   function Test_Assign_Element
     (Dump         : Boolean;
      Index_Size   : SYSPositive := DEFAULT_VARIABLE_SIZE;
      Element_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is

      ARRAY_SIZE : constant SYSPositive := (2**Index_Size) * Element_Size;
      Pass       : Boolean              := True;
      The_Values : Boolean_Array_Type
        (0 .. ARRAY_SIZE + Index_Size + Element_Size - 1);
      The_Array    : Word_Type;
      The_Index    : Word_Type;
      The_Element  : Word_Type;
      The_Result   : Boolean_Array_Type (0 .. ARRAY_SIZE - 1);
      The_Expected : Boolean_Array_Type (0 .. ARRAY_SIZE - 1);
   begin
      if Dump then
         Ada.Text_IO.Put_Line ("Assign Element");
         Ada.Text_IO.Put_Line
           ("<array> <index> <element>   <result>   <expected>");
      end if;

      Create_Variable (The_Array, ARRAY_SIZE, 0);
      Create_Variable (The_Index, Index_Size, Variable_Type (ARRAY_SIZE));
      Create_Variable
        (The_Element,
         Element_Size,
         Variable_Type (ARRAY_SIZE + Index_Size));

      Assign_Element (The_Array, The_Element, The_Index);

      for I in BCModular range 0 .. 2**Index_Size - 1 loop
         for J in BCModular range 0 .. 2**Element_Size - 1 loop
            if J < 2**(Element_Size - 1) then
               The_Values (0 .. ARRAY_SIZE - 1) :=
                 (0 .. ARRAY_SIZE - 1 => True);
            else
               The_Values (0 .. ARRAY_SIZE - 1) :=
                 (0 .. ARRAY_SIZE - 1 => False);
            end if;

            for K in 0 .. 2**Index_Size - 1 loop
               if K = Integer (I) then
                  The_Expected
                    (K * Element_Size .. K * Element_Size + Element_Size - 1) :=
                    BCModular_To_Array (J) (0 .. Element_Size - 1);
               else
                  The_Expected
                    (K * Element_Size .. K * Element_Size + Element_Size - 1) :=
                    The_Values (0 .. Element_Size - 1);
               end if;

            end loop;

            The_Values (ARRAY_SIZE .. ARRAY_SIZE + Index_Size - 1) :=
              BCModular_To_Array (I) (0 .. Index_Size - 1);

            The_Values
              (ARRAY_SIZE + Index_Size ..
                 ARRAY_SIZE + Index_Size + Element_Size - 1) :=
                BCModular_To_Array (J) (0 .. Element_Size - 1);

            The_Result := Solve (The_Array, The_Values);
            if Dump or The_Expected /= The_Result then
               Ada.Text_IO.Put
                 (Image_Of (The_Values (0 .. ARRAY_SIZE - 1)) &
                    " " &
                    BCModular'Image (I) &
                    " (" &
                    Image_Of
                    (The_Values (ARRAY_SIZE .. ARRAY_SIZE + Index_Size - 1)) &
                    ") " &
                    BCModular'Image (J) &
                    " (" &
                    Image_Of
                    (The_Values
                         (ARRAY_SIZE + Index_Size ..
                              ARRAY_SIZE + Index_Size + Element_Size - 1)) &
                      ")   " &
                    Image_Of (The_Result) &
                    "   " &
                    Image_Of (The_Expected));
               if The_Expected /= The_Result then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;
               New_Line;
            end if;
         end loop;
      end loop;
      Dispose (The_Array);
      Dispose (The_Index);
      Dispose (The_Element);

      return Pass;
   end Test_Assign_Element;

   -------------------------
   -- Test_Access_Element --
   -------------------------

   function Test_Access_Element
     (Dump         : Boolean;
      Index_Size   : SYSPositive := DEFAULT_VARIABLE_SIZE;
      Element_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      ARRAY_SIZE : constant SYSPositive := (2**Index_Size) * Element_Size;
      Pass       : Boolean              := True;
      The_Values : Boolean_Array_Type
        (0 .. ARRAY_SIZE + Index_Size + Element_Size - 1);
      The_Array  : Word_Type;
      The_Index  : Word_Type;
      The_Result : Boolean_Array_Type (0 .. Element_Size - 1);
   begin
      if Dump then
         Ada.Text_IO.Put_Line ("Test_Access_Element");
         Ada.Text_IO.Put_Line ("<array> <index> <size>  <result>   <expected>");
      end if;

      Create_Variable (The_Array, ARRAY_SIZE, 0);
      Create_Variable (The_Index, Index_Size, Variable_Type (ARRAY_SIZE));

      Access_Element (The_Array, The_Index, Element_Size);

      for I in BCModular range 0 .. 2**Index_Size - 1 loop
         for J in BCModular range 0 .. 2**Element_Size - 1 loop

            for K in 0 .. 2**Index_Size - 1 loop
               if K = Integer (I) then
                  The_Values
                    (K * Element_Size .. K * Element_Size + Element_Size - 1) :=
                    BCModular_To_Array (J) (0 .. Element_Size - 1);
               else
                  if J < 2**(Element_Size - 1) then
                     The_Values
                       (K * Element_Size ..
                          K * Element_Size + Element_Size - 1) :=
                         (0 .. Element_Size - 1 => True);
                  else
                     The_Values
                       (K * Element_Size ..
                          K * Element_Size + Element_Size - 1) :=
                         (0 .. Element_Size - 1 => False);
                  end if;
               end if;

            end loop;

            The_Values (ARRAY_SIZE .. ARRAY_SIZE + Index_Size - 1) :=
              BCModular_To_Array (I) (0 .. Index_Size - 1);

            The_Result := Solve (The_Array, The_Values);
            if Dump or J /= BCModular_At (0, Element_Size, The_Result) then
               Ada.Text_IO.Put
                 (Image_Of (The_Values (0 .. ARRAY_SIZE - 1)) &
                    " " &
                    BCModular'Image (I) &
                    " (" &
                    Image_Of
                    (The_Values (ARRAY_SIZE .. ARRAY_SIZE + Index_Size - 1)) &
                    ") " &
                    SYSPositive'Image (Element_Size) &
                    "   " &
                    BCModular'Image (BCModular_At (0, Element_Size, The_Result)) &
                    " (" &
                    Image_Of (The_Result) &
                    ")   " &
                    BCModular'Image (J) &
                    " (" &
                    Image_Of (BCModular_To_Array (J) (0 .. Element_Size - 1)) &
                    ")   ");

               if J /= BCModular_At (0, Element_Size, The_Result) then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;
               New_Line;
            end if;
         end loop;
      end loop;
      Dispose (The_Array);
      Dispose (The_Index);

      return Pass;
   end Test_Access_Element;

   ------------------
   -- Test_If_Else --
   ------------------

   function Test_If_Else
     (Dump     : Boolean;
      The_Size : SYSPositive := DEFAULT_VARIABLE_SIZE) return Boolean
   is
      Pass          : Boolean := True;
      The_Then      : Word_Type;
      The_Else      : Word_Type;
      The_Condition : Word_Type;
      The_Values    : Boolean_Array_Type (0 .. 1 + 2 * The_Size - 1);
      The_Result    : Boolean_Array_Type (0 .. The_Size - 1);
   begin
      if Dump then
         Ada.Text_IO.Put_Line ("Test_If_Else");
         Ada.Text_IO.Put_Line
           ("<condition> <then> <else>   <result>   <expected>");
      end if;

      Create_Variable (The_Condition, 1, 0);
      Create_Variable (The_Then, The_Size, 1);
      Create_Variable (The_Else, The_Size, Variable_Type (The_Size + 1));

      If_Else (The_Condition, The_Then, The_Else);

      for I in False .. True loop
         for J in BCModular (0) .. 2**The_Size - 1 loop
            The_Values (0 .. 0) := Boolean_Array_Type'(0 .. 0 => I);

            The_Values (1 .. The_Size) :=
              BCModular_To_Array (J) (0 .. The_Size - 1);

            The_Values (The_Size + 1 .. 2 * The_Size) :=
              BCModular_To_Array ((2**The_Size - 1) - J) (0 .. The_Size - 1);

            The_Result := Solve (The_Then, The_Values);

            if I then
               if Dump or J /= BCModular_At (0, The_Size, The_Result) then
                  Ada.Text_IO.Put
                    (Boolean'Image (I) &
                       " " &
                       Image_Of (The_Values (0 .. 0)) &
                       " " &
                       BCModular'Image (J) &
                       " (" &
                       Image_Of (The_Values (1 .. The_Size)) &
                       ") " &
                       BCModular'Image ((2**The_Size - 1) - J) &
                       " (" &
                       Image_Of (The_Values (The_Size + 1 .. 2 * The_Size)) &
                       ")   " &
                       BCModular'Image (BCModular_At (0, The_Size, The_Result)) &
                       " (" &
                       Image_Of (The_Result) &
                       ")   " &
                       BCModular'Image (J) &
                       " (" &
                       Image_Of (BCModular_To_Array (J) (0 .. The_Size - 1)) &
                       ")   ");
               end if;

               if J /= BCModular_At (0, The_Size, The_Result) then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;

               New_Line;

            else

               if Dump or
                 ((2**The_Size - 1) - J) /=
                   BCModular_At (0, The_Size, The_Result)
               then
                  Ada.Text_IO.Put
                    (Boolean'Image (I) &
                       " " &
                       Image_Of (The_Values (0 .. 0)) &
                       " " &
                       BCModular'Image (J) &
                       " (" &
                       Image_Of (The_Values (1 .. The_Size)) &
                       ") " &
                       BCModular'Image ((2**The_Size - 1) - J) &
                       " (" &
                       Image_Of (The_Values (The_Size + 1 .. 2 * The_Size)) &
                       ")   " &
                       BCModular'Image (BCModular_At (0, The_Size, The_Result)) &
                       " (" &
                       Image_Of (The_Result) &
                       ")   " &
                       BCModular'Image (((2**The_Size - 1) - J)) &
                       " (" &
                       Image_Of
                       (BCModular_To_Array (((2**The_Size - 1) - J))
                        (0 .. The_Size - 1)) &
                       ")   ");
               end if;

               if ((2**The_Size - 1) - J) /=
                 BCModular_At (0, The_Size, The_Result)
               then
                  Pass := False;
                  Ada.Text_IO.Put (" **");
               end if;

               New_Line;
            end if;
         end loop;
      end loop;
      Dispose (The_Condition);
      Dispose (The_Then);
      Dispose (The_Else);

      return Pass;
   end Test_If_Else;

   --------------
   -- Test_Not --
   --------------

   procedure Test_Not (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      pragma Warnings (Off);
      function Test_Not is new Test_Unsigned_Unary_Op
        (The_Title  => "Not",
         Unary_Op   => "not",
         Boolean_Op => Not_Op);
      pragma Warnings (On);

   begin
      Assert
        (The_Result   => Test_Not (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result not.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Not;

   --------------
   -- Test_And --
   --------------

   procedure Test_And (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      function Test_And is new Test_Unsigned_Binary_Op
        (The_Title  => "And",
         Binary_Op  => "and",
         Boolean_Op => And_Op);
   begin
      Assert
        (The_Result   => Test_And (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result and.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_And;

   -------------
   -- Test_Or --
   -------------

   procedure Test_Or (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      function Test_Or is new Test_Unsigned_Binary_Op
        (The_Title  => "Or",
         Binary_Op  => "or",
         Boolean_Op => Or_Op);
   begin
      Assert
        (The_Result   => Test_Or (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result or.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Or;

   --------------
   -- Test_Xor --
   --------------

   procedure Test_Xor (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      function Test_Xor is new Test_Unsigned_Binary_Op
        (The_Title  => "Xor",
         Binary_Op  => "xor",
         Boolean_Op => Xor_Op);
   begin
      Assert
        (The_Result   => Test_Xor (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result xor.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Xor;

   --------------------
   -- Test_Not_Equal --
   --------------------

   procedure Test_Not_Equal
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Not_Equal is new Test_Unsigned_Comparison_Op
        (The_Title  => "Not_Equal",
         Binary_Op  => "/=",
         Boolean_Op => Not_Equal);
   begin
      Assert
        (The_Result   => Test_Not_Equal (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result not equal.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Not_Equal;

   -----------------------------
   -- Test_Unsigned_Less_Than --
   -----------------------------

   procedure Test_Unsigned_Less_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Less_Than is new Test_Unsigned_Comparison_Op
        (The_Title  => "Unsigned_Less_Than",
         Binary_Op  => "<",
         Boolean_Op => Unsigned_Less_Than);
      -- (anbn + AnBn) (unsigned_less_than) + -- if both 1 or both 0 Anbn -- if an
      -- 0 and bn 1
   begin
      Assert
        (The_Result   => Test_Unsigned_Less_Than (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned less than.");
      --          Assert
      --             (Test_Unsigned_Less_Than (DEFAULT_DISPLAY_TEST_UNSIGNED_LESS_THAN),
      --              "Incorrect result unsigned less than");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Less_Than;

   ---------------------------
   -- Test_Signed_Less_Than --
   ---------------------------

   procedure Test_Signed_Less_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Less_Than is new Test_Signed_Comparison_Op
        (The_Title  => "Signed_Less_Than",
         Binary_Op  => "<",
         Boolean_Op => Signed_Less_Than);
      -- (anbn + AnBn) (unsigned_less_then) + -- if both 1 or both 0 a2B2 -- if an
      -- 1 and bn 0
   begin
      Assert
        (The_Result   => Test_Signed_Less_Than (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result signed less than.");
      --          Assert
      --             (Test_Signed_Less_Than (DEFAULT_DISPLAY_TEST_SIGNED_LESS_THAN),
      --              "Incorrect result signed less than");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Less_Than;

   --------------------------------
   -- Test_Unsigned_Greater_Than --
   --------------------------------

   procedure Test_Unsigned_Greater_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Greater_Than is new Test_Unsigned_Comparison_Op
        (The_Title  => "Unsigned_Greater_Than",
         Binary_Op  => ">",
         Boolean_Op => Unsigned_Greater_Than);
      -- (anbn + AnBn) (unsigned_greater_than) + -- if both 1 or both 0 Anbn -- if
      -- an 0 and bn 1
   begin
      Assert
        (The_Result   => Test_Unsigned_Greater_Than (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned greater than.");
      --          Assert
      --             (Test_Unsigned_Greater_Than
      --                 (DEFAULT_DISPLAY_TEST_UNSIGNED_GREATER_THAN),
      --              "Incorrect result unsigned greater than");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Greater_Than;

   ------------------------------
   -- Test_Signed_Greater_Than --
   ------------------------------

   procedure Test_Signed_Greater_Than
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Greater_Than is new Test_Signed_Comparison_Op
        (The_Title  => "Signed_Greater_Than",
         Binary_Op  => ">",
         Boolean_Op => Signed_Greater_Than);
      -- (anbn + AnBn) (unsigned_greater_then) + -- if both 1 or both 0 Anbn -- if
      -- an 0 and bn 1
   begin
      Assert
        (The_Result   => Test_Signed_Greater_Than (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned greater than.");
      --          Assert
      --             (Test_Signed_Greater_Than (DEFAULT_DISPLAY_TEST_SIGNED_GREATER_THAN),
      --              "Incorrect result Signed less than");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Greater_Than;

   -----------------
   -- Test_Negate --
   -----------------

   procedure Test_Negate (The_Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (The_Test);

      function Test_Negate is new Test_Signed_Unary_Op
        (The_Title  => "Negate",
         Unary_Op   => "-",
         Boolean_Op => Negate);
   begin
      Assert
        (The_Result   => Test_Negate (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result negate.");
      --          Assert
      --             (Test_Negate (DEFAULT_DISPLAY_TEST_NEGATE),
      --              "Incorrect result negate");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Negate;

   -----------------------
   -- Test_Unsigned_Add --
   -----------------------

   procedure Test_Unsigned_Add
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Add is new Test_Unsigned_Binary_Op
        (The_Title  => "Unsigned_Add",
         Binary_Op  => "+",
         Boolean_Op => Add);
   begin
      Assert
        (The_Result   => Test_Unsigned_Add (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned add.");
      --          Assert
      --             (Test_Unsigned_Add (DEFAULT_DISPLAY_TEST_UNSIGNED_ADD),
      --              "Incorrect result unsigned add");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Add;

   ---------------------
   -- Test_Signed_Add --
   ---------------------

   procedure Test_Signed_Add
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Add is new Test_Signed_Binary_Op
        (The_Title  => "Signed_Add",
         Binary_Op  => "+",
         Boolean_Op => Add);
   begin
      Assert
        (The_Result   => Test_Signed_Add (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result signed add.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Add;

   ----------------------------
   -- Test_Unsigned_Subtract --
   ----------------------------

   procedure Test_Unsigned_Subtract
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Subtract is new Test_Unsigned_Binary_Op
        (The_Title  => "Unsigned_Subtract",
         Binary_Op  => "-",
         Boolean_Op => Subtract);
   begin
      Assert
        (The_Result   => Test_Unsigned_Subtract (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned subtract.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Subtract;

   --------------------------
   -- Test_Signed_Subtract --
   --------------------------

   procedure Test_Signed_Subtract
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Subtract is new Test_Signed_Binary_Op
        (The_Title  => "Signed_Subtract",
         Binary_Op  => "-",
         Boolean_Op => Subtract);
   begin
      Assert
        (The_Result   => Test_Signed_Subtract (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result signed subtract.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Subtract;

   ----------------------------
   -- Test_Unsigned_Multiply --
   ----------------------------

   procedure Test_Unsigned_Multiply
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Multiply is new Test_Unsigned_Binary_Op
        (The_Title  => "Unsigned_Multiply",
         Binary_Op  => "*",
         Boolean_Op => Multiply);
   begin
      Assert
        (The_Result   => Test_Unsigned_Multiply (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned multiply.");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Multiply;

   --------------------------
   -- Test_Signed_Multiply --
   --------------------------

   procedure Test_Signed_Multiply
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Multiply is new Test_Signed_Binary_Op
        (The_Title  => "Signed_Multiply",
         Binary_Op  => "*",
         Boolean_Op => Multiply);
   begin
      Assert
        (The_Result =>
           Test_Signed_Multiply (Word_Dump, DEFAULT_SIGNED_VARIABLE_SIZE),
         The_Expected => True,
         The_Test     => "Incorrect result signed multiply");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Multiply;

   --------------------------
   -- Test_Unsigned_Divide --
   --------------------------

   procedure Test_Unsigned_Divide
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Divide is new Test_Unsigned_Binary_Op
        (The_Title     => "Unsigned_Divide",
         Not_Zero_Left => True,
         Binary_Op     => "/",
         Boolean_Op    => Unsigned_Divide);
   begin
      Assert
        (The_Result   => Test_Unsigned_Divide (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned divide");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Divide;

   ------------------------
   -- Test_Signed_Divide --
   ------------------------

   procedure Test_Signed_Divide
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Divide is new Test_Signed_Binary_Op
        (The_Title     => "Signed_Divide",
         Not_Zero_Left => True,
         Binary_Op     => "/",
         Boolean_Op    => Signed_Divide);
   begin
      Assert
        (The_Result =>
           Test_Signed_Divide (Word_Dump, DEFAULT_SIGNED_VARIABLE_SIZE),
         The_Expected => True,
         The_Test     => "Incorrect result signed divide");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Divide;

   -----------------------------
   -- Test_Unsigned_Remainder --
   -----------------------------

   procedure Test_Unsigned_Remainder
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Unsigned_Remainder is new Test_Unsigned_Binary_Op
        (The_Title     => "Unsigned_Remainder",
         Not_Zero_Left => True,
         Binary_Op     => "rem",
         Boolean_Op    => Unsigned_Remainder);
   begin
      Assert
        (The_Result   => Test_Unsigned_Remainder (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect result unsigned remainder");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Unsigned_Remainder;

   ---------------------------
   -- Test_Signed_Remainder --
   ---------------------------

   procedure Test_Signed_Remainder
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

      function Test_Signed_Remainder is new Test_Signed_Binary_Op
        (The_Title     => "Signed_Remainder",
         Not_Zero_Left => True,
         Binary_Op     => "rem",
         Boolean_Op    => Signed_Remainder);
   begin
      Assert
        (The_Result =>
           Test_Signed_Remainder (Word_Dump, DEFAULT_SIGNED_VARIABLE_SIZE),
         The_Expected => True,
         The_Test     => "Incorrect result signed remainder");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Signed_Remainder;

   -------------------------
   -- Test_Assign_Element --
   -------------------------

   procedure Test_Assign_Element
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);
   begin
      Assert
        (The_Result =>
           Test_Assign_Element
             (Word_Dump,
              DEFAULT_INDEX_SIZE,
              DEFAULT_ELEMENT_SIZE),
         The_Expected => True,
         The_Test     => "Incorrect result assign element");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Assign_Element;

   -------------------------
   -- Test_Access_Element --
   -------------------------

   procedure Test_Access_Element
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);
   begin
      Assert
        (The_Result =>
           Test_Access_Element
             (Word_Dump,
              DEFAULT_INDEX_SIZE,
              DEFAULT_ELEMENT_SIZE),
         The_Expected => True,
         The_Test     => "Incorrect result access element");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_Access_Element;

   ------------------
   -- Test_If_Else --
   ------------------

   procedure Test_If_Else
     (The_Test : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (The_Test);
   begin
      Assert
        (The_Result   => Test_If_Else (Word_Dump),
         The_Expected => True,
         The_Test     => "Incorrect if else");

      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Number_Package.The_Pool),
         The_Expected => The_Unmarked_Number_Allocations,
         The_Test     => "Incorrect number allocations.");
      Assert
        (The_Result =>
           Pool_Package.Unmarked_Allocations (Word_Package.The_Pool),
         The_Expected => The_Unmarked_Word_Allocations,
         The_Test     => "Incorrect word allocations.");
   end Test_If_Else;

end Word_Test;

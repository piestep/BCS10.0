pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with System.Storage_Elements;
--
with Term_Package; use Term_Package;
--

package body Word_Package with
Spark_Mode => Off is

   -- Deallocate the boolean word.

   procedure Deallocate (The_Word : in out Word_Type) is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Word_Array_Type,
         Word_Type);
   begin
      if The_Word /= EMPTY_WORD then
         Unchecked_Deallocation (The_Word);
      end if;
   end Deallocate;

   -- Use a mask to apply an index equation to a boolean equation.

   procedure Index_Equation
     (The_Equation : in out Equation_Type;
      The_Index    :        Word_Type;
      The_Mask     :        BCInteger_Boolean_Array_Type);

   procedure Index_Equation
     (The_Equation : in out Equation_Type;
      The_Index    :        Word_Type;
      The_Mask     :        BCInteger_Boolean_Array_Type)
   is
      Tmp1 : Equation_Type;
   begin
      for L in 0 .. Length_Of (The_Index) - 1 loop

         if The_Mask (L) then
            And_Op (The_Equation, The_Index (L));

         else
            Copy (The_Index (L), Tmp1);
            Not_Op (Tmp1);
            And_Op (The_Equation, Tmp1);

            Dispose (Tmp1);
         end if;

      end loop;
   end Index_Equation;

   -- Return set of variables for boolean word.

   function Variables_Of (The_Word : Word_Type) return Variable_Set_Type is
      The_Set : Variable_Set_Type := EMPTY_SET;
   begin
      for I in The_Word'Range loop
         The_Set := The_Set or Variables_Of (The_Word (I));
      end loop;
      return The_Set;
   end Variables_Of;

   -- Create the boolean word with the length. NOTE: dose not have to be valid.

   procedure Create (The_Word : out Word_Type; The_Length : Word_Length_Type) is
   begin
      The_Word := new Word_Array_Type (0 .. The_Length - 1);
   end Create;

   -- Create a boolean word with the boolean equation.

   procedure Create (The_Word : out Word_Type; The_Equation : Equation_Type) is
   begin
      Create (The_Word, 1);
      The_Word (0) := The_Equation;
   end Create;

   -- Create an empty boolean word.

   procedure Create (The_Word : out Word_Type) is
   begin
      Create (The_Word, 0);
   end Create;

   -- Create a bw constant value from the boolean array.

   procedure Create_Constant
     (The_Word  : out Word_Type;
      The_Value :     Boolean_Array_Type)
   is
   begin
      Create (The_Word, The_Value'Length);

      for I in The_Word'Range loop
         Create (The_Word (I), The_Value (I + The_Value'First));
      end loop;
   end Create_Constant;

   -- Create a bw constant value of size from the integer.

   procedure Create_Constant
     (The_Word   : out Word_Type;
      The_Length :     Word_Length_Type;
      The_Value  :     BCModular)
   is
   begin
      Create_Constant
        (The_Word,
         BCModular_To_Array (The_Value) (0 .. The_Length - 1));
   end Create_Constant;

   -- Create a bw of variable boolean equations, f(n) = n, staring with the
   -- variable.

   procedure Create_Variable
     (The_Word     : out Word_Type;
      The_Length   :     Word_Length_Type;
      The_Variable :     Variable_Type)
   is
   begin
      Create (The_Word, The_Length);

      for I in The_Word'Range loop
         Create (The_Word (I), The_Variable + I);
      end loop;
   end Create_Variable;

   -- Dispose the word.

   procedure Dispose (The_Word : in out Word_Type) is
   begin
      if The_Word /= EMPTY_WORD then
         for I in The_Word'Range loop
            Dispose (The_Word (I));
         end loop;
         Deallocate (The_Word);
      end if;
   end Dispose;

   -- Copy the word.

   procedure Copy (From_The_Word : Word_Type; To_The_Word : out Word_Type) is
   begin
      Create (To_The_Word, Length_Of (From_The_Word));

      for I in From_The_Word'Range loop
         Copy (From_The_Word (I), To_The_Word (I));
      end loop;
   end Copy;

   -- Append the boolean equation to a word.

   procedure Append
     (To_The_Word  : in out Word_Type;
      The_Equation :        Equation_Type)
   is
      The_Result : Word_Type;
   begin
      if Length_Of (To_The_Word) = 0 then
         Create (The_Result, 1);
         Copy (The_Equation, The_Result (0));
      else
         Create (The_Result, Length_Of (To_The_Word) + 1);

         The_Result (0 .. Length_Of (To_The_Word) - 1) :=
           To_The_Word (0 .. Length_Of (To_The_Word) - 1);

         Copy (The_Equation, The_Result (Length_Of (The_Result) - 1));
      end if;

      Deallocate (To_The_Word);
      To_The_Word := The_Result;
   end Append;

   -- Append the word to a word.

   procedure Append (To_The_Word : in out Word_Type; The_Word : Word_Type) is
      The_Result : Word_Type;
   begin
      if Length_Of (To_The_Word) = 0 then
         Create (The_Result, Length_Of (The_Word));

         for I in The_Word'Range loop
            Copy (The_Word (I), The_Result (I));
         end loop;

      else
         Create (The_Result, Length_Of (To_The_Word) + Length_Of (The_Word));

         The_Result (0 .. Length_Of (To_The_Word) - 1) :=
           To_The_Word (0 .. Length_Of (To_The_Word) - 1);

         for I in 0 .. Length_Of (The_Word) - 1 loop
            Copy (The_Word (I), The_Result (I + Length_Of (To_The_Word)));
         end loop;
      end if;

      Deallocate (To_The_Word);
      To_The_Word := The_Result;
   end Append;

   -- Fill the word of size with the boolean equation.

   procedure Fill
     (The_Word          : out Word_Type;
      Of_The_Length     :     Word_Length_Type;
      With_The_Equation :     Equation_Type)
   is
   begin
      Create (The_Word, Of_The_Length);

      for I in The_Word'Range loop
         Copy (With_The_Equation, The_Word (I));
      end loop;
   end Fill;

   -- Shift the word left and return in the parameter.

   procedure Shift_Left (The_Word : Word_Type) is
   begin
      Dispose (The_Word (Length_Of (The_Word) - 1));

      for I in reverse 1 .. Length_Of (The_Word) - 1 loop
         The_Word (I) := The_Word (I - 1);
      end loop;

      Create (The_Word (The_Word'First), False);
   end Shift_Left;

   -- Shift the word right and return in the parameter.

   procedure Shift_Right (The_Word : Word_Type) is
   begin
      Dispose (The_Word (0));

      for I in 1 .. Length_Of (The_Word) - 1 loop
         The_Word (I - 1) := The_Word (I);
      end loop;
      Create (The_Word (Length_Of (The_Word) - 1), False);
   end Shift_Right;

   -- Not the word and return in the parameter.

   procedure Not_Op (The_Word : Word_Type) is
   begin
      for I in The_Word'Range loop
         Not_Op (The_Word (I));
      end loop;
   end Not_Op;

   -- And the words and return the result in the first parameter.

   procedure And_Op (The_Word : in out Word_Type; With_The_Word : Word_Type) is
   begin
      for I in The_Word'Range loop
         And_Op (The_Word (I), With_The_Word (I));
      end loop;
   end And_Op;

   -- Or the words and return the result in the first parameter.

   procedure Or_Op (The_Word : in out Word_Type; With_The_Word : Word_Type) is
   begin
      for I in The_Word'Range loop
         Or_Op (The_Word (I), With_The_Word (I));
      end loop;
   end Or_Op;

   -- Xor the words and return the result in the first parameter.

   procedure Xor_Op (The_Word : in out Word_Type; With_The_Word : Word_Type) is
   begin
      for I in The_Word'Range loop
         Xor_Op (The_Word (I), With_The_Word (I));
      end loop;
   end Xor_Op;

   -- Return the result of applying the condition to the words, f(a) = c | a & C
   -- | b.

   procedure If_Else
     (The_Condition : Word_Type;
      The_Word      : Word_Type;
      And_The_Word  : Word_Type)
   is
      Tmp1 : Equation_Type;
      Tmp2 : Equation_Type;
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop
         And_Op (The_Word (I), The_Condition (0));

         Copy (The_Condition (0), Tmp1);
         Not_Op (Tmp1);

         Copy (And_The_Word (I), Tmp2);
         And_Op (Tmp2, Tmp1);

         Dispose (Tmp1);

         Or_Op (The_Word (I), Tmp2);

         Dispose (Tmp2);
      end loop;
   end If_Else;

   -- Return the result of not equal between the words and return the result in
   -- the first parameter.

   procedure Not_Equal
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      The_Result : Word_Type;
   begin
      Create_Constant (The_Result, 1, 0);

      for I in The_Word'Range loop

         Xor_Op (The_Word (I), With_The_Word (I));
         Or_Op (The_Result (0), The_Word (I));
      end loop;

      Dispose (The_Word);
      The_Word := The_Result;
   end Not_Equal;

   procedure Unsigned_Less_Than
     (The_Result    : out Equation_Type;
      The_Word      :     Word_Array_Type;
      With_The_Word :     Word_Array_Type)
   is
      Tmp1 : Equation_Type;
      Tmp2 : Equation_Type;
      Tmp3 : Equation_Type;
      -- A2b2 + (A2B2 + a2b2)(A1b1 + (A1B1 + a1b1)(A0b0))
   begin
      -- Ab
      Copy (With_The_Word (The_Word'First), Tmp1);

      Copy (The_Word (0), The_Result);
      Not_Op (The_Result);
      And_Op (The_Result, Tmp1);

      Dispose (Tmp1);

      for I in 1 .. The_Word'Last loop
         -- AB
         Copy (With_The_Word (I), Tmp1);
         Not_Op (Tmp1);

         Copy (The_Word (I), Tmp2);
         Not_Op (Tmp2);
         And_Op (Tmp1, Tmp2);

         Dispose (Tmp2);

         -- ab
         Copy (With_The_Word (I), Tmp2);
         Copy (The_Word (I), Tmp3);
         And_Op (Tmp2, Tmp3);

         Dispose (Tmp3);

         -- AB + ab
         Or_Op (Tmp1, Tmp2);

         Dispose (Tmp2);

         -- (AB + ab)(-)
         And_Op (The_Result, Tmp1);

         Dispose (Tmp1);

         -- Ab
         Copy (The_Word (I), Tmp1);
         Not_Op (Tmp1);
         And_Op (Tmp1, With_The_Word (I));

         -- Ab + (AB + ab)(-)
         Or_Op (The_Result, Tmp1);

         Dispose (Tmp1);
      end loop;
   end Unsigned_Less_Than;

   -- Return the result of less then between the words and return the result in
   -- the first parameter.

   procedure Unsigned_Less_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      The_Result : Equation_Type;
      -- A2b2 + (A2B2 + a2b2)(A1b1 + (A1B1 + a1b1)(A0b0))
   begin
      Unsigned_Less_Than
        (The_Result,
         The_Word (0 .. Length_Of (The_Word) - 1),
         With_The_Word (0 .. Length_Of (With_The_Word) - 1));

      Dispose (The_Word);

      Create (The_Word, The_Result);
   end Unsigned_Less_Than;

   -- Return the result of less then between the words and return the result in
   -- the first parameter.

   procedure Signed_Less_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      -- (AnBn + anbn)(unsigned less than (a0 .. an-1)) + -- if both 0 or 1 anBn
      -- -- if an 1 and bn 0

      Tmp1       : Equation_Type;
      Tmp2       : Equation_Type;
      Tmp3       : Equation_Type;
      The_Result : Equation_Type;
   begin
      Copy (With_The_Word (Length_Of (With_The_Word) - 1), Tmp1); -- bn
      Not_Op (Tmp1); -- Bn

      Copy (The_Word (Length_Of (The_Word) - 1), The_Result); -- an
      And_Op (The_Result, Tmp1); -- anB

      Dispose (Tmp1);

      if Length_Of (The_Word) > 1 then
         -- AnBn if a and b both 0
         Copy (The_Word (Length_Of (The_Word) - 1), Tmp1);
         Not_Op (Tmp1); -- An

         Copy (With_The_Word (Length_Of (With_The_Word) - 1), Tmp2);
         Not_Op (Tmp2); -- Bn

         And_Op (Tmp1, Tmp2);

         Dispose (Tmp2);

         -- anan if a and b both 1
         Copy (The_Word (Length_Of (The_Word) - 1), Tmp2); -- an
         Copy (With_The_Word (Length_Of (With_The_Word) - 1), Tmp3); -- bn
         And_Op (Tmp2, Tmp3);

         Dispose (Tmp3);

         Or_Op (Tmp1, Tmp2); -- AnBn + anbn

         Dispose (Tmp2);

         Unsigned_Less_Than
           (Tmp2,
            The_Word (0 .. Length_Of (The_Word) - 2),
            With_The_Word (0 .. Length_Of (With_The_Word) - 2));

         And_Op
           (Tmp1,
            Tmp2); -- (AnBn + anbn) (unsigned greater_than of (0..n-1))

         Dispose (Tmp2);

         Or_Op (The_Result, Tmp1);

         Dispose (Tmp1);
      end if;

      Dispose (The_Word);
      Create (The_Word, The_Result);
   end Signed_Less_Than;

   procedure Unsigned_Greater_Than
     (The_Result    : out Equation_Type;
      The_Word      :     Word_Array_Type;
      With_The_Word :     Word_Array_Type)
   is
      Tmp1 : Equation_Type;
      Tmp2 : Equation_Type;
      Tmp3 : Equation_Type;
      -- a2B2 + (A2B2 + a3b3)(a1B1 + (A1B1 + a1b1)(a0B0))
   begin
      -- aB
      Copy (The_Word (0), Tmp1);

      Copy (With_The_Word (0), The_Result);
      Not_Op (The_Result);
      And_Op (The_Result, Tmp1);

      Dispose (Tmp1);

      for I in 1 .. The_Word'Last loop
         -- AB
         Copy (With_The_Word (I), Tmp1);
         Not_Op (Tmp1);

         Copy (The_Word (I), Tmp2);
         Not_Op (Tmp2);
         And_Op (Tmp1, Tmp2);

         Dispose (Tmp2);

         -- ab
         Copy (With_The_Word (I), Tmp2);
         Copy (The_Word (I), Tmp3);
         And_Op (Tmp2, Tmp3);

         Dispose (Tmp3);

         -- AB + ab
         Or_Op (Tmp1, Tmp2);

         Dispose (Tmp2);

         -- (AB + ab)(-)
         And_Op (The_Result, Tmp1);

         Dispose (Tmp1);

         -- aB
         Copy (The_Word (I), Tmp1);
         Copy (With_The_Word (I), Tmp2);
         Not_Op (Tmp2);
         And_Op (Tmp1, Tmp2);

         Dispose (Tmp2);

         -- aB + (AB + ab)(-)
         Or_Op (The_Result, Tmp1);

         Dispose (Tmp1);
      end loop;
   end Unsigned_Greater_Than;

   -- Return the result of greater then between the words and return the result
   -- in the first parameter.

   procedure Unsigned_Greater_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      Tmp1 : Equation_Type;
      -- a2B2 + (A2B2 + a3b3)(a1B1 + (A1B1 + a1b1)(a0B0))
   begin
      Unsigned_Greater_Than
        (Tmp1,
         The_Word (0 .. Length_Of (The_Word) - 1),
         With_The_Word (0 .. Length_Of (With_The_Word) - 1));

      Dispose (The_Word);

      Create (The_Word, Tmp1);
   end Unsigned_Greater_Than;

   -- Return the result of greater then between the words and return the result
   -- in the first parameter.

   procedure Signed_Greater_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      Tmp1       : Equation_Type;
      Tmp2       : Equation_Type;
      The_Result : Equation_Type;
      -- AnBn(unsigned greater than) + -- if both 0 anbn(unsigned greater then) +
      -- -- if both 1 Anbn -- if an 0 and bn 1
   begin
      Copy (With_The_Word (Length_Of (With_The_Word) - 1), Tmp1); -- bn

      Copy (The_Word (Length_Of (The_Word) - 1), The_Result);
      Not_Op (The_Result); -- An

      And_Op (The_Result, Tmp1); -- Anbn

      Dispose (Tmp1);

      if Length_Of (The_Word) > 1 then
         Copy (The_Word (Length_Of (The_Word) - 1), Tmp1);
         Not_Op (Tmp1); -- An

         Copy (With_The_Word (Length_Of (With_The_Word) - 1), Tmp2);
         Not_Op (Tmp2); -- Bn

         And_Op (Tmp1, Tmp2); -- AnBn
         Dispose (Tmp2);

         Unsigned_Greater_Than
           (Tmp2,
            The_Word (0 .. Length_Of (The_Word) - 2),
            With_The_Word (0 .. Length_Of (With_The_Word) - 2));
         And_Op (Tmp1, Tmp2); -- AnBn (unsigned greater_than)

         Dispose (Tmp2);

         Or_Op (The_Result, Tmp1);

         Dispose (Tmp1);

         Copy (The_Word (Length_Of (The_Word) - 1), Tmp1); -- an
         Copy (With_The_Word (Length_Of (With_The_Word) - 1), Tmp2); -- bn
         And_Op (Tmp1, Tmp2); -- anbn

         Dispose (Tmp2);

         Unsigned_Greater_Than
           (Tmp2,
            The_Word (0 .. Length_Of (The_Word) - 2),
            With_The_Word (0 .. Length_Of (With_The_Word) - 2));
         And_Op (Tmp1, Tmp2); -- anbn (unsigned greater_than)
         Dispose (Tmp2);

         Or_Op (The_Result, Tmp1);

         Dispose (Tmp1);
      end if;

      Dispose (The_Word);
      Create (The_Word, The_Result);
   end Signed_Greater_Than;

   -- Negate the word and return the result in the parameter.

   procedure Negate (The_Word : Word_Type) is
      The_Carry : Equation_Type;
      Tmp       : Equation_Type;
   begin
      Create (The_Carry, True);

      for I in 0 .. Length_Of (The_Word) - 1 loop

         Not_Op (The_Word (I));
         Copy (The_Word (I), Tmp);

         Xor_Op (The_Word (I), The_Carry);
         And_Op (The_Carry, Tmp);

         Dispose (Tmp);
      end loop;

      Dispose (The_Carry);
   end Negate;

   -- Return the addition result of the words in the first parameter.

   procedure Add (The_Word : in out Word_Type; With_The_Word : Word_Type) is
      The_Carry : Equation_Type;
      Tmp1      : Equation_Type;
      Tmp2      : Equation_Type;
   begin
      Create (The_Carry, False);

      for I in 0 .. Length_Of (The_Word) - 1 loop

         Copy (The_Word (I), Tmp1);
         Xor_Op (The_Word (I), With_The_Word (I));

         Copy (The_Word (I), Tmp2);
         Xor_Op (The_Word (I), The_Carry);

         And_Op (The_Carry, Tmp2);

         Dispose (Tmp2);

         And_Op (Tmp1, With_The_Word (I));
         Or_Op (The_Carry, Tmp1);

         Dispose (Tmp1);
      end loop;

      Dispose (The_Carry);
   end Add;

   -- Return the subtraction result of the words in the first parameter.

   procedure Subtract
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      The_Borrow : Equation_Type;
      Tmp1       : Equation_Type;
      Tmp2       : Equation_Type;
   begin
      Create (The_Borrow, False);

      for I in 0 .. Length_Of (The_Word) - 1 loop

         Copy (The_Word (I), Tmp1);

         Xor_Op (The_Word (I), With_The_Word (I));
         Xor_Op (The_Word (I), The_Borrow);

         Not_Op (Tmp1);
         Copy (Tmp1, Tmp2);
         Or_Op (Tmp2, With_The_Word (I));
         And_Op (The_Borrow, Tmp2);

         Dispose (Tmp2);

         And_Op (Tmp1, With_The_Word (I));
         Or_Op (The_Borrow, Tmp1);

         Dispose (Tmp1);
      end loop;

      Dispose (The_Borrow);
   end Subtract;

   -- Return the multiplication result of the words in the first parameter.

   procedure Multiply
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      The_Product : Word_Type;
      Tmp1        : Word_Type;
      Tmp2        : Word_Type;
      The_Mask    : Word_Type;
   begin
      Create_Constant (The_Product, Length_Of (The_Word), 0);

      for I in 0 .. Length_Of (The_Word) - 1 loop

         -- carry bit
         Copy (The_Word, Tmp1);

         Create_Constant
           (The_Mask,
            The_Word'Length,
            2**(Length_Of (The_Word) - 1));

         And_Op (Tmp1, The_Mask);
         Not_Equal (Tmp1, The_Mask);

         Dispose (The_Mask);

         Not_Op (Tmp1);

         Shift_Left (The_Word);
         Shift_Left (The_Product);

         Copy (The_Product, Tmp2);
         Add (Tmp2, With_The_Word);

         If_Else (Tmp1, Tmp2, The_Product);

         Dispose (Tmp1);
         Dispose (The_Product);

         The_Product := Tmp2;
      end loop;

      Dispose (The_Word);
      The_Word := The_Product;
   end Multiply;

   -- Return the dividsion result of the words in the first parameter.

   procedure Unsigned_Divide
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      The_Remainder : Word_Type;
      Tmp1          : Word_Type;
      Tmp2          : Word_Type;
      Tmp3          : Word_Type;
      Literal1      : Word_Type;
      The_Borrow    : Equation_Type;
   begin
      Create_Constant (Literal1, Length_Of (The_Word), 1);
      Create_Constant (The_Remainder, Length_Of (The_Word), 0);

      for I in 0 .. Length_Of (The_Word) - 1 loop

         -- shift left (remainder <- equation)
         Copy (The_Word (The_Word'Last), The_Borrow);
         Shift_Left (The_Word);
         Shift_Left (The_Remainder);

         Dispose (The_Remainder (0));

         The_Remainder (0) := The_Borrow;

         -- remainder - right
         Copy (The_Remainder, Tmp1);
         Subtract (Tmp1, With_The_Word);

         -- equation+1
         Copy (The_Word, Tmp2);
         Add (Tmp2, Literal1);

         -- remainder >= right
         Copy (The_Remainder, Tmp3);
         Unsigned_Less_Than (Tmp3, With_The_Word);
         Not_Op (Tmp3);

         If_Else (Tmp3, Tmp1, The_Remainder);

         Dispose (The_Remainder);

         The_Remainder := Tmp1;

         If_Else (Tmp3, Tmp2, The_Word);

         Dispose (Tmp3);
         Dispose (The_Word);

         The_Word := Tmp2;
      end loop;

      Dispose (Literal1);
      Dispose (The_Remainder);
   end Unsigned_Divide;

   -- Return the dividsion result of the words in the first parameter.

   procedure Signed_Divide
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      -- anbn (negate a; negate b; unsigned divide) -- both 1 (negative) anBn
      -- (negate a; unsigned divide; negate result) -- an 1 (negative) and bn
      -- 0 (positive)
      --     dispose negate a
      -- Anbn (negate b; unsigned divide; negate result) -- an 0 (positive) and
      -- bn 1 (negative)
      --     dispose negate b
      -- AnBn (unsigned divide) -- both 0 (positive)
      --     dispose An, Bn
      Not_An     : Equation_Type;
      Not_Bn     : Equation_Type;
      Equ1       : Equation_Type;
      Neg_A      : Word_Type;
      Neg_B      : Word_Type;
      Word1      : Word_Type;
      Word2      : Word_Type;
      The_Result : Word_Type;
   begin
      -- create initial values

      Copy (The_Word (The_Word'Last), Not_An);
      Not_Op (Not_An);

      Copy (With_The_Word (With_The_Word'Last), Not_Bn);
      Not_Op (Not_Bn);

      Copy (The_Word, Neg_A);
      Negate (Neg_A); -- negate a
      Copy (With_The_Word, Neg_B);
      Negate (Neg_B); -- negate b

      -- anbn (negate a; negate b; unsigned divide)

      Copy (Neg_A, Word1);
      Unsigned_Divide (Word1, Neg_B); -- unsigned divide(negate a, negate b)

      Copy (The_Word (The_Word'Last), Equ1);
      And_Op (Equ1, With_The_Word (With_The_Word'Last)); -- anbn

      Fill (The_Result, Word1'Length, Equ1);

      Dispose (Equ1);

      And_Op (The_Result, Word1); -- anbn (unsigned divide)

      Dispose (Word1);

      -- The_Result := (anbn (...))

      -- anBn (negate a; unsigned divide; negate result) -- an 1 (negative) and
      -- bn 0 (positive) dispose negate a

      Word1 := Neg_A; -- rename negate a
      Unsigned_Divide (Word1, With_The_Word); -- unsigned divide(negate a, b)
      Negate (Word1); -- negate result

      Copy (Not_Bn, Equ1);
      And_Op (Equ1, The_Word (The_Word'Last)); -- anBn

      Fill (Word2, Word1'Length, Equ1);

      Dispose (Equ1);

      And_Op (Word1, Word2); -- anBn (negate a; unsigned divide; negate result)

      Dispose (Word2);

      Or_Op (The_Result, Word1); -- (anbn (...)) + (anBn (...))

      Dispose (Word1);

      -- Anbn (negate b; unsigned divide; negate result) dispose negate b

      Copy (The_Word, Word1);
      Unsigned_Divide (Word1, Neg_B); -- unsigned divide(a, negate b)

      Dispose (Neg_B);

      Negate (Word1); -- negate result

      Copy (Not_An, Equ1);
      And_Op (Equ1, With_The_Word (With_The_Word'Last)); -- anBn

      Fill (Word2, Word1'Length, Equ1);

      Dispose (Equ1);

      And_Op (Word1, Word2); -- anBn (negate b; unsigned divide; negate result)

      Dispose (Word2);

      Or_Op (The_Result, Word1); -- (anbn (...)) + (anBn (...)) + (Anbn (...))

      Dispose (Word1);

      -- AnBn (unsigned divide) -- both 0 (positive)

      Copy (The_Word, Word1);
      Unsigned_Divide (Word1, With_The_Word); -- unsigned divide(a, b)

      Equ1 := Not_An; -- renames An
      And_Op (Equ1, Not_Bn); -- AnBn

      Dispose (Not_Bn);

      Fill (Word2, Word1'Length, Equ1);
      Dispose (Equ1);
      And_Op (Word1, Word2); -- AnBn (unsigned divide)

      Dispose (Word2);

      Or_Op
        (The_Result,
         Word1); -- (anbn (...)) + (anBn (...)) + (Anbn (...)) + (AnBn (...))

      Dispose (Word1);
      Dispose (The_Word);

      The_Word := The_Result;
   end Signed_Divide;

   -- Return the remainder result of the words in the first parameter.

   procedure Unsigned_Remainder
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      The_Remainder : Word_Type;
      Tmp1          : Word_Type;
      Tmp2          : Word_Type;
      Tmp3          : Word_Type;
      Literal1      : Word_Type;
      The_Borrow    : Equation_Type;
   begin
      Create_Constant (Literal1, Length_Of (The_Word), 1);
      Create_Constant (The_Remainder, Length_Of (The_Word), 0);

      for I in 0 .. Length_Of (The_Word) - 1 loop

         -- shift left (remainder <- equation)
         Copy (The_Word (Length_Of (The_Word) - 1), The_Borrow);
         Shift_Left (The_Word);
         Shift_Left (The_Remainder);

         Dispose (The_Remainder (0));

         The_Remainder (0) := The_Borrow;

         -- remainder - right
         Copy (The_Remainder, Tmp1);
         Subtract (Tmp1, With_The_Word);

         -- equation+1
         Copy (The_Word, Tmp2);
         Add (Tmp2, Literal1);

         -- remainder >= right
         Copy (The_Remainder, Tmp3);

         Unsigned_Less_Than (Tmp3, With_The_Word);
         Not_Op (Tmp3);

         If_Else (Tmp3, Tmp1, The_Remainder);

         Dispose (The_Remainder);

         The_Remainder := Tmp1;

         If_Else (Tmp3, Tmp2, The_Word);

         Dispose (Tmp3);
         Dispose (The_Word);

         The_Word := Tmp2;
      end loop;

      Dispose (Literal1);
      Dispose (The_Word);

      The_Word := The_Remainder;
   end Unsigned_Remainder;

   -- Return the remainder result of the words in the first parameter.

   procedure Signed_Remainder
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type)
   is
      -- anbn (negate a; negate b; unsigned remainder; negate result) -- both 1
      -- (negative) anBn (negate a; unsigned remainder; negate result) -- an 1
      -- (negative) and bn 0 (positive)
      --     dispose negate a
      -- Anbn (negate b; unsigned remainder) -- an 0 (positive) and bn 1
      -- (negative)
      --     dispose negate b
      -- AnBn (unsigned remainder) -- both 0 (positive)
      --     dispose An, Bn
      Not_An     : Equation_Type;
      Not_Bn     : Equation_Type;
      Equ1       : Equation_Type;
      Neg_A      : Word_Type;
      Neg_B      : Word_Type;
      Word1      : Word_Type;
      Word2      : Word_Type;
      The_Result : Word_Type;
   begin
      -- create initial values
      Copy (The_Word (Length_Of (The_Word) - 1), Not_An);
      Not_Op (Not_An);

      Copy (With_The_Word (Length_Of (With_The_Word) - 1), Not_Bn);
      Not_Op (Not_Bn);

      Copy (The_Word, Neg_A);
      Negate (Neg_A); -- negate a
      Copy (With_The_Word, Neg_B);
      Negate (Neg_B); -- negate b

      -- anbn (negate a; negate b; unsigned remainder; negate result)

      Copy (Neg_A, Word1);
      Unsigned_Remainder
        (Word1,
         Neg_B); -- unsigned remainder(negate a, negate b)
      Negate (Word1);  -- negate result

      Copy (The_Word (Length_Of (The_Word) - 1), Equ1);
      And_Op (Equ1, With_The_Word (Length_Of (With_The_Word) - 1)); -- anbn

      Fill (The_Result, Length_Of (The_Word), Equ1);

      Dispose (Equ1);

      And_Op (The_Result, Word1); -- anbn (unsigned remainder)

      Dispose (Word1);

      -- The_Result := (anbn (...))

      -- anBn (negate a; unsigned remainder; negate result) -- an 1 (negative)
      -- and bn 0 (positive) dispose negate a

      Word1 := Neg_A; -- rename negate a
      Unsigned_Remainder
        (Word1,
         With_The_Word); -- unsigned remainder(negate a, b)
      Negate (Word1); -- negate result

      Copy (Not_Bn, Equ1);
      And_Op (Equ1, The_Word (The_Word'Last)); -- anBn

      Fill (Word2, Word1'Length, Equ1);

      Dispose (Equ1);

      And_Op
        (Word1,
         Word2); -- anBn (negate a; unsigned remainder; negate result)

      Dispose (Word2);

      Or_Op (The_Result, Word1); -- (anbn (...)) + (anBn (...))

      Dispose (Word1);

      -- Anbn (negate b; unsigned remainder) dispose negate b

      Copy (The_Word, Word1);
      Unsigned_Remainder (Word1, Neg_B); -- unsigned remainder(a, negate b)

      Dispose (Neg_B);

      Copy (Not_An, Equ1);
      And_Op (Equ1, With_The_Word (With_The_Word'Last)); -- anBn

      Fill (Word2, Word1'Length, Equ1);

      Dispose (Equ1);
      And_Op
        (Word1,
         Word2); -- anBn (negate b; unsigned remainder; negate result)

      Dispose (Word2);

      Or_Op (The_Result, Word1); -- (anbn (...)) + (anBn (...)) + (Anbn (...))

      Dispose (Word1);

      -- AnBn (unsigned remainder) -- both 0 (positive)

      Copy (The_Word, Word1);
      Unsigned_Remainder (Word1, With_The_Word); -- unsigned remainder(a, b)

      Equ1 := Not_An; -- renames An
      And_Op (Equ1, Not_Bn); -- AnBn

      Dispose (Not_Bn);

      Fill (Word2, Word1'Length, Equ1);

      Dispose (Equ1);

      And_Op (Word1, Word2); -- AnBn (unsigned remainder)
      Dispose (Word2);

      Or_Op
        (The_Result,
         Word1); -- (anbn (...)) + (anBn (...)) + (Anbn (...)) + (AnBn (...))

      Dispose (Word1);
      Dispose (The_Word);

      The_Word := The_Result;
   end Signed_Remainder;

   -- Return the result of placing a word into a section of another word at an
   -- index.

   procedure Assign_Element
     (The_Word       : in out Word_Type;
      The_Element    :        Word_Type;
      With_The_Index :        Word_Type)
   is

      ELEMENTS : constant Word_Count_Type :=
        Length_Of (The_Word) / Length_Of (The_Element);

      The_Result : Word_Type;
      The_Offset : Word_Index_Type;
      The_Mask   : BCInteger_Boolean_Array_Type;
      Tmp1       : Equation_Type;

   begin
      Create_Constant (The_Result, (0 .. Length_Of (The_Word) - 1 => False));

      for I in Word_Index_Type range 0 .. ELEMENTS - 1 loop

         The_Offset := I * Length_Of (The_Element);

         for J in Word_Index_Type range 0 .. Length_Of (The_Element) - 1 loop

            for K in 0 .. ELEMENTS - 1 loop

               if K = I then
                  Copy (The_Element (J), Tmp1);
               else
                  Copy (The_Word (I * The_Element'Length + J), Tmp1);
               end if;

               The_Mask := BCInteger_To_Array (K);

               Index_Equation (Tmp1, With_The_Index, The_Mask);

               Or_Op (The_Result (I * The_Element'Length + J), Tmp1);

               Dispose (Tmp1);
            end loop;
         end loop;
      end loop;

      Dispose (The_Word);
      The_Word := The_Result;
   end Assign_Element;

   -- Return the result of accessing a section, size, in a word.

   procedure Access_Element
     (The_Word         : in out Word_Type;
      With_The_Index   :        Word_Type;
      And_Element_Size :        Word_Length_Type)
   is

      ELEMENTS : constant Word_Count_Type :=
        Length_Of (The_Word) / And_Element_Size;

      The_Result : Word_Type;
      The_Mask   : BCInteger_Boolean_Array_Type;
      Tmp1       : Equation_Type;

   begin
      Create_Constant (The_Result, (0 .. And_Element_Size - 1 => False));

      for I in 0 .. And_Element_Size - 1 loop

         for J in 0 .. ELEMENTS - 1 loop

            Create (Tmp1, True);
            The_Mask := BCInteger_To_Array (J);

            Index_Equation (Tmp1, With_The_Index, The_Mask);

            And_Op (Tmp1, The_Word (I + J * And_Element_Size));
            Or_Op (The_Result (I), Tmp1);

            Dispose (Tmp1);
         end loop;
      end loop;

      Dispose (The_Word);
      The_Word := The_Result;
   end Access_Element;

   -- Convert the word to the size.

   procedure Convert
     (The_Word   : in out Word_Type;
      The_Length :        Word_Length_Type)
   is
      The_Result : Word_Type;
   begin
      if The_Length /= Length_Of (The_Word) then

         Create (The_Result, The_Length);

         if The_Length > Length_Of (The_Word) then

            The_Result (0 .. Length_Of (The_Word) - 1) :=
              The_Word (0 .. Length_Of (The_Word) - 1);

            for I in Length_Of (The_Word) .. The_Length - 1 loop
               Create (The_Result (I), False);
            end loop;

         else
            The_Result (0 .. The_Length - 1) := The_Word (0 .. The_Length - 1);

            for I in The_Length .. Natural (The_Word'Length) - 1 loop
               Dispose (The_Word (I));
            end loop;
         end if;

         Deallocate (The_Word);
         The_Word := The_Result;
      end if;
   end Convert;

   -- Normalize the word's boolean equations with all its variables.

   procedure Normalize (The_Word : Word_Type) is
      The_Set : Variable_Set_Type := EMPTY_SET;
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop

         The_Set := The_Set or Variables_Of (The_Word (I));
      end loop;

      for I in 0 .. Length_Of (The_Word) - 1 loop
         Normalize (The_Word (I), The_Set);
      end loop;
   end Normalize;

   -- Normalize the word's boolean equations with the variables. NOTE: Not sure
   -- this is right. Should or all variable sets together first.

   procedure Normalize
     (The_Word      : Word_Type;
      The_Variables : Variable_Set_Type)
   is
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop
         Normalize (The_Word (I), The_Variables);
      end loop;
   end Normalize;

   -- Return true if the word is a constant.

   function Is_Constant (The_Word : Word_Type) return Boolean is
      The_Result : Boolean := True;
   begin

      for I in 0 .. Length_Of (The_Word) - 1 loop
         The_Result := The_Result and Is_Constant (The_Word (I));
      end loop;

      return The_Result;
   end Is_Constant;

   -- Return the constant value of the word as an integer.

   function To_Constant (The_Word : Word_Type) return BCInteger is
      The_Integer  : BCInteger_Boolean_Array_Type;
      The_Constant : Boolean_Array_Type (The_Word'Range);
   begin
      The_Integer  := BCInteger_Boolean_Array_Type'(others => False);
      The_Constant := To_Constant (The_Word);

      for I in BCInteger_Boolean_Array_Type'Range loop

         exit when I > The_Constant'Last;
         The_Integer (I) := The_Constant (I);
      end loop;

      return To_BCInteger (The_Integer);
   end To_Constant;

   -- Return the constant value of the word as a boolean array.

   function To_Constant (The_Word : Word_Type) return Boolean_Array_Type is
      The_Result : Boolean_Array_Type (The_Word'Range);
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop

         The_Result (I) := To_Constant (The_Word (I));
      end loop;

      return The_Result;
   end To_Constant;

   -- Return boolean word solutions for the variable values.

   function Solve
     (The_Word        : Word_Type;
      With_The_Values : Boolean_Array_Type) return Boolean_Array_Type
   is
      The_Result : Boolean_Array_Type (0 .. Length_Of (The_Word) - 1);
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop

         The_Result (I) := Solve (The_Word (I), With_The_Values);
      end loop;

      return The_Result;
   end Solve;

   -- Return the string representation of boolean values for the word.

   function Boolean_Of (The_Word : Word_Type) return String is
      The_Image : Unbounded_String := Null_Unbounded_String;
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop

         Append (The_Image, " " & Boolean_Of (The_Word (I)));
      end loop;

      return To_String (The_Image);
   end Boolean_Of;

   -- Return the string representation of fundamental product terms for the
   -- word.

   function Image_Of (The_Word : Word_Type) return String is
      The_Image : Unbounded_String := Null_Unbounded_String;
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop

         Append (The_Image, " " & Image_Of (The_Word (I)));
      end loop;

      return To_String (The_Image);
   end Image_Of;

   -- Iterate over the boolean equations applying the procedure on each.

   procedure Iterate
     (The_Word : Word_Type;
      Process  : not null access procedure (The_Equation : Equation_Type))
   is
   begin
      for I in 0 .. Length_Of (The_Word) - 1 loop

         Process (The_Word (I));
      end loop;
   end Iterate;

end Word_Package;


-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Pool_Package;
--
with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
with Equation_Package; use Equation_Package;
with Number_Package;   use Number_Package;

-- A package to define the boolean equations of a value, boolean word (bw).
--
-- A bw is an array of boolean equations that represent an numeric variable or
-- value.
--
-- Boolean word operations, not, and, or, xor, ..., returns the result in the
-- first parameter.

package Word_Package with
     Spark_Mode => Off is

   WORD_SIZE : constant := 30;

   -- Boolean Word memory storage pool.

   The_Pool : Pool_Package.Storage_Pool;

   -- Boolean Word type.

   type Word_Type is private;

   -- The Boolean Word type.

   subtype Word_Count_Type is SYSNatural range 0 .. WORD_SIZE;
   subtype Word_Length_Type is Word_Count_Type;
   subtype Word_Index_Type is Word_Count_Type range 0 .. WORD_SIZE - 1;

   -- An empty boolean word for intialization.

   EMPTY_WORD : constant Word_Type;

   -- Return length of the boolean word.

   function Length_Of (The_Word : Word_Type) return Word_Count_Type;

   -- Return set of variables for boolean word.

   function Variables_Of (The_Word : Word_Type) return Variable_Set_Type;

   -- Create an empty boolean word.

   procedure Create (The_Word : out Word_Type);

   -- Create a boolean word constant value from the boolean array.

   procedure Create_Constant
     (The_Word  : out Word_Type;
      The_Value :     Boolean_Array_Type);

   -- Create a boolean word constant value of size from the modular.

   procedure Create_Constant
     (The_Word   : out Word_Type;
      The_Length :     Word_Length_Type;
      The_Value  :     BCModular);

   -- Create a boolean word of variable boolean equations, f(n) = n, staring
   -- with the variable.

   procedure Create_Variable
     (The_Word     : out Word_Type;
      The_Length   :     Word_Length_Type;
      The_Variable :     Variable_Type);

   -- Dispose the word.

   procedure Dispose (The_Word : in out Word_Type);

   -- Copy the word.

   procedure Copy (From_The_Word : Word_Type; To_The_Word : out Word_Type);

   -- Append the boolean equation to a word.

   procedure Append
     (To_The_Word  : in out Word_Type;
      The_Equation :        Equation_Type);

   -- Append the word to a word.

   procedure Append (To_The_Word : in out Word_Type; The_Word : Word_Type);

   -- Fill the word of size with the boolean equation.

   procedure Fill
     (The_Word          : out Word_Type;
      Of_The_Length       :     Word_Length_Type;
      With_The_Equation :     Equation_Type);

   -- Shift the word left and return in the parameter.

   procedure Shift_Left (The_Word : Word_Type);

   -- Shift the word right and return in the parameter.

   procedure Shift_Right (The_Word : Word_Type);

   -- Not the word and return in the parameter.

   procedure Not_Op (The_Word : Word_Type);

   -- And the words and return the result in the first parameter.

   procedure And_Op (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Or the words and return the result in the first parameter.

   procedure Or_Op (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Xor the words and return the result in the first parameter.

   procedure Xor_Op (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Return the result of applying the condition to the words, f(a) = c | a & C
   -- | b.

   procedure If_Else
     (The_Condition : Word_Type;
      The_Word      : Word_Type;
      And_The_Word  : Word_Type);

   -- Return the result of not equal between the words and return the result in
   -- the first parameter.

   procedure Not_Equal (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Return the result of less then between the words and return the result in
   -- the first parameter.

   procedure Unsigned_Less_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the result of less then between the words and return the result in
   -- the first parameter.

   procedure Signed_Less_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the result of greater then between the words and return the result
   -- in the first parameter.

   procedure Unsigned_Greater_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the result of greater then between the words and return the result
   -- in the first parameter.

   procedure Signed_Greater_Than
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Negate the word and return the result in the parameter.

   procedure Negate (The_Word : Word_Type);

   -- Return the addition result of the words in the first parameter.

   procedure Add (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Return the subtraction result of the words in the first parameter.

   procedure Subtract (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Return the multiplication result of the words in the first parameter.

   procedure Multiply (The_Word : in out Word_Type; With_The_Word : Word_Type);

   -- Return the dividsion result of the words in the first parameter.

   procedure Unsigned_Divide
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the dividsion result of the words in the first parameter.

   procedure Signed_Divide
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the remainder result of the words in the first parameter.

   procedure Unsigned_Remainder
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the remainder result of the words in the first parameter.

   procedure Signed_Remainder
     (The_Word      : in out Word_Type;
      With_The_Word :        Word_Type);

   -- Return the result of placing a word into a section of another word at an
   -- index.

   procedure Assign_Element
     (The_Word       : in out Word_Type;
      The_Element    :        Word_Type;
      With_The_Index :        Word_Type);

   -- Return the result of accessing a section, size, in a word.

   procedure Access_Element
     (The_Word         : in out Word_Type;
      With_The_Index   :        Word_Type;
      And_Element_Size :        Word_Length_Type);

   -- Convert the word to the size.

   procedure Convert
     (The_Word   : in out Word_Type;
      The_Length :        Word_Length_Type);

   -- Normalize the word's boolean equations with all its variables.

   procedure Normalize (The_Word : Word_Type);

   -- Normalize the word's boolean equations with the variables. NOTE: Not sure
   -- this is right. Should or all variable sets together first.

   procedure Normalize
     (The_Word      : Word_Type;
      The_Variables : Variable_Set_Type);

   -- Return true if the word is a constant.

   function Is_Constant (The_Word : Word_Type) return Boolean;

   -- Return the constant value of the word as an integer.

   function To_Constant (The_Word : Word_Type) return BCInteger;

   -- Return the constant value of the word as a boolean array.

   function To_Constant (The_Word : Word_Type) return Boolean_Array_Type;

   -- Return boolean word solutions for the variable values.

   function Solve
     (The_Word        : Word_Type;
      With_The_Values : Boolean_Array_Type) return Boolean_Array_Type;

   -- Return the string representation of boolean values for the word.

   function Boolean_Of (The_Word : Word_Type) return String;

   -- Return the string representation of fundamental product terms for the
   -- word.

   function Image_Of (The_Word : Word_Type) return String;

   -- Iterate over the boolean equations applying the procedure on each.

   procedure Iterate
     (The_Word : Word_Type;
      Process  : not null access procedure (The_Equation : Equation_Type));

private

   -- Boolean word's array of boolean equations.

   type Word_Array_Type is array (Word_Index_Type range <>) of Equation_Type;
   pragma Pack (Word_Array_Type);

   -- The Boolean Word type.

   type Word_Type is access Word_Array_Type;
   for Word_Type'Storage_Pool use The_Pool;

   -- An empty boolean word for intialization.

   EMPTY_WORD : constant Word_Type := new Word_Array_Type (0 .. -1);

   -- Create the boolean word with the length.

   procedure Create (The_Word : out Word_Type; The_Length : Word_Length_Type);

      -- Create a boolean word with the boolean equation.

   procedure Create
     (The_Word     : out Word_Type;
      The_Equation :     Equation_Type);

   -- Return length of the boolean word.

   function Length_Of
     (The_Word : Word_Type) return Word_Count_Type is
     (The_Word'Length);

end Word_Package;

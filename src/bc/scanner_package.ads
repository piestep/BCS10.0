-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with BC_Package;     use BC_Package;
with Source_Package; use Source_Package;
--

-- A package to scan the source program and parse into symbols.

package Scanner_Package is

   NUMBER_OF_RESERVED_WORDS : constant := 30;

   Scanner_Debug : Boolean := False; -- scanner debug flag.

   -- BC defined source program strings.

   Boolean_String : constant Unbounded_String :=
     To_Unbounded_String ("BOOLEAN");
   True_String    : constant Unbounded_String := To_Unbounded_String ("TRUE");
   False_String   : constant Unbounded_String := To_Unbounded_String ("FALSE");
   Integer_String : constant Unbounded_String :=
     To_Unbounded_String ("INTEGER");

   -- BC program symbols.

   type Symbol is
     (Identifier_Symbol,
      Integer_Symbol,
      Plus_Symbol,
      Minus_Symbol,
      Times_Symbol,
      Divide_Symbol,
      Rem_Symbol,
      Mod_Symbol,
      And_Symbol,
      Or_Symbol,
      Xor_Symbol,
      Not_Symbol,
      Equal_Symbol,
      Not_Equal_Symbol,
      Less_Than_Symbol,
      Greater_Than_Symbol,
      Less_Than_Equal_Symbol,
      Greater_Than_Equal_Symbol,
      Left_Paren_Symbol,
      Right_Paren_Symbol,
      Tick_Symbol,
      Thru_Symbol,
      Becomes_Symbol,
      Period_Symbol,
      Comma_Symbol,
      Colon_Symbol,
      Semicolon_Symbol,
      Package_Symbol,
      Body_Symbol,
      Procedure_Symbol,
      In_Symbol,
      Out_Symbol,
      Is_Symbol,
      Type_Symbol,
      Constant_Symbol,
      New_Symbol,
      Range_Symbol,
      Array_Symbol,
      Of_Symbol,
      Begin_Symbol,
      Null_Symbol,
      If_Symbol,
      Then_Symbol,
      Else_Symbol,
      For_Symbol,
      Reverse_Symbol,
      Loop_Symbol,
      End_Symbol);

   -- Last symbol read from source program.

   The_Symbol : Symbol;

   -- The string of the last symbol.

   The_String : Unbounded_String;

   -- The ending position of the last symbol.

   The_Position : Source_Position;

   -- Parse next symbol in source program.

   procedure Next_Symbol;

end Scanner_Package;

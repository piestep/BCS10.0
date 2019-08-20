-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Deallocation;
--
with Ada.Text_IO; use Ada.Text_IO;
--
with Debug_Package; use Debug_Package;
--

package body Operand_Package is

   -- Copy a constant operand.

   function Copy
     (The_Operand : access Constant_Operand) return Operand_Pointer
   is
   begin
      return new Constant_Operand'
          (The_Type  => The_Operand.The_Type,
           The_Value => The_Operand.The_Value);
   end Copy;

   -- Copy a variable operand.

   function Copy
     (The_Operand : access Variable_Operand) return Operand_Pointer
   is
   begin
      return new Variable_Operand'(The_Type => The_Operand.The_Type);
   end Copy;

   -- Copy an identifier operand.

   function Copy
     (The_Operand : access Identifier_Operand) return Operand_Pointer
   is
   begin
      return new Identifier_Operand'
          (The_Type       => The_Operand.The_Type,
           The_Identifier => The_Operand.The_Identifier);
   end Copy;

   -- Copy an array operand.

   function Copy (The_Operand : access Array_Operand) return Operand_Pointer is
   begin
      return new Array_Operand'
          (The_Type       => The_Operand.The_Type,
           The_Identifier => The_Operand.The_Identifier,
           The_Index      => Copy (The_Operand.The_Index));
   end Copy;

   -- Dispose the operand.

   procedure Dispose (The_Operand : in out Operand_Pointer) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Operand_Record'Class,
         Operand_Pointer);
   begin
      if The_Operand = null then
         return;
      end if;

      if Is_Array (The_Operand) then
         Dispose (Array_Operand (The_Operand.all).The_Index);
      end if;

      Free (The_Operand);
   end Dispose;

   -- Return true if constant operand.

   function Is_Constant (The_Operand : Operand_Pointer) return Boolean is
   begin
      return The_Operand.all in Constant_Operand;
   end Is_Constant;

   -- Return true if variable operand.

   function Is_Variable (The_Operand : Operand_Pointer) return Boolean is
   begin
      return The_Operand.all in Variable_Operand;
   end Is_Variable;

   -- Return true if identifier operand.

   function Is_Identifier (The_Operand : Operand_Pointer) return Boolean is
   begin
      return The_Operand.all in Identifier_Operand;
   end Is_Identifier;

   -- Return true if array operand.

   function Is_Array (The_Operand : Operand_Pointer) return Boolean is
   begin
      return The_Operand.all in Array_Operand;
   end Is_Array;

   -- Return result of an unary operation. Raise critical exception if symbol
   -- not an unary operation.

   function Constant_Operation
     (The_Operator : Symbol;
      The_Right    : SYSInteger) return SYSInteger
   is
   begin
      case The_Operator is
         when Not_Symbol =>
            if The_Right = 0 then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Plus_Symbol =>
            return The_Right;
         when Minus_Symbol =>
            return 0 - The_Right;
         when others =>
            raise Critical_Error;
      end case;
   end Constant_Operation;

   -- Return result of a binary operation. Raise critical exception if symbol
   -- not a binary operation.

   function Constant_Operation
     (The_Operator : Symbol;
      The_Left     : SYSInteger;
      The_Right    : SYSInteger) return SYSInteger
   is
   begin
      case The_Operator is
         when Equal_Symbol =>
            if The_Left = The_Right then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Not_Equal_Symbol =>
            if The_Left /= The_Right then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Less_Than_Symbol =>
            if The_Left < The_Right then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Less_Than_Equal_Symbol =>
            if The_Left <= The_Right then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Greater_Than_Symbol =>
            if The_Left > The_Right then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Greater_Than_Equal_Symbol =>
            if The_Left >= The_Right then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when And_Symbol =>
            if The_Left = Boolean_True and The_Right = Boolean_True then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Or_Symbol =>
            if The_Left = Boolean_True or The_Right = Boolean_True then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Xor_Symbol =>
            if (The_Left = Boolean_True and The_Right = Boolean_False) or
              (The_Left = Boolean_False and The_Right = Boolean_True)
            then
               return Boolean_True;
            else
               return Boolean_False;
            end if;
         when Plus_Symbol =>
            return The_Left + The_Right;
         when Minus_Symbol =>
            return The_Left - The_Right;
         when Times_Symbol =>
            return The_Left * The_Right;
         when Divide_Symbol =>
            return The_Left / The_Right;
         when Rem_Symbol =>
            return The_Left rem The_Right;
         when Mod_Symbol =>
            return The_Left mod The_Right;
         when others =>
            raise Critical_Error;
      end case;
   end Constant_Operation;

begin
   Debug (Debug_Initialization, "Operand_Package");
end Operand_Package;

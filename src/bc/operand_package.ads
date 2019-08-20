-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package; use System_Package;
with Pool_Package;   use Pool_Package;
--
with BC_Package;         use BC_Package;
with Scanner_Package;    use Scanner_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
--

-- A pacakge to define operands for parsing. Operands are used to hold operand
-- data passed between language constructs.

package Operand_Package is

   The_Pool : Storage_Pool;

   -- An abstract operand.

   type Operand_Record is abstract tagged record
      The_Type : Type_Pointer;        -- operand type.
   end record;

   -- The Operand type.

   type Operand_Pointer is access all Operand_Record'Class;
   for Operand_Pointer'Storage_Pool use The_Pool;

   -- Copy an operand.

   function Copy
     (The_Operand : access Operand_Record) return Operand_Pointer is abstract;

   -- A constant operand.

   type Constant_Operand is new Operand_Record with record
      The_Value : SYSInteger;    -- the operand value.
   end record;

   -- Copy a constant operand.

   function Copy (The_Operand : access Constant_Operand) return Operand_Pointer;

   -- A variable operand.

   type Variable_Operand is new Operand_Record with null record;

   -- Copy a variable operand.

   function Copy (The_Operand : access Variable_Operand) return Operand_Pointer;

   -- An identifier operand.

   type Identifier_Operand is new Operand_Record with record
      The_Identifier : Identifier_Pointer;    -- operands identifier.
   end record;

   -- Copy an identifier operand.

   function Copy
     (The_Operand : access Identifier_Operand) return Operand_Pointer;

   -- An array operand.

   type Array_Operand is new Identifier_Operand with record
      The_Index : Operand_Pointer;                    -- operand of array's index.
   end record;

   -- Copy an array operand.

   function Copy (The_Operand : access Array_Operand) return Operand_Pointer;

   -- Dispose the operand.

   procedure Dispose (The_Operand : in out Operand_Pointer);

   -- Return true if constant operand.

   function Is_Constant (The_Operand : Operand_Pointer) return Boolean;

   -- Return true if variable operand.

   function Is_Variable (The_Operand : Operand_Pointer) return Boolean;

   -- Return true if identifier operand.

   function Is_Identifier (The_Operand : Operand_Pointer) return Boolean;

   -- Return true if array operand.

   function Is_Array (The_Operand : Operand_Pointer) return Boolean;

   -- Return result of an unary operation. Raise critical exception if symbol
   -- not an unary operation.

   function Constant_Operation
     (The_Operator : Symbol;
      The_Right    : SYSInteger) return SYSInteger;

   -- Return result of a binary operation. Raise critical exception if symbol
   -- not a binary operation.

   function Constant_Operation
     (The_Operator : Symbol;
      The_Left     : SYSInteger;
      The_Right    : SYSInteger) return SYSInteger;

end Operand_Package;

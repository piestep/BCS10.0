-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with System_Package;  use System_Package;
with Pool_Package; use Pool_Package;
with BC_Package;      use BC_Package;
with Type_Package;    use Type_Package;
--

-- A pacakge to define program source identifiers.

package Identifier_Package is

   The_Pool : Storage_Pool;

   -- An abstract basic identifier.

   type Identifier_Record is abstract tagged record
      The_String : Unbounded_String;  -- string for the identifier.
   end record;

   -- The Identifier type.

   type Identifier_Pointer is access all Identifier_Record'Class;
   for Identifier_Pointer'Storage_Pool use The_Pool;

   -- A package identifier.

   type Package_Identifier is new Identifier_Record with null record;

   -- A procedure identifier.

   type Procedure_Identifier is new Identifier_Record with null record;

   -- A type identifier.

   type Type_Identifier is new Identifier_Record with record
      The_Type : Type_Pointer;         -- type of the identifier.
   end record;

   -- A typed identifier.

   type Typed_Identifier is abstract new Identifier_Record with record
      The_Type : Type_Pointer;         -- type of the variable.
   end record;

   -- A constant identifier.

   type Constant_Identifier is new Typed_Identifier with record
      The_Value : SYSInteger;                      -- value.
   end record;

   -- An addressable identifier.

   type Addressable_Identifier is abstract new Typed_Identifier with record
      The_Address : SYSNatural;              -- address.
   end record;

   -- A variable identifier.

   type Variable_Identifier is new Addressable_Identifier with record
      The_Value : SYSInteger;                      -- initial value.
   end record;

   -- A index loop identifier.

   type Index_Identifier is new Addressable_Identifier with null record;

   -- A parameter identifier.

   type Parameter_Identifier is new Addressable_Identifier with record
      Is_In  : Boolean := False;     -- in parameter.
      Is_Out : Boolean := False;     -- out parameter.
   end record;

   -- Return true if identifier is a package identifier.

   function Is_Package (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is a procedure identifier.

   function Is_Procedure (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is a type identifier.

   function Is_Type (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is a typed identifier.

   function Is_Typed (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is a constant identifier.

   function Is_Constant (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is an addressable identifier.

   function Is_Addressable (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is a variable identifier.

   function Is_Variable (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is an index identifier.

   function Is_Index (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is an parameter identifier.

   function Is_Parameter (The_Identifier : Identifier_Pointer) return Boolean;

end Identifier_Package;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Pool_Package; use Pool_Package;
--
with System_Package; use System_Package;
with BC_Package;     use BC_Package;
--

-- A pacakge to define basic compiler types.

package Type_Package is

   The_Pool : Storage_Pool;

   -- Type Record and Type pointer.

   type Type_Record;

   type Type_Pointer is access all Type_Record'Class;
   for Type_Pointer'Storage_Pool use The_Pool;

   -- A list of allocated types.
   -- Note: Used to deallocate all types after deallocation of the graph.

   The_Last : Type_Pointer;

   -- A basic type.

   type Type_Record is tagged record
      The_Previous : Type_Pointer;
      The_Base : Type_Pointer; --      parent type.
   end record;

   -- An array type.

   type Array_Type is new Type_Record with record
      The_Index   : Type_Pointer; -- array index range type.
      The_Element : Type_Pointer; -- element type.
      The_First   : SYSInteger;   -- first index value.
      The_Last    : SYSInteger;   -- last index value.
   end record;

   -- A scalar type.

   type Scalar_Type is abstract new Type_Record with record
      The_First : SYSInteger; -- first value of scalar range.
      The_Last  : SYSInteger; -- last value of scalar range.
      The_Size  : SYSNatural; -- number of bits to hold scalar value.
   end record;

   -- A discrete type.

   type Discrete_Type is new Scalar_Type with null record;

   -- A signed type.

   type Signed_Type is new Scalar_Type with null record;

   -- A modular type.

   type Modular_Type is new Scalar_Type with record
      The_Modulas : SYSInteger; -- modulas of type.
   end record;

   -- The base type for all booleans.

   Universal_Boolean : constant Type_Pointer;

   -- The base type for all integers.

   Universal_Integer : constant Type_Pointer;

   -- The boolean type.

   Boolean_Type : constant Type_Pointer;

   -- The integer type.

   Integer_Type : constant Type_Pointer;

   -- Clear all linked types.

   procedure Clear;

   -- Dispose type.

   procedure Dispose (The_Type : in out Type_Pointer);

   -- Return true if array type.

   function Is_Array (The_Type : Type_Pointer) return Boolean;

   -- Return true if scalar type.

   function Is_Scalar (The_Type : Type_Pointer) return Boolean;

   -- Return true if discrete type.

   function Is_Discrete (The_Type : Type_Pointer) return Boolean;

   -- Return true if signed type.

   function Is_Signed (The_Type : Type_Pointer) return Boolean;

   -- Return true if modular.

   function Is_Modular (The_Type : Type_Pointer) return Boolean;

   -- Return true if type is null or type is derived from universal boolean.

   function Is_Boolean (The_Type : Type_Pointer) return Boolean;

   -- Return true if type is null or type is derived from universal integer.

   function Is_Integer (The_Type : Type_Pointer) return Boolean;

   -- Return derived base type, if type is null then return null, for scalar
   -- type the type where base is null, and for array type the type.

   function Base_Of (The_Type : Type_Pointer) return Type_Pointer;

   -- Return first value of type, if type is null return 0, if scalar then first
   -- value, if array then first value of range. Raise critical exception if
   -- type is not array or scalar.

   function First_Of (The_Type : Type_Pointer) return SYSInteger;

   -- Return last value of type, if type is null return 0, if scalar then last
   -- value, if array then last value of range. Raise critical exception if type
   -- is not array or scalar.

   function Last_Of (The_Type : Type_Pointer) return SYSInteger;

   -- Return size of type, if type is null return 0, if scalar then size,
   -- if array then number of elements * size of the element. Raise critical
   -- exception if type is not array or scalar.

   function Size_Of (The_Type : Type_Pointer) return SYSNatural;

   -- Return true for type is null return null, for scalar then if value within
   -- first and last value, for array then if value is within first and last
   -- index range. Raise critical exception if type is not array or scalar.

   function Is_Within
     (The_Value       : SYSInteger;
      Within_The_Type : Type_Pointer) return Boolean;

   -- Return true if left or right type is null, or if both the left and right
   -- have the same base type.

   function Is_Compatiable
     (The_Left  : Type_Pointer;
      The_Right : Type_Pointer) return Boolean;

   -- Return the best type, non-null or non-universal type.

   function Best_Of
     (The_Left  : Type_Pointer;
      The_Right : Type_Pointer) return Type_Pointer;

   -- Return size for an integer type to hold the maximum and minimum values.

   function Size_Of
     (The_Minimum : SYSInteger;
      The_Maximum : SYSInteger) return SYSNatural;

   -- Return size for a modular type to hold the maximum value.

   function Size_Of (The_Maximum : SYSInteger) return SYSNatural;

private

   -- Define universal types.

   Universal_Boolean : constant Type_Pointer :=
     new Type_Record'(The_Previous => null, The_Base => null);

   Universal_Integer : constant Type_Pointer :=
     new Type_Record'(The_Previous => null, The_Base => null);

   -- Define boolean type.

   Boolean_Type : constant Type_Pointer :=
     new Discrete_Type'
       (The_Previous => null,
        The_Base  => Universal_Boolean,
        The_First => Boolean_False,
        The_Last  => Boolean_True,
        The_Size  => 1);

   -- Define integer type.

   Integer_Type : constant Type_Pointer :=
     new Signed_Type'
       (The_Previous => null,
        The_Base  => Universal_Integer,
        The_First => -(2**7),
        The_Last  => (2**7) - 1,
        The_Size  => 8);

end Type_Package;

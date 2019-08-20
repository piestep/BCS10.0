-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with Debug_Package; use Debug_Package;
--

package body Type_Package is

   -- Return true if scalar type.

   function Is_Array (The_Type : Type_Pointer) return Boolean is
   begin
      return The_Type.all in Array_Type'Class;
   end Is_Array;

   -- Return true if type is null or type is derived from universal boolean.

   function Is_Scalar (The_Type : Type_Pointer) return Boolean is
   begin
      return The_Type.all in Scalar_Type'Class;
   end Is_Scalar;

   -- Return true if discrete type.

   function Is_Discrete (The_Type : Type_Pointer) return Boolean is
   begin
      return The_Type.all in Discrete_Type'Class;
   end Is_Discrete;

   -- Return true if signed type.

   function Is_Signed (The_Type : Type_Pointer) return Boolean is
   begin
      return The_Type.all in Signed_Type'Class;
   end Is_Signed;

   -- Return true if modular.

   function Is_Modular (The_Type : Type_Pointer) return Boolean is
   begin
      return The_Type.all in Modular_Type'Class;
   end Is_Modular;

   -- Return true if type is null or type is derived from universal boolean.

   function Is_Boolean (The_Type : Type_Pointer) return Boolean is
   begin
      if The_Type = null then
         return True;
      end if;
      return Base_Of (The_Type) = Universal_Boolean;
   end Is_Boolean;

   -- Return true if type is null or type is derived from universal integer.

   function Is_Integer (The_Type : Type_Pointer) return Boolean is
   begin
      if The_Type = null then
         return True;
      end if;
      return Base_Of (The_Type) = Universal_Integer;
   end Is_Integer;

   -- Return derived base type, if type is null then return null, for scalar
   -- type the parent type where base is null, and for array type the type.

   function Base_Of (The_Type : Type_Pointer) return Type_Pointer is
   begin
      if The_Type = null then
         return null;
      elsif Is_Array (The_Type) then
         return The_Type;
      elsif The_Type.The_Base = null then
         return The_Type;
      else
         return Base_Of (The_Type.The_Base);
      end if;
   end Base_Of;

   -- Return first value of type, if type is null return 0, if scalar then first
   -- value, if array then first value of range. Raise critical exception if
   -- type is not array or scalar.

   function First_Of (The_Type : Type_Pointer) return SYSInteger is
   begin
      if The_Type = null then
         return 0;
      elsif Is_Scalar (The_Type) then
         return Scalar_Type (The_Type.all).The_First;
      elsif Is_Array (The_Type) then
         return Array_Type (The_Type.all).The_First;
      else
         raise Critical_Error;
      end if;
   end First_Of;

   -- Return last value of type, if type is null return 0, if scalar then last
   -- value, if array then last value of range. Raise critical exception if type
   -- is not array or scalar.

   function Last_Of (The_Type : Type_Pointer) return SYSInteger is
   begin
      if The_Type = null then
         return 0;
      elsif Is_Scalar (The_Type) then
         return Scalar_Type (The_Type.all).The_Last;
      elsif Is_Array (The_Type) then
         return Array_Type (The_Type.all).The_Last;
      else
         raise Critical_Error;
      end if;
   end Last_Of;

   -- Return size of type, if type is null return 0, if scalar then size,
   -- if array then number of elements * size of the element. Raise critical
   -- exception if type is not array or scalar.

   function Size_Of (The_Type : Type_Pointer) return SYSNatural is
      The_Size : SYSInteger;
   begin
      if The_Type = null then
         return 0;
      elsif Is_Scalar (The_Type) then
         return Scalar_Type (The_Type.all).The_Size;
      elsif Is_Array (The_Type) then
         The_Size :=
           SYSNatural'Max
             (0,
              (Array_Type (The_Type.all).The_Last -
               Array_Type (The_Type.all).The_First +
               1) *
              Size_Of (Array_Type (The_Type.all).The_Element));
         return The_Size;
      else
         raise Critical_Error;
      end if;
   end Size_Of;

   -- Return true for type is null return null, for scalar then if value within
   -- first and last value, for array then if value is within the index range.
   -- Raise critical exception if type is not array or scalar.

   function Is_Within
     (The_Value       : SYSInteger;
      Within_The_Type : Type_Pointer) return Boolean
   is
   begin
      if Within_The_Type = null then
         return True;
      elsif Within_The_Type = Universal_Integer then
         return True;
      elsif Within_The_Type = Universal_Boolean then
         return The_Value = 0 or The_Value = 1;
      elsif Is_Discrete (Within_The_Type) or
        Is_Signed (Within_The_Type) or
        Is_Modular (Within_The_Type)
      then
         return The_Value >= First_Of (Within_The_Type) and
           The_Value <= Last_Of (Within_The_Type);
      elsif Is_Array (Within_The_Type) then
         return The_Value >= Array_Type (Within_The_Type.all).The_First and
           The_Value <= Array_Type (Within_The_Type.all).The_Last;
      else
         raise Critical_Error;
      end if;
   end Is_Within;

   -- Return true if left or right type is null, or if both the left and right
   -- have the same base type.

   function Is_Compatiable
     (The_Left  : Type_Pointer;
      The_Right : Type_Pointer) return Boolean
   is
   begin
      if The_Left = null then
         return True;
      elsif The_Right = null then
         return True;
      elsif The_Left = The_Right then
         return True;
      elsif The_Left.The_Base = null and The_Left = Base_Of (The_Right) then
         return True;
      elsif The_Right.The_Base = null and The_Right = Base_Of (The_Left) then
         return True;
      end if;
      return False;
   end Is_Compatiable;

   -- Return the best type, non-null or non-universal type. Return the left if
   -- both are acceptable.

   function Best_Of
     (The_Left  : Type_Pointer;
      The_Right : Type_Pointer) return Type_Pointer
   is
   begin
      if The_Left = null then
         return The_Right;
      elsif The_Right = null then
         return The_Left;
      elsif The_Left = Universal_Integer then
         return The_Right;
      elsif The_Right = Universal_Integer then
         return The_Left;
      elsif The_Left = Universal_Boolean then
         return The_Right;
      elsif The_Right = Universal_Boolean then
         return The_Left;
      end if;
      return The_Left;
   end Best_Of;

   -- Return size for an integer type to hold the maximum and minimum values.

   function Size_Of
     (The_Minimum : SYSInteger;
      The_Maximum : SYSInteger) return SYSNatural
   is
      The_Size : SYSNatural := 1;
      The_Count : SYSNatural;
   begin
      The_Count := The_Maximum - The_Minimum + 1;

      while 2**The_Size < The_Count loop
         The_Size := The_Size + 1;
      end loop;

      return The_Size;
   end Size_Of;

   -- Return size for a modular type to hold the maximum value.

   function Size_Of (The_Maximum : SYSInteger) return SYSNatural is
      The_Size : SYSNatural := 1;
   begin
      while 2**The_Size < The_Maximum loop
         The_Size := The_Size + 1;
      end loop;
      return The_Size;
   end Size_Of;

begin

   Debug (Debug_Initialization, "Type_Package");

end Type_Package;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Conversion;
--
with System_Package; use System_Package;
--

-- A package to define various arrays of boolean values.

package Boolean_Package with
Spark_Mode => Off is

   -- Boolean array constants.

   MAX_INDEX : constant := SYSNatural'Last - 1;

   -- Boolean array types.

   subtype Boolean_Count_Type is SYSNatural;
   subtype Boolean_Index_Type is Boolean_Count_Type range 0 .. MAX_INDEX;

   -- Boolean array type.

   type Boolean_Array_Type is array (Boolean_Index_Type range <>) of Boolean;
   pragma Pack (Boolean_Array_Type);

   -- An array of boolean values the size of a system integer.

   subtype SYSInteger_Boolean_Array_Type is
     Boolean_Array_Type (0 .. SYSINTEGER_SIZE - 1);

   -- An array of boolean values the size of a bc integer type.

   subtype BCInteger_Boolean_Array_Type is
     Boolean_Array_Type (0 .. BCINTEGER_SIZE - 1);
   subtype BCInteger_Boolean_Array_Index_Type is
     BCModular range 0 .. BCINTEGER_SIZE;

   -- An array of boolean values the size of a bc modular type.

   subtype BCModular_Boolean_Array_Type is
     Boolean_Array_Type (0 .. BCMODULAR_SIZE - 1);
   subtype BCModular_Boolean_Array_Index_Type is
     BCModular range 0 .. BCMODULAR_SIZE;

   -- Convert an integer to an array of boolean values the size of system
   -- integer.

   function SYSInteger_To_Boolean_Array_Type is new Ada.Unchecked_Conversion
     (SYSInteger,
      SYSInteger_Boolean_Array_Type);

   -- Convert an integer to an array of boolean values the size of an integer.

   function BCInteger_To_Array is new Ada.Unchecked_Conversion
     (BCInteger,
      BCInteger_Boolean_Array_Type);

   -- Convert an array of boolean values the size of an integer to an integer.

   function To_BCInteger is new Ada.Unchecked_Conversion
     (BCInteger_Boolean_Array_Type,
      BCInteger);

   -- Convert a modular to an array of boolean values the size of a modular.

   function BCModular_To_Array is new Ada.Unchecked_Conversion
     (BCModular,
      BCModular_Boolean_Array_Type);

   -- Convert an array of boolean values the size of a modular to a modular.

   function To_BCModular is new Ada.Unchecked_Conversion
     (BCModular_Boolean_Array_Type,
      BCModular);

   -- Return the number of boolean true values.

   function Count_Of (The_Array : Boolean_Array_Type) return Boolean_Count_Type;

   -- Convert a section of an array of boolean values starting at index with the
   -- size to an integer. Proprogate last bit (sign bit) to the higher bits.

   function BCInteger_At
     (The_Index      : Boolean_Index_Type;
      The_Size       : BCInteger_Size_Type;
      From_The_Array : Boolean_Array_Type) return BCInteger with
     Pre => From_The_Array'Length > 0
     and then The_Index in From_The_Array'Range
     and then The_Index + The_Size - 1 in From_The_Array'Range;

   -- Convert a section of an array of boolean values starting at index with
   -- the size to a modular. Set higher bits to 0.

   function BCModular_At
     (The_Index      : Boolean_Index_Type;
      The_Size       : BCModular_Size_Type;
      From_The_Array : Boolean_Array_Type) return BCModular with
     Pre => From_The_Array'Length > 0
     and then The_Index in From_The_Array'Range
     and then The_Index + The_Size - 1 in From_The_Array'Range;

   -- Return a string representation of boolean values for the boolean array.

   function Image_Of (The_Array : Boolean_Array_Type) return String;

end Boolean_Package;

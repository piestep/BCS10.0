-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--

package body Boolean_Package with
Spark_Mode => Off is

   -- Return the number of boolean true values.

   function Count_Of
     (The_Array : Boolean_Array_Type) return Boolean_Count_Type
   is
      The_Count : Boolean_Count_Type := 0;
   begin
      for I in The_Array'Range loop
         if The_Array (I) then
            The_Count := The_Count + 1;
         end if;
      end loop;
      return The_Count;
   end Count_Of;

   -- Convert a section of an array of boolean values starting at index with the
   -- size to an integer. Proprogate last bit (sign bit) to the higher bits.

   function BCInteger_At
     (The_Index      : Boolean_Index_Type;
      The_Size       : BCInteger_Size_Type;
      From_The_Array : Boolean_Array_Type) return BCInteger
   is
      The_Integer : BCInteger_Boolean_Array_Type :=
        (BCInteger_Boolean_Array_Type'Range => False);
   begin
      if From_The_Array (The_Index + The_Size - 1) then
         The_Integer := (BCInteger_Boolean_Array_Type'Range => True);
      end if;
      for I in Boolean_Index_Type range 0 .. The_Size - 1 loop
         The_Integer (I) := From_The_Array (The_Index + I);
      end loop;
      return To_BCInteger (The_Integer);
   end BCInteger_At;

   -- Convert a section of an array of boolean values starting at index with the
   -- size to a natural. Set higher bits to 0.

   function BCModular_At
     (The_Index      : Boolean_Index_Type;
      The_Size       : BCModular_Size_Type;
      From_The_Array : Boolean_Array_Type) return BCModular
   is
      The_Modular : BCModular_Boolean_Array_Type :=
        (BCModular_Boolean_Array_Type'Range => False);
   begin
      for I in Boolean_Index_Type range 0 .. The_Size - 1 loop
         The_Modular (I) := From_The_Array (The_Index + I);
      end loop;
      return To_BCModular (The_Modular);
   end BCModular_At;

   -- Return a string representation of boolean values for the boolean array.

   function Image_Of (The_Array : Boolean_Array_Type) return String is
      The_Length : constant SYSNatural      := The_Array'Length;
      The_String : String (1 .. The_Length) := (1 .. The_Length => ' ');
   begin
      if The_Length = 0 then
         return "*";
      end if;

      for I in The_Array'Range loop
         if The_Array (I) then
            The_String (Integer (I - The_Array'First) + 1) := '1';
         else
            The_String (Integer (I - The_Array'First) + 1) := '0';
         end if;
      end loop;
      return The_String;
   end Image_Of;

end Boolean_Package;

pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--

package body Variable_Package with
Spark_Mode => Off is

   -- The number of letters that can represent a variable.

   LETTERS : constant Positive := Character'Pos ('z') - Character'Pos ('a') + 1;


   procedure Include
     (The_Variable  :        Variable_Type;
      The_Variables : in out Variable_Set_Type)
   is
   begin
      The_Variables (The_Variable) := True;
   end Include;

   procedure Exclude
     (The_Variable  :        Variable_Type;
      The_Variables : in out Variable_Set_Type)
   is
   begin
      The_Variables (The_Variable) := False;
   end Exclude;

   -- Return a variable array  of variable set.

   function Array_Of (The_Set : Variable_Set_Type) return Variable_Array_Type is

      The_Length : constant Variable_Count_Type := Count_Of (The_Set);

      The_Array : Variable_Array_Type (Variable_Array_Index_Type'Range) :=
        (Variable_Array_Index_Type'Range => 0);

      The_Count : Variable_Count_Type := 0;

   begin
      for I in Variable_Index_Type'Range loop

         if The_Set (I) then
            The_Count             := The_Count + 1;
            The_Array (The_Count) := I;
         end if;
      end loop;

      return The_Array (1 .. The_Length);
   end Array_Of;

   -- Return the negation, 'not', of a set.

   function "not" (The_Right : Variable_Set_Type) return Variable_Set_Type is
   begin
      return Variable_Set_Type (not Boolean_Array_Type (The_Right));
   end "not";

   -- Return the intersection, 'and', of the two sets.

   function "and"
     (The_Left, The_Right : Variable_Set_Type) return Variable_Set_Type
   is
   begin
      return Variable_Set_Type
        (Boolean_Array_Type (The_Left) and Boolean_Array_Type (The_Right));
   end "and";

   -- Return the union, 'or', of the two sets.

   function "or"
     (The_Left, The_Right : Variable_Set_Type) return Variable_Set_Type
   is
   begin
      return Variable_Set_Type
        (Boolean_Array_Type (The_Left) or Boolean_Array_Type (The_Right));
   end "or";

   -- Return the exclusive or, 'xor', of the two sets.

   function "xor"
     (The_Left, The_Right : Variable_Set_Type) return Variable_Set_Type
   is
   begin
      return Variable_Set_Type
        (Boolean_Array_Type (The_Left) xor Boolean_Array_Type (The_Right));
   end "xor";

   -- Return a string representation for a variable.

   function Image_Of (The_Variable : Variable_Type) return String is
      The_Letter    : SYSNatural;
      The_Subscript : SYSNatural;
   begin
      The_Letter    := SYSNatural (The_Variable) mod LETTERS;
      The_Subscript := SYSNatural (The_Variable) / LETTERS;

      return Character'Val (Character'Pos ('a') + The_Letter) &
        SYSNatural'Image (The_Subscript)
        (2 .. SYSNatural'Image (The_Subscript)'Last);
   end Image_Of;

   -- Return a string representation of the not image for a variable.

   function Not_Image_Of (The_Variable : Variable_Type) return String is
      The_Letter    : SYSNatural;
      The_Subscript : SYSNatural;
   begin
      The_Letter    := SYSNatural (The_Variable) mod LETTERS;
      The_Subscript := SYSNatural (The_Variable) / LETTERS;

      return Character'Val (Character'Pos ('A') + The_Letter) &
        SYSNatural'Image (The_Subscript)
        (2 .. SYSNatural'Image (The_Subscript)'Last);
   end Not_Image_Of;

   -- Return a string representation of boolean values for the set of variables.

   function Boolean_Of (The_Set : Variable_Set_Type) return String is
      The_String : String (1 .. VARIABLES);
   begin
      for I in Variable_Set_Type'Range loop

         if The_Set (I) then
            The_String (I + 1) := '1';
         else
            The_String (I + 1) := '0';
         end if;
      end loop;

      return The_String;
   end Boolean_Of;

   -- Return a string representation for the set of variables.

   function Image_Of (The_Set : Variable_Set_Type) return String is
      The_Image : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Variable_Set_Type'Range loop

         if The_Set (I) = True then
            Append (The_Image, " " & Image_Of (I));
         end if;
      end loop;

      return To_String (The_Image);
   end Image_Of;

end Variable_Package;

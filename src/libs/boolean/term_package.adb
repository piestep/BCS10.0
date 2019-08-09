pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--

package body Term_Package with
Spark_Mode => Off is

   -- Return the boolean array representation for a fpt.

   function To_Boolean_Array (The_Term : Term_Type) return Boolean_Array_Type is
   begin
      return The_Term.The_Array (0 .. Length_Of (The_Term) - 1);
   end To_Boolean_Array;

   -- Convert a fpt representation to a fpt numeric representation.

   function To_Index (The_Term : Term_Type) return Index_Type is
      The_Index : Term_Array_Type := (Term_Index_Type'Range => False);
   begin
      The_Index (0 .. Length_Of (The_Term) - 1) :=
        The_Term.The_Array (0 .. Length_Of (The_Term) - 1);
      return Term_Array_Type_To_Index_Type (The_Index);
   end To_Index;

   -- Return the maximum + 1 of the fundamental product term, (2 ** length
   -- of fpt array). NOTE: Used to caluclate the length of the characteristic
   -- number.

   function Max_Of (The_Length : Term_Length_Type) return Index_Range_Type is
   begin
      return 2**The_Length;
   end Max_Of;

   -- Create a fpt to represent the number of variables. The fpt variable
   -- boolean values are set to false.

   procedure Create
     (The_Term                     : out Term_Type;
      With_The_Number_Of_Variables :     Term_Count_Type)
   is
   begin
      The_Term :=
        (The_Length => With_The_Number_Of_Variables,
         The_Array  => (Term_Index_Type => False));
   end Create;

   -- Create a fpt to represent the number of variables in the index.

   procedure Create
     (The_Term                     : out Term_Type;
      With_The_Number_Of_Variables :     Term_Count_Type;
      The_Value                    :     Index_Type)
   is
      The_Array : Term_Array_Type;
   begin
      The_Term :=
        (The_Length => With_The_Number_Of_Variables,
         The_Array  => (Term_Index_Type => False));

      The_Array := Index_Type_To_Term_Array_Type (The_Value);

      The_Term.The_Array (0 .. Length_Of (The_Term) - 1) :=
        The_Array (0 .. Length_Of (The_Term) - 1);
   end Create;

   -- Set the variable in the fpt to the value.

   procedure Set
     (The_Variable :        Term_Index_Type;
      In_The_Term  : in out Term_Type;
      To_The_Value :        Boolean)
   is
   begin
      In_The_Term.The_Array (The_Variable) := To_The_Value;
   end Set;

   -- Return true if the fpts are equal value.

   function Is_Equal (The_Left, The_Right : Term_Type) return Boolean is
   begin
      return The_Left.The_Length = The_Right.The_Length
        and then
          The_Left.The_Array (0 .. The_Left.The_Length - 1) =
        The_Right.The_Array (0 .. The_Right.The_Length - 1);
   end Is_Equal;

   -- Return a fpt (term) that is a subset of the fpt, 'The_Term', specified by
   -- a mask of boolean values where true represents a variable to be removed
   -- and false a variable to keep. The new fpt will have a length of the count
   -- of false values in the mask. The mask should be the same length as the
   -- fpt, 'The_Term'.

   function Index_Of
     (The_Term : Index_Type;
      The_Mask : Mask_Type) return Index_Type
   is
      The_Index : Term_Array_Type;
      The_Array : Term_Array_Type;
      The_Count : Term_Count_Type := 0;
   begin
      The_Index := (Term_Array_Type'Range => False);
      The_Array := Index_Type_To_Term_Array_Type (The_Term);

      -- loop for all variables in the mask (same length as the fpt, 'The_Term')

      for I in 0 .. Length_Of (The_Mask) - 1 loop

         -- if the mask is false then keep the variable's value of the the fpt,
         -- 'The_Term'.

         if not The_Mask.The_Array (I) then
            The_Index (The_Count) := The_Array (I);
            The_Count             := The_Count + 1;
         end if;
      end loop;

      return Term_Array_Type_To_Index_Type (The_Index);
   end Index_Of;

   -- Return the string representation of variables for a fpt.

   function Image_Of (The_Term : Term_Type) return String is

      The_Image : Unbounded_String := Null_Unbounded_String;

   begin
      for I in 0 .. Length_Of (The_Term) - 1 loop

         if The_Term.The_Array (I) then
            Append (The_Image, Image_Of (Variable_Type (I)));
         else
            Append (The_Image, Not_Image_Of (Variable_Type (I)));
         end if;
      end loop;

      return To_String (The_Image);
   end Image_Of;

   -- Return the string representation of variables using the array of variables
   -- for a fpt.

   function Image_Of
     (The_Term           : Term_Type;
      With_The_Variables : Variable_Array_Type) return String
   is

      The_Image : Unbounded_String := Null_Unbounded_String;

   begin
      for I in 0 .. Length_Of (The_Term) - 1 loop

         if The_Term.The_Array (I) then
            Append (The_Image, Image_Of (With_The_Variables (I + 1)));
         else
            Append (The_Image, Not_Image_Of (With_The_Variables (I + 1)));
         end if;
      end loop;

      return To_String (The_Image);
   end Image_Of;

   -- Return the string representation of boolean values for a fpt.

   function Boolean_Of (The_Term : Term_Type) return String is

      The_Image : Unbounded_String := Null_Unbounded_String;

   begin
      for I in 0 .. Length_Of (The_Term) - 1 loop

         if The_Term.The_Array (I) then
            Append (The_Image, '1');
         else
            Append (The_Image, '0');
         end if;
      end loop;

      return To_String (The_Image);
   end Boolean_Of;

end Term_Package;

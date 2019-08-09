pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Unchecked_Deallocation;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--

package body Number_Package with
Spark_Mode => Off is

   -- Create the cn with the length.

   procedure Create
     (The_Number : out Number_Type;
      The_Length :     Number_Length_Type)
   is
   begin
      The_Number := new Number_Array_Type (0 .. The_Length - 1);
      The_Number (0 .. The_Length - 1) := (0 .. The_Length - 1 => False);
   end Create;

   -- Create a cn which equals, f(a) = a.

   procedure Create (The_Number : out Number_Type) is
   begin
      Create (The_Number, 2);
      Include (0, The_Number, False);
      Include (1, The_Number, True);
   end Create;

   -- Create a cn for the constant, f() = 0 | 1.

   procedure Create (The_Number : out Number_Type; From_The_Value : Boolean) is
   begin
      Create (The_Number, 1);
      Include (0, The_Number, From_The_Value);
   end Create;

   -- Dispose a cn.

   procedure Dispose (The_Number : in out Number_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Number_Array_Type,
         Number_Type);
   begin
      Free (The_Number);
   end Dispose;

   -- Copy a Characteristic Number.

   procedure Copy
     (From_The_Number :     Number_Type;
      To_The_Number   : out Number_Type)
   is
   begin
      --        To_The_Number := new Number_Array_Type (From_The_Number'Range);
      Create (To_The_Number, Length_Of (From_The_Number));
      To_The_Number (From_The_Number'Range) :=
        From_The_Number (From_The_Number'Range);
   end Copy;

   -- Not a cn and return the result in the parameter.

   procedure Not_Op (The_Number : Number_Type) is
   begin
      The_Number.all := not The_Number.all;
   end Not_Op;

   -- And two cn and return the result in the first parameter.

   procedure And_Op (The_Number : Number_Type; With_The_Number : Number_Type) is
   begin
      The_Number.all := The_Number.all and With_The_Number.all;
   end And_Op;

   -- Or two cn and return the result in the first parameter.

   procedure Or_Op (The_Number : Number_Type; With_The_Number : Number_Type) is
   begin
      The_Number.all := The_Number.all or With_The_Number.all;
   end Or_Op;

   -- Xor two cn and return the result in the first parameter.

   procedure Xor_Op (The_Number : Number_Type; With_The_Number : Number_Type) is
   begin
      The_Number.all := The_Number.all xor With_The_Number.all;
   end Xor_Op;

   -- Normalize a cn to include the variables in the mask.

   procedure Normalize
     (The_Number   : in out Number_Type;
      For_The_Mask :        Mask_Type)
   is
      To_The_Number : Number_Type;
      The_Term      : Index_Type := 0;
   begin
      -- allocate new space for the cn.
      --        To_The_Number := new Number_Array_Type (0 .. 2**For_The_Mask'Length - 1);
      Create (To_The_Number, Max_Of (Length_Of (For_The_Mask)));

      -- loop over all fpts.
      for I in 0 .. Length_Of (To_The_Number) - 1 loop
         -- return the old fpt matching new fpt and assign its boolean value.

         The_Term          := Index_Of (I, For_The_Mask);
         To_The_Number (I) := The_Number (The_Term);
      end loop;

      Dispose (The_Number);

      The_Number := To_The_Number;
   end Normalize;

   -- Set the fpt in the cn.

   procedure Include
     (The_Term     : Term_Type;
      The_Number   : Number_Type;
      Include_Term : Boolean)
   is
   begin
      The_Number (To_Index (The_Term)) := Include_Term;
   end Include;

   -- Set the fpt in the cn.

   procedure Include
     (The_Index    : Index_Type;
      The_Number   : Number_Type;
      Include_Term : Boolean)
   is
   begin
      The_Number (The_Index) := Include_Term;
   end Include;

   -- Return true if all the fpts of the Characteristic Numbers are set to the
   -- value.

   function Is_Constant
     (The_Number : Number_Type;
      The_Value  : Boolean) return Boolean
   is
   begin
      for I in 0 .. Length_Of (The_Number) - 1 loop
         if The_Number (I) /= The_Value then
            return False;
         end if;
      end loop;
      return True;
   end Is_Constant;

   -- Return true if the cn is a constant. A constant is represented by all fpts
   -- being the same value, 0 or 1.

   function Is_Constant (The_Number : Number_Type) return Boolean is
      The_Value : Boolean;
   begin
      The_Value := The_Number (0);
      for I in The_Number'First + 1 .. The_Number'Last loop
         if The_Number (I) /= The_Value then
            return False;
         end if;
      end loop;
      return True;
   end Is_Constant;

   -- Return the string representation of boolean values for the cn.

   function Boolean_Of (The_Number : Number_Type) return String is
      The_Image : Unbounded_String := Null_Unbounded_String;
   begin
      for I in 0 .. Length_Of (The_Number) - 1 loop
         if The_Number (I) = True then
            Append (The_Image, "1");
         else
            Append (The_Image, "0");
         end if;
      end loop;

      return To_String (The_Image);
   end Boolean_Of;

   -- Return the string representation of variables for the cn. The variables
   -- are started at the smallest variable, a0.

   function Image_Of
     (The_Number : Number_Type;
      The_Size   : Variable_Count_Type) return String
   is
      The_Image : Unbounded_String := Null_Unbounded_String;
      The_Term  : Term_Type        := NULL_TERM;
   begin
      if Is_Constant (The_Number, True) then
         Append (The_Image, " 1");

      elsif Is_Constant (The_Number, False) then
         Append (The_Image, " 0");

      else
         for I in 0 .. Length_Of (The_Number) - 1 loop

            if The_Number (I) then
               Create (The_Term, The_Size, I);
               Append (The_Image, " " & Image_Of (The_Term));
            end if;
         end loop;
      end if;

      return To_String (The_Image);
   end Image_Of;

   -- Return the string representation of variables for the cn using the array
   -- of variables for the fpt variables.

   function Image_Of
     (The_Number         : Number_Type;
      With_The_Variables : Variable_Array_Type) return String
   is

      The_Image : Unbounded_String := Null_Unbounded_String;
      The_Term  : Term_Type        := NULL_TERM;

   begin
      if Is_Constant (The_Number, True) then
         Append (The_Image, " 1");

      elsif Is_Constant (The_Number, False) then
         Append (The_Image, " 0");

      else
         for I in 0 .. Length_Of (The_Number) - 1 loop

            if The_Number (I) then
               Create (The_Term, With_The_Variables'Length, I);
               Append (The_Image, " ");
               Append (The_Image, Image_Of (The_Term, With_The_Variables));
            end if;
         end loop;
      end if;

      return To_String (The_Image);
   end Image_Of;

end Number_Package;

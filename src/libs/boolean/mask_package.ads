-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
--

-- A variable length array boolean values where true represents the inclusion of
-- a value and false the exclusion of a value. NOTE: A mask can have 0 length to
-- denote an empty mask.
-- Implemented as a record with the array's length and a fixed length array of
-- boolean values.

package Mask_Package with
Spark_Mode => Off is

   -- Mask types.

   subtype Mask_Count_Type is Integer range 0 .. VARIABLES;
   subtype Mask_Length_Type is Mask_Count_Type;
   subtype Mask_Index_Type is Mask_Count_Type range 0 .. VARIABLES - 1;

   -- Mask array type.

   subtype Mask_Array_Type is Boolean_Array_Type (Mask_Index_Type'Range);

   -- The mask type.

   type Mask_Type is record
      The_Length : Mask_Count_Type := 0;
      The_Array  : Mask_Array_Type := (Mask_Index_Type'Range => False);
   end record;

   -- An empty mask.

   EMPTY_MASK : constant Mask_Type :=
     (The_Length => 0, The_Array => (Mask_Index_Type'Range => False));

   -- Return length, number of variables, for the mask.

   function Length_Of (The_Mask : Mask_Type) return Mask_Length_Type;

   -- Return number of true values in the mask.

   function Count_Of (The_Mask : Mask_Type) return Mask_Count_Type;

   -- Create a mask of false values for the length.

   function Create (With_The_Length : Mask_Length_Type) return Mask_Type with
     Post => Length_Of (Create'Result) = With_The_Length;

   -- Create false mask from a boolean Array.

   function Create (With_The_Array : Boolean_Array_Type) return Mask_Type with
     Pre => With_The_Array'First = 0
     and then With_The_Array'Length <= VARIABLES,
     Post => Length_Of (Create'Result) = With_The_Array'Length;

   -- Return a string representation of boolean values for the mask.

   function Image_Of (The_Mask : Mask_Type) return String;

private

   -- Return length, number of variables, for the mask.

   function Length_Of
     (The_Mask : Mask_Type) return Mask_Length_Type is
     (The_Mask.The_Length);

   -- Return number of true values in the mask.

   function Count_Of
     (The_Mask : Mask_Type) return Mask_Count_Type is
     (Boolean_Package.Count_Of (Boolean_Array_Type (The_Mask.The_Array)));

   -- Create a mask of false values for the length.

   function Create
     (With_The_Length : Mask_Length_Type) return Mask_Type is
     ((The_Length => With_The_Length,
       The_Array  => (Mask_Index_Type'Range => False)));

   -- Return a string representation of boolean values for the mask.

   function Image_Of
     (The_Mask : Mask_Type) return String is
     (Image_Of
        (Boolean_Array_Type
             (The_Mask.The_Array (0 .. Length_Of (The_Mask) - 1))));

end Mask_Package;

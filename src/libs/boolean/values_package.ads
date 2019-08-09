-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
--

-- A variable length array Variable boolean values where true represents a
-- value of true and false a value of false for that Variable. NOTE: Values
-- may have 0 length to denote an no values.
-- Implemented as a record with the array's length and a fixed length array of
-- boolean values.

package Values_Package with
Spark_Mode => Off is

   -- Values types.

   subtype Values_Count_Type is SYSNatural range 0 .. VARIABLES;
   subtype Values_Size_Type is Values_Count_Type;
   subtype Values_Index_Type is Values_Count_Type range 0 .. VARIABLES - 1;

   -- A boolean array representation for values.

   subtype Values_Array is Boolean_Array_Type (Values_Index_Type'Range);

   -- The Values_Type type.

   type Values_Type is record
      The_Length : Values_Count_Type := 0;
      The_Array  : Values_Array      := (Values_Index_Type'Range => False);
   end record;

   -- An empty values.

   EMPTY_VALUES : constant Values_Type :=
     (The_Length => 0, The_Array => (Values_Index_Type'Range => False));

   -- Return length, number of variables, for values.

   function Length_Of (The_Values : Values_Type) return Values_Count_Type;

   -- Create false variables values for the number of variables.

   function Create
     (With_The_Number_Of_Variables : Values_Size_Type) return Values_Type with
     Post => Length_Of (Create'Result) = With_The_Number_Of_Variables;

   -- Create values from a boolean Array.

   function Create (With_The_Array : Boolean_Array_Type) return Values_Type with
     Pre => With_The_Array'First = 0
     and then With_The_Array'Length <= VARIABLES,
     Post => Length_Of (Create'Result) = With_The_Array'Length;

   -- Return concatenation of the two values

   function "&"
     (The_Left  : Values_Type;
      The_Right : Values_Type) return Values_Type with
     Pre  => Length_Of (The_Left) + Length_Of (The_Right) <= VARIABLES,
     Post => Length_Of ("&"'Result) =
     Length_Of (The_Left) + Length_Of (The_Right);

   -- Return a string representation of boolean values for the values.

   function Image_Of (The_Values : Values_Type) return String;

private

   function Length_Of
     (The_Values : Values_Type) return Values_Count_Type is
     (The_Values.The_Length);

   -- Create values of variables.

   function Create
     (With_The_Number_Of_Variables : Values_Size_Type) return Values_Type is
     ((The_Length => With_The_Number_Of_Variables,
       The_Array  => (Values_Index_Type'Range => False)));

   -- Return a string representation of boolean values for the values.

   function Image_Of
     (The_Values : Values_Type) return String is
     (Image_Of
        (Boolean_Array_Type
             (The_Values.The_Array (0 .. Length_Of (The_Values) - 1))));

end Values_Package;

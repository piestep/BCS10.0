-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Unchecked_Conversion;
--
with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
with Mask_Package;     use Mask_Package;

-- A package to define a boolean algabric fundamental product term (fpt).
--
-- A fpt is a product of variables and can be represented as a numeric or array
-- of boolean values. The boolean value true represents the product of the
-- variable and the boolean value false represents the product of the not of
-- the variable. Note: A fpt is allowed to have 0 length. The Term Index of a
-- 0 length fpt is always 0.
--
-- This package requires a maximum size for fundamental product terms.

package Term_Package with
Spark_Mode => Off is

   -- Maximum size of a fpt boolean array.

   TERMS    : constant := VARIABLES;
   MAX_TERM : constant := 2**TERMS - 1;

   -- Term types.

   subtype Term_Count_Type is SYSNatural range 0 .. VARIABLES;
   subtype Term_Length_Type is Term_Count_Type;
   subtype Term_Index_Type is Term_Count_Type range 0 .. VARIABLES - 1;

   -- Index term types.

   subtype Index_Range_Type is SYSNatural range 1 .. 2**VARIABLES;

   -- The numeric type representation of a fpt.

   subtype Index_Type is SYSNatural range 0 .. 2**VARIABLES - 1;

   -- The Fundamental Product Term type.

   type Term_Type is private;

   -- A null fpt.

   NULL_TERM : constant Term_Type;

   -- Return length, number of variables, for the fpt.

   function Length_Of (The_Term : Term_Type) return Term_Length_Type;

   -- Return the boolean array representation for a fpt.

   function To_Boolean_Array (The_Term : Term_Type) return Boolean_Array_Type;

   -- Convert a fpt representation to a fpt numeric representation.

   function To_Index (The_Term : Term_Type) return Index_Type;

   -- Return the maximum + 1 of the fundamental product term, (2 ** length
   -- of fpt array). NOTE: Used to caluclate and verify the length of the
   -- characteristic number.

   function Max_Of (The_Length : Term_Length_Type) return Index_Range_Type;

   -- Create a fpt to represent the number of variables. The fpt variable
   -- boolean values are set to false.

   procedure Create
     (The_Term                     : out Term_Type;
      With_The_Number_Of_Variables :     Term_Count_Type);

   -- Create a fpt to represent the number of variables in the index.

   procedure Create
     (The_Term                     : out Term_Type;
      With_The_Number_Of_Variables :     Term_Count_Type;
      The_Value                    :     Index_Type);

   -- Set the variable in the fpt to the value.

   procedure Set
     (The_Variable :        Term_Index_Type;
      In_The_Term  : in out Term_Type;
      To_The_Value :        Boolean) with
     Pre => Length_Of (In_The_Term) > 0
     and then The_Variable < Length_Of (In_The_Term);

   -- Return true if the variable is set to true in the fpt.

   function Is_Set
     (The_Variable : Term_Index_Type;
      In_The_Term  : Term_Type) return Boolean with
     Pre => Length_Of (In_The_Term) > 0
     and then The_Variable < Length_Of (In_The_Term);

   -- Return true if the fpts are equal value.

   function Is_Equal (The_Left, The_Right : Term_Type) return Boolean with
     Pre => Length_Of (The_Left) = Length_Of (The_Right);

   -- Return a fpt that is a subset specified by a mask of the fpt
   -- (The_Index). NOTE: exposed for testing purposes.

   function Index_Of
     (The_Term : Index_Type;
      The_Mask : Mask_Type) return Index_Type;

   -- Return the string representation of variables for a fpt.

   function Image_Of (The_Term : Term_Type) return String;

   -- Return the string representation of variables using the array of variables
   -- for a fpt.

   function Image_Of
     (The_Term           : Term_Type;
      With_The_Variables : Variable_Array_Type) return String;

   -- Return the string representation of boolean values for a fpt.

   function Boolean_Of (The_Term : Term_Type) return String;

private

   -- A boolean value array representation of a fpt.

   subtype Term_Array_Type is Boolean_Array_Type (Term_Index_Type'Range);

   -- The Fundamental Product Term type.
   -- Implemented as a record with the array's length and a fixed length
   -- array of boolean values.

   type Term_Type is record
      The_Length : Term_Count_Type := 0;
      The_Array  : Term_Array_Type := (Term_Index_Type'Range => False);
   end record;

   -- A null fpt.

   NULL_TERM : constant Term_Type :=
     (The_Length => 0, The_Array => (Term_Index_Type => False));

   -- Return size, number of variables, for the fpt.

   function Length_Of
     (The_Term : Term_Type) return Term_Count_Type is
     (The_Term.The_Length);

   -- Return true if the variable is set to true in the fpt.

   function Is_Set
     (The_Variable : Term_Index_Type;
      In_The_Term  : Term_Type) return Boolean is
     (In_The_Term.The_Array (The_Variable));

   -- Convert a fpt boolean value array representation to a fpt numeric
   -- representation.

   function Term_Array_Type_To_Index_Type is new Ada.Unchecked_Conversion
     (Term_Array_Type,
      Index_Type);

   -- Convert a fpt numeric representation to a boolean value array
   -- representation.

   function Index_Type_To_Term_Array_Type is new Ada.Unchecked_Conversion
     (Index_Type,
      Term_Array_Type);

end Term_Package;

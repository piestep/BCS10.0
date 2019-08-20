-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Unchecked_Conversion;
with Ada.Containers.Vectors;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Boolean_Package; use Boolean_Package;
with Word_Package;    use Word_Package;
--

-- A package to define BC boolean code (bcode). BCode is a list of
-- program source output values defined by boolean equations.

package BCode_Package is

   -- BCode boolean equations coorasponding to output values.

   type BCode_Words is private;

   -- Dispose the words.

   procedure Dispose (The_Words : in out BCode_Words);

   -- Append the word to the bcode words.

   procedure Append (The_Words : in out BCode_Words; The_Word : Word_Type);

   -- Normalize the boolean equations with all its variables.

   procedure Normalize (The_Words : in out BCode_Words);

   -- Return number of boolean equations.

   function Number_Of (The_Words : BCode_Words) return Natural;

   -- Return bcode word solutions for the variable values.

   function Solve
     (The_Words       : BCode_Words;
      With_The_Values : Boolean_Array_Type) return Boolean_Array_Type;

   -- Return the string representation of boolean word values for the words
   -- seperated by a comma.

   function Boolean_Of (The_Words : BCode_Words) return String;

   -- Return the string representation of fundamental product terms
   -- for the words seperated by a comma.

   function Image_Of (The_Words : BCode_Words) return String;

   -- Iterate over the bcode words applying the procedure on each.

   procedure Iterate
     (The_Words : BCode_Words;
      Process   : not null access procedure (The_Word : Word_Type));

private

   package Vector_Package is new Ada.Containers.Vectors
     (Natural,
      Word_Type);
   use Vector_Package;

   type BCode_Words is new Vector with null record;

end BCode_Package;

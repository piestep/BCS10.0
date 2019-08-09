-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package;  use System_Package;
with Boolean_Package; use Boolean_Package;
--

-- A package to define boolean variables and associated types.
--
-- A variable is a discreate type.
--
-- Other associate types are:
--
-- An array of variables.
--
-- A set of variables. The set of variables is a bound set of size MAX_VARIABLE.
--
-- An image of a variable is a small letter followed by an integer. The not
-- image of a variable is it's associated large letter followed by an integer.

package Variable_Package with
     Spark_Mode => Off is

   -- Variable constants.

   VARIABLES    : constant := 30;
   MIN_VARIABLE : constant := 0;
   MAX_VARIABLE : constant := VARIABLES - 1;

   -- Variable types.

   subtype Variable_Count_Type is Boolean_Count_Type range 0 .. VARIABLES;
   subtype Variable_Index_Type is Variable_Count_Type range 0 .. VARIABLES - 1;

   -- The Variable Type.

   subtype Variable_Type is SYSNatural range 0 .. MAX_VARIABLE;

   -- An array of variables type.

   type Variable_Set_Type is private;

   -- Variable array types.

   subtype Variable_Array_Index_Type is
     Variable_Count_Type range 1 .. VARIABLES;

   -- Variable array type.

   type Variable_Array_Type is
     array (Variable_Array_Index_Type range <>) of Variable_Type;

   --Empty variable set.

   EMPTY_SET : constant Variable_Set_Type;

   -- Return number of members in the set.

   function Count_Of (The_Set : Variable_Set_Type) return Variable_Count_Type;

   -- Return true if the variable is included in the set.

   function Is_Included
     (The_Variable  : Variable_Type;
      The_Variables : Variable_Set_Type) return Boolean;

   -- Include the variable in the set.

   procedure Include
     (The_Variable  :        Variable_Type;
      The_Variables : in out Variable_Set_Type);

   -- Exnclude (remove) the variable from the set.

   procedure Exclude
     (The_Variable  :        Variable_Type;
      The_Variables : in out Variable_Set_Type);

   -- Return an array of variables of the set of variables.

   function Array_Of (The_Set : Variable_Set_Type) return Variable_Array_Type;

   -- Return the negation, 'not', of a set.

   function "not" (The_Right : Variable_Set_Type) return Variable_Set_Type;

   -- Return the intersection, 'and', of the two sets.

   function "and"
     (The_Left, The_Right : Variable_Set_Type) return Variable_Set_Type;

   -- Return the union, 'or', of the two sets.

   function "or"
     (The_Left, The_Right : Variable_Set_Type) return Variable_Set_Type;

   -- Return the exclusive or, 'xor', of the two sets.

   function "xor"
     (The_Left, The_Right : Variable_Set_Type) return Variable_Set_Type;

   -- Return a string representation for a variable.

   function Image_Of (The_Variable : Variable_Type) return String;

   -- Return a string representation of the not image for a variable.

   function Not_Image_Of (The_Variable : Variable_Type) return String;

-- Return a string representation of boolean values for the set of variables.

   function Boolean_Of (The_Set : Variable_Set_Type) return String;

   -- Return a string representation for the set of variables.

   function Image_Of (The_Set : Variable_Set_Type) return String;

private

   type Variable_Set_Type is new Boolean_Array_Type (Variable_Index_Type'Range);

   EMPTY_SET : constant Variable_Set_Type := (Variable_Set_Type'Range => False);

   -- Return number of members in the set.

   function Count_Of (The_Set : Variable_Set_Type) return Variable_Count_Type is
     (Count_Of (Boolean_Array_Type (The_Set)));

   -- Return true if the variable is included in the set.

   function Is_Included
     (The_Variable  : Variable_Type;
      The_Variables : Variable_Set_Type) return Boolean is
     (The_Variables (The_Variable));

end Variable_Package;

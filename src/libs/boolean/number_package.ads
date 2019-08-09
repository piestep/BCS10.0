-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Conversion;
--
with Pool_Package;
--
with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
with Mask_Package;     use Mask_Package;
with Term_Package;     use Term_Package;

-- A package to define a boolean algabric characteristic number.
--
-- A characteristic number (cn) is a sum of products boolean equation of the
-- equation's fundamental product terms (fpt). The cn can be represented as
-- an array of boolean values where the index into the array is the numeric
-- representation of a fpt and where a true value represents that a fpt is a
-- product of the equation's sum and a false value represents that the fpt is
-- not a product of the equation's sum.
--
-- Characteristic Numbers operations, not, and, or, xor, returns the result in
-- the first parameter changing the original parameter.

package Number_Package with
     Spark_Mode => Off is

   -- Characteristic number memory storage pool.

   The_Pool : Pool_Package.Storage_Pool;

   -- Charaistic number types.

   subtype Number_Count_Type is Integer range 0 .. 2**VARIABLES;
   subtype Number_Length_Type is Number_Count_Type range 1 .. 2**VARIABLES;
   subtype Number_Index_Type is Number_Count_Type range 0 .. 2**VARIABLES - 1;

   -- The characteristic number type.

   type Number_Type is private;

   -- Return the length of the characterisitc number. Note: May return 0 length
   -- for disposed numbers.

   function Length_Of (The_Number : Number_Type) return Number_Count_Type;

   -- Create a cn which equals, f(a) = a.

   procedure Create (The_Number : out Number_Type);

   -- Create a cn for the constant, f() = 0 | 1.

   procedure Create (The_Number : out Number_Type; From_The_Value : Boolean);

   -- Dispose a cn.

   procedure Dispose (The_Number : in out Number_Type);

   -- Copy a Characteristic Number.

   procedure Copy
     (From_The_Number :     Number_Type;
      To_The_Number   : out Number_Type);

   -- Not a cn and return the result in the parameter.

   procedure Not_Op (The_Number : Number_Type);

   -- And two cn and return the result in the first parameter.

   procedure And_Op (The_Number : Number_Type; With_The_Number : Number_Type);

   -- Or two cn and return the result in the first parameter.

   procedure Or_Op (The_Number : Number_Type; With_The_Number : Number_Type);

   -- Xor two cn and return the result in the first parameter.

   procedure Xor_Op (The_Number : Number_Type; With_The_Number : Number_Type);

   -- Normalize a cn to include the variables in the mask.

   procedure Normalize
     (The_Number   : in out Number_Type;
      For_The_Mask :        Mask_Type);

   -- Return true if the fpt is in the cn.

   function Is_Included
     (The_Term   : Term_Type;
      The_Number : Number_Type) return Boolean;

   -- Set the fpt in the cn.

   procedure Include
     (The_Term     : Term_Type;
      The_Number   : Number_Type;
      Include_Term : Boolean);

   -- Return true if the fpt is in the cn.

   function Is_Included
     (The_Index  : Index_Type;
      The_Number : Number_Type) return Boolean;

   -- Set the fpt in the cn.

   procedure Include
     (The_Index    : Index_Type;
      The_Number   : Number_Type;
      Include_Term : Boolean);

   -- Return true if all the fpts of the Characteristic Numbers are set to the
   -- value.

   function Is_Constant
     (The_Number : Number_Type;
      The_Value  : Boolean) return Boolean;

   -- Return true if the cn is a constant. A constant is represented by all fpts
   -- being the same value, 0 or 1.

   function Is_Constant (The_Number : Number_Type) return Boolean;

   -- Return constant value of a constant cn. Return a constant value of true
   -- if all fpts are equal to 1 and a constant value of false if all fpts are
   -- equal to 0.

   function To_Constant (The_Number : Number_Type) return Boolean;

   -- Return the string representation of boolean values for the cn.

   function Boolean_Of (The_Number : Number_Type) return String;

   -- Return the string representation of variables for the cn. The variables
   -- are started at the smallest variable, a0.

   function Image_Of
     (The_Number : Number_Type;
      The_Size   : Variable_Count_Type) return String;

   -- Return the string representation of variables for the cn using the array
   -- of variables for the fpt variables.

   function Image_Of
     (The_Number         : Number_Type;
      With_The_Variables : Variable_Array_Type) return String;

private

   -- A boolean value array to hold a characteristic number.

   type Number_Array_Type is array (Index_Type range <>) of Boolean;
   pragma Pack (Number_Array_Type);

   -- The characteristic number type.

   type Number_Type is access Number_Array_Type;
   for Number_Type'Storage_Pool use The_Pool;

   -- Create the cn with the length.

   procedure Create
     (The_Number : out Number_Type;
      The_Length :     Number_Length_Type);

   -- Return the length of the characterisitc number. Note: May return 0 length
   -- for disposed numbers.

   function Length_Of
     (The_Number : Number_Type) return Number_Count_Type is
     (The_Number'Length);

   -- Return true if the fpt is in the cn.

   function Is_Included
     (The_Term   : Term_Type;
      The_Number : Number_Type) return Boolean is
     (The_Number (To_Index (The_Term)));

   -- Return true if the fpt is in the cn.

   function Is_Included
     (The_Index  : Index_Type;
      The_Number : Number_Type) return Boolean is
     (The_Number (The_Index));

   -- Return constant value of a constant cn. Return a constant value of true
   -- if all fpts are equal to 1 and a constant value of false if all fpts are
   -- equal to 0.

   function To_Constant
     (The_Number : Number_Type) return Boolean is
     (The_Number.all = Number_Array_Type'(The_Number'Range => True));

end Number_Package;

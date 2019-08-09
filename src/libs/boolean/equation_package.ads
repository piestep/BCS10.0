-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package;   use System_Package;
with Boolean_Package;  use Boolean_Package;
with Variable_Package; use Variable_Package;
with Mask_Package;     use Mask_Package;
with Term_Package;     use Term_Package;
with Number_Package;   use Number_Package;
--

-- A package to define a boolean equation (be).
--
-- A be is a boolean algbraic equation with a set of variables and a
-- characteristic number.
--
-- Boolean equation operations, not, and, or, xor, returns the result in the
-- first parameter disposing of the original parameter.

package Equation_Package with
     Spark_Mode => Off is

   -- The boolean equation record.

   type Equation_Type is private;

   -- Return the length of the boolean equations characteristic number.

   function Length_Of (The_Equation : Equation_Type) return Number_Count_Type;

   -- Return set of variables for boolean equation.

   function Variables_Of
     (The_Equation : Equation_Type) return Variable_Set_Type;

   -- Return the characteristic number of the boolean equation.

   function Number_Of (The_Equation : Equation_Type) return Number_Type;

   -- Return number of variables in the equation.

   function Count_Of (The_Equation : Equation_Type) return Term_Count_Type;

   -- Create a constant equation of boolean value, f() = 0 | 1.

   procedure Create (The_Equation : out Equation_Type; The_Value : Boolean);

   -- Create an equation for the variable, f(a) = a.

   procedure Create
     (The_Equation : out Equation_Type;
      The_Variable :     Variable_Type);

   -- Dispose an equation.

   procedure Dispose (The_Equation : in out Equation_Type);

   -- Copy an equation.

   procedure Copy
     (From_The_Equation :     Equation_Type;
      To_The_Equation   : out Equation_Type);

   -- Not the equation and return the result in the parameter.

   procedure Not_Op (The_Equation : in out Equation_Type);

   -- And two equations and return result in the first parameter.

   procedure And_Op
     (The_Equation      : in out Equation_Type;
      With_The_Equation :        Equation_Type);

   -- Or two equations and return result in the first parameter.

   procedure Or_Op
     (The_Equation      : in out Equation_Type;
      With_The_Equation :        Equation_Type);

   -- Xor two equations and return result in the first parameter.

   procedure Xor_Op
     (The_Equation      : in out Equation_Type;
      With_The_Equation :        Equation_Type);

   -- Normalize the equation with the variables.

   procedure Normalize
     (The_Equation       : in out Equation_Type;
      With_The_Variables :        Variable_Set_Type);

   -- Return true if all fundamental product terms are set to the value.

   function Is_Constant
     (The_Equation : Equation_Type;
      The_Value    : Boolean) return Boolean;

   -- Return true if the equation is a constant.

   function Is_Constant (The_Equation : Equation_Type) return Boolean;

   -- Return the constant value of the constant equation.

   function To_Constant (The_Equation : Equation_Type) return Boolean;

   -- Return boolean equation solution for the variable values.

   function Solve
     (The_Equation    : Equation_Type;
      With_The_Values : Boolean_Array_Type) return Boolean;

   -- Return the string representation of boolean values for the equation.

   function Boolean_Of (The_Equation : Equation_Type) return String;

   -- Return the string representation of fundamental product terms for the
   -- equation.

   function Image_Of (The_Equation : Equation_Type) return String;

private

   -- The boolean equation record.

   type Equation_Type is record
      The_Variables : Variable_Set_Type;      -- The be's set of variables.
      The_Number    : Number_Type; -- The be's characteristic number.
   end record;

   -- Return a characteristic number (cn) mask of the two sets for cn
   -- normalization. Include a variable in the mask if in the union and not
   -- the set, (The_Union xor The_Set) and The_Union.

   function Mask_Of
     (The_Union : Variable_Set_Type;  -- the union of the two BE set of variables.
      The_Set   : Variable_Set_Type)    -- one of the two BE set of variables.
      return Mask_Type;

   -- Return the length of the boolean equations characteristic number.

   function Length_Of
     (The_Equation : Equation_Type) return Number_Count_Type is
     (Length_Of (The_Equation.The_Number));

   -- Return set of variables for boolean equation.

   function Variables_Of
     (The_Equation : Equation_Type) return Variable_Set_Type is
     (The_Equation.The_Variables);

   -- Return the characteristic number of the boolean equation.

   function Number_Of
     (The_Equation : Equation_Type) return Number_Type is
     (The_Equation.The_Number);

   -- Return number of variables in the equation.

   function Count_Of
     (The_Equation : Equation_Type) return Variable_Count_Type is
     (Count_Of (The_Equation.The_Variables));

   -- Return true if all fundamental product terms are set to the value.

   function Is_Constant
     (The_Equation : Equation_Type;
      The_Value    : Boolean) return Boolean is
     (Is_Constant (The_Equation.The_Number, The_Value));

   -- Return true if the equation is a constant.

   function Is_Constant
     (The_Equation : Equation_Type) return Boolean is
     (Is_Constant (The_Equation.The_Number));

   -- Return the constant value of the constant equation.

   function To_Constant
     (The_Equation : Equation_Type) return Boolean is
     (To_Constant (The_Equation.The_Number));

end Equation_Package;

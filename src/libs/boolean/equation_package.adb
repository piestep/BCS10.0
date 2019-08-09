pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Unchecked_Deallocation;
--
with System_Package; use System_Package;
with Term_Package;   use Term_Package;
--

package body Equation_Package with
Spark_Mode => Off is

   -- Return a characteristic number (cn) mask of the two sets for CN
   -- normalization. Note: returns null mask when union and set are empty sets

   function Mask_Of
     (The_Union : Variable_Set_Type;  -- the union of the two BE set of variables.
      The_Set   : Variable_Set_Type)  -- one of the two BE set of variables.
      return Mask_Type
   is
      The_Index : Term_Count_Type := 0;
      The_Mask  : Mask_Type;
      The_Count : Term_Count_Type;

   begin
      The_Count := Count_Of (The_Union);
      The_Mask  := (The_Count, (Mask_Index_Type'Range => False));

      -- include a variable in the mask if in the union and not the set.

      for The_Variable in Variable_Type'First .. Variable_Type'Last loop

         if Is_Included (The_Variable, The_Union) then

            if not Is_Included (The_Variable, The_Set) then
               The_Mask.The_Array (The_Index) := True;
            end if;

            The_Index := The_Index + 1;
            exit when The_Index = The_Count;
         end if;
      end loop;

      return The_Mask;
   end Mask_Of;

   -- Create a constant equation of boolean value, f() = 0 | 1.

   procedure Create (The_Equation : out Equation_Type; The_Value : Boolean) is
   begin
      The_Equation.The_Variables := EMPTY_SET;
      Create (The_Equation.The_Number, The_Value);
   end Create;

   -- Create an equation for the variable, f(a) = a.

   procedure Create
     (The_Equation : out Equation_Type;
      The_Variable :     Variable_Type)
   is
   begin
      The_Equation.The_Variables := EMPTY_SET;
      Include (The_Variable, The_Equation.The_Variables);
      Create (The_Equation.The_Number);
   end Create;

   -- Dispose an equation.

   procedure Dispose (The_Equation : in out Equation_Type) is
   begin
      Dispose (The_Equation.The_Number);
   end Dispose;

   -- Copy an equation.

   procedure Copy
     (From_The_Equation :     Equation_Type;
      To_The_Equation   : out Equation_Type)
   is
   begin
      To_The_Equation.The_Variables := From_The_Equation.The_Variables;
      Copy (From_The_Equation.The_Number, To_The_Equation.The_Number);
   end Copy;

   -- Not the equation and return the result in the parameter.

   procedure Not_Op (The_Equation : in out Equation_Type) is
   begin
      Not_Op (The_Equation.The_Number);
   end Not_Op;

   -- And two equations and return result in the first parameter.

   procedure And_Op
     (The_Equation      : in out Equation_Type;
      With_The_Equation :        Equation_Type)
   is

      The_Variables : Variable_Set_Type := EMPTY_SET;
      The_Number    : Number_Type;
   begin
      if The_Equation.The_Variables = With_The_Equation.The_Variables then
         And_Op (The_Equation.The_Number, With_The_Equation.The_Number);
      else
         -- normalize the two equations.

         The_Variables := The_Equation.The_Variables;

         The_Equation.The_Variables :=
           The_Equation.The_Variables or With_The_Equation.The_Variables;

         Copy (With_The_Equation.The_Number, The_Number);

         Normalize
           (The_Equation.The_Number,
            Mask_Of (The_Equation.The_Variables, The_Variables));

         Normalize
           (The_Number,
            Mask_Of
              (The_Equation.The_Variables,
               With_The_Equation.The_Variables));

         -- and

         And_Op (The_Equation.The_Number, The_Number);

         Dispose (The_Number);
      end if;
   end And_Op;

   -- Or two equations and return result in the first parameter.

   procedure Or_Op
     (The_Equation      : in out Equation_Type;
      With_The_Equation :        Equation_Type)
   is

      The_Variables : Variable_Set_Type := EMPTY_SET;
      The_Number    : Number_Type;

   begin
      if The_Equation.The_Variables = With_The_Equation.The_Variables then
         Or_Op (The_Equation.The_Number, With_The_Equation.The_Number);
      else
         -- normalize the two equations.

         The_Variables := The_Equation.The_Variables;

         The_Equation.The_Variables :=
           The_Equation.The_Variables or With_The_Equation.The_Variables;

         Copy (With_The_Equation.The_Number, The_Number);

         Normalize
           (The_Equation.The_Number,
            Mask_Of (The_Equation.The_Variables, The_Variables));

         Normalize
           (The_Number,
            Mask_Of
              (The_Equation.The_Variables,
               With_The_Equation.The_Variables));

         -- or

         Or_Op (The_Equation.The_Number, The_Number);

         Dispose (The_Number);
      end if;
   end Or_Op;

   -- Xor two equations and return result in the first parameter.

   procedure Xor_Op
     (The_Equation      : in out Equation_Type;
      With_The_Equation :        Equation_Type)
   is

      The_Variables : Variable_Set_Type := EMPTY_SET;
      The_Number    : Number_Type;

   begin
      if The_Equation.The_Variables = With_The_Equation.The_Variables then
         Xor_Op (The_Equation.The_Number, With_The_Equation.The_Number);
      else
         -- normalize the two equations.

         The_Variables := The_Equation.The_Variables;

         The_Equation.The_Variables :=
           The_Equation.The_Variables or With_The_Equation.The_Variables;

         Copy (With_The_Equation.The_Number, The_Number);

         Normalize
           (The_Equation.The_Number,
            Mask_Of (The_Equation.The_Variables, The_Variables));

         Normalize
           (The_Number,
            Mask_Of
              (The_Equation.The_Variables,
               With_The_Equation.The_Variables));

         -- xor

         Xor_Op (The_Equation.The_Number, The_Number);

         Dispose (The_Number);
      end if;
   end Xor_Op;

   -- Normalize the equation with the variables.

   procedure Normalize
     (The_Equation       : in out Equation_Type;
      With_The_Variables :        Variable_Set_Type)
   is
      The_Variables : Variable_Set_Type := EMPTY_SET;
   begin
      if The_Equation.The_Variables /= With_The_Variables then
         The_Variables := The_Equation.The_Variables;

         The_Equation.The_Variables :=
           The_Equation.The_Variables or With_The_Variables;

         Normalize
           (The_Equation.The_Number,
            Mask_Of (The_Equation.The_Variables, The_Variables));
      end if;
   end Normalize;

   -- Return boolean equation solution for the variable values.

   function Solve
     (The_Equation    : Equation_Type;
      With_The_Values : Boolean_Array_Type) return Boolean
   is
      The_Count  : Term_Count_Type;
      The_Term   : Term_Type;
      The_Offset : Term_Index_Type := 0;
   begin
      The_Count := Count_Of (The_Equation.The_Variables);

      if The_Count > 0 then
         Create (The_Term, The_Count);

         for I in 0 .. With_The_Values'Length - 1 loop

            if Is_Included (I, The_Equation.The_Variables) then
               Set (The_Offset, The_Term, With_The_Values (I));
               The_Offset := The_Offset + 1;
            end if;
         end loop;

         return Is_Included (The_Term, The_Equation.The_Number);
      else
         return Is_Included (0, The_Equation.The_Number);
      end if;
   end Solve;

   -- Return the string representation of boolean values for the equation.

   function Boolean_Of (The_Equation : Equation_Type) return String is
   begin
      return "f(" &
        Image_Of (The_Equation.The_Variables) &
        " ) " &
        Boolean_Of (The_Equation.The_Number);
   end Boolean_Of;

   -- Return the string representation of fundamental product terms for the
   -- equation.

   function Image_Of (The_Equation : Equation_Type) return String is
   begin
      return "f(" &
        Image_Of (The_Equation.The_Variables) &
        " )" &
        Image_Of
        (The_Equation.The_Number,
         Array_Of (The_Equation.The_Variables));
   end Image_Of;

end Equation_Package;

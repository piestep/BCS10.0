-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

package body Values_Package with
Spark_Mode => Off is

   -- Create values of variables from a boolean Array.

   function Create (With_The_Array : Boolean_Array_Type) return Values_Type is
      The_Values : Values_Type;
   begin
      if With_The_Array'Length = 0 then
         The_Values := EMPTY_VALUES;
      else
         The_Values := Create (With_The_Array'Length);
         The_Values.The_Array (0 .. With_The_Array'Last) :=
           With_The_Array (0 .. With_The_Array'Last);
      end if;

      return The_Values;
   end Create;

   function "&"
     (The_Left  : Values_Type;
      The_Right : Values_Type) return Values_Type
   is
      The_Result : Values_Type;
   begin
      The_Result :=
        (The_Length => Length_Of (The_Left) + Length_Of (The_Right),
         The_Array  => The_Left.The_Array);

      The_Result.The_Array
        (Length_Of (The_Left) ..
             Length_Of (The_Left) + Length_Of (The_Right) - 1) :=
          The_Right.The_Array (0 .. Length_Of (The_Right) - 1);

      return The_Result;
   end "&";

end Values_Package;

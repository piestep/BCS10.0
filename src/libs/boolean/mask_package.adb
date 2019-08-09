pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

package body Mask_Package with
Spark_Mode => Off is

   -- Create false mask from a boolean Array.

   function Create (With_The_Array : Boolean_Array_Type) return Mask_Type is
      The_Mask : Mask_Type;
   begin
      if With_The_Array'Length = 0 then
         The_Mask := EMPTY_MASK;
      else
         The_Mask := Create (With_The_Array'Length);
         The_Mask.The_Array (0 .. With_The_Array'Last) :=
           With_The_Array (0 .. With_The_Array'Last);
      end if;

      return The_Mask;
   end Create;

end Mask_Package;

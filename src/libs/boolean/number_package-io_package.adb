pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

-- Read/write the length of the characteristic number to the file followed by
-- the boolean array values of the characteristic number. Format: <length of
-- characteristic number> { <boolean values> }

package body Number_Package.IO_Package with
Spark_Mode => Off is

   -- Write the characteristic number to the file.

   procedure Write
     (The_File   : Storage_IO.File_Type;
      The_Number : Number_Type)
   is

      The_Array : Storage_Boolean_Array_Type;
      The_Count : SYSNatural;

      procedure Write_Value (The_Value : Boolean) is
      begin
         The_Array (The_Count) := The_Value;
         The_Count             := The_Count + 1;

         if The_Count = Storage_Type'Size then
            Storage_IO.Write
              (The_File,
               Storage_Boolean_Array_Type_To_Storage_Type (The_Array));
            The_Array := (The_Array'Range => False);
            The_Count := 0;
         end if;
      end Write_Value;

   begin
      The_Count := 0;

      Storage_IO.Write (The_File, The_Number'Length);

      for The_Index in The_Number'Range loop
         Write_Value (The_Number (The_Index));
      end loop;

      if The_Count /= 0 then
         Storage_IO.Write
           (The_File,
            Storage_Boolean_Array_Type_To_Storage_Type (The_Array));
      end if;
   end Write;

   -- Read the characteristic number from the file.

   procedure Read
     (The_File      :        Storage_IO.File_Type;
      The_Variables :        Variable_Count_Type;
      The_Number    : in out Number_Type)
   is

      The_Count   : SYSNatural;
      The_Storage : Storage_Type;
      The_Array   : Storage_Boolean_Array_Type;

      procedure Read_Value (The_Value : out Boolean) is
      begin
         if The_Count = Storage_Type'Size then

            Storage_IO.Read (The_File, The_Storage);
            The_Array :=
              Storage_Type_To_Storage_Boolean_Array_Type (The_Storage);
            The_Count := 0;
         end if;
         The_Value := The_Array (The_Count);
         The_Count := The_Count + 1;
      end Read_Value;

   begin
      The_Count := Storage_Type'Size;

      Storage_IO.Read (The_File, The_Storage);

      if The_Storage /= 2**SYSNatural (The_Variables) then
         raise Constraint_Error;
      end if;

      The_Number := new Number_Array_Type (0 .. 2**SYSNatural (The_Variables) - 1);

      for The_Index in The_Number'Range loop
         Read_Value (The_Number (The_Index));
      end loop;
   end Read;

end Number_Package.IO_Package;

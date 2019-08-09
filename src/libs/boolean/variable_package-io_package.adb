pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

-- Read/write the variable as an integer value (variable) to the file. Format:
-- <number of variables> { <variable> }

package body Variable_Package.IO_Package with
Spark_Mode => Off is

   -- Write the set of variables to the file.

   procedure Write
     (The_File      : Storage_IO.File_Type;
      The_Variables : Variable_Set_Type)
   is
   begin
      Storage_IO.Write (The_File, Storage_Type (Count_Of (The_Variables)));

      for The_Variable in Variable_Set_Type'Range loop
         if The_Variables (The_Variable) then
            Storage_IO.Write (The_File, Storage_Type (The_Variable));
         end if;
      end loop;
   end Write;

   -- Read the set of variables from the file.

   procedure Read
     (The_File      :        Storage_IO.File_Type;
      The_Variables : in out Variable_Set_Type)
   is

      The_Count    : SYSNatural;
      The_Storage  : Storage_Type;
      The_Variable : Variable_Type;

      procedure Read_Variable (The_Variable : out Variable_Type) is
      begin
         Storage_IO.Read (The_File, The_Storage);
         The_Variable := Variable_Type (The_Storage);
      end Read_Variable;

   begin
      Storage_IO.Read (The_File, The_Storage);
      The_Count := SYSNatural (The_Storage);

      The_Variables := (Variable_Set_Type'Range => False);
      for The_Index in 0 .. The_Count - 1 loop
         Read_Variable (The_Variable);
         The_Variables (The_Variable) := True;
      end loop;
   end Read;

end Variable_Package.IO_Package;

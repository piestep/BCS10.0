pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 9 Paul Estep

with Equation_Package.IO_Package; use Equation_Package.IO_Package;

-- Read/write the word's leangth followed by the boolean equations. Format:
-- <length> { <boolean equation> }

package body Word_Package.IO_Package with
Spark_Mode => Off is

   -- Write the word to the file.

   procedure Write (The_File : Storage_IO.File_Type; The_Word : Word_Type) is
   begin
      Storage_IO.Write (The_File, Storage_Type (The_Word'Length));

      for I in 0 .. Length_Of (The_Word) - 1 loop
         Write (The_File, The_Word (I));
      end loop;
   end Write;

   -- Read the word from the file.

   procedure Read (The_File : Storage_IO.File_Type; The_Word : out Word_Type) is
      The_Storage : Storage_Type;
   begin
      Storage_IO.Read (The_File, The_Storage);

      Create (The_Word, Word_Count_Type (The_Storage));

      for I in 0 .. Length_Of (The_Word) - 1 loop
         Read (The_File, The_Word (I));
      end loop;
   end Read;

end Word_Package.IO_Package;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Storage_Package; use Storage_Package;

-- A package to read and write boolean words to a file.

package Word_Package.IO_Package with
     Spark_Mode => Off is

   -- Write the word to the file.

   procedure Write (The_File : Storage_IO.File_Type; The_Word : Word_Type);

   -- Read the word from the file.

   procedure Read (The_File : Storage_IO.File_Type; The_Word : out Word_Type);

end Word_Package.IO_Package;

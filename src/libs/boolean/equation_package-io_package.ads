-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Storage_Package; use Storage_Package;

-- A package to read and write a boolean equations to a file.

package Equation_Package.IO_Package with
     Spark_Mode => Off is

   -- Write the equation to the file.

   procedure Write
     (The_File     : Storage_IO.File_Type;
      The_Equation : Equation_Type);

   -- Read the equation from the file.

   procedure Read
     (The_File     :     Storage_IO.File_Type;
      The_Equation : out Equation_Type);

end Equation_Package.IO_Package;

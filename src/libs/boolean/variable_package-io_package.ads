-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Storage_Package; use Storage_Package;

-- A package to read and write a set of variables to a file.

package Variable_Package.IO_Package with
     Spark_Mode => Off is

   -- Write the set of variables to the file.

   procedure Write
     (The_File      : Storage_IO.File_Type;
      The_Variables : Variable_Set_Type);

   -- Read the set of variables from the file.

   procedure Read
     (The_File      :        Storage_IO.File_Type;
      The_Variables : in out Variable_Set_Type);

end Variable_Package.IO_Package;

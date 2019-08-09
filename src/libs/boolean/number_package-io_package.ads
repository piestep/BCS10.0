-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Storage_Package; use Storage_Package;

-- A package to read and write a chratcteristic number to a file.

package Number_Package.IO_Package with
     Spark_Mode => Off is

   -- Write the characteristic number to the file.

   procedure Write (The_File : Storage_IO.File_Type; The_Number : Number_Type);

   -- Read the characteristic number from the file. Raise data error if the
   -- characteristic number format is wrong.

   procedure Read
     (The_File      :        Storage_IO.File_Type;
      The_Variables :        Variable_Count_Type;
      The_Number    : in out Number_Type);

end Number_Package.IO_Package;

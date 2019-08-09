pragma Ada_2012;
-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Storage_Package; use Storage_Package;
with Term_Package;    use Term_Package;
--
with Variable_Package.IO_Package; use Variable_Package.IO_Package;
with Number_Package.IO_Package;   use Number_Package.IO_Package;

-- Read/write the equation's set of variables followed by the equation's
-- characteristic number. Format: <set of variables> <characteristc number>

package body Equation_Package.IO_Package with
Spark_Mode => Off is

   -- Write the equation to the file.

   procedure Write
     (The_File     : Storage_IO.File_Type;
      The_Equation : Equation_Type)
   is
   begin
      Write (The_File, The_Equation.The_Variables);
      Write (The_File, The_Equation.The_Number);
   end Write;

   -- Read the equation from the file.

   procedure Read
     (The_File     :     Storage_IO.File_Type;
      The_Equation : out Equation_Type)
   is
      The_Variables : Variable_Count_Type;
      The_Set       : Variable_Set_Type;
      The_Number    : Number_Type;
   begin
      Read (The_File, The_Set);
      The_Variables := Count_Of (The_Set);

      Read (The_File, The_Variables, The_Number);

      The_Equation := (The_Variables => The_Set, The_Number => The_Number);
   end Read;

end Equation_Package.IO_Package;

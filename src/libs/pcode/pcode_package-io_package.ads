-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
--

-- An Input/Output package for PCode.

package PCode_Package.IO_Package is

   -- End of file error reading PCode file.

   End_Error : exception renames Ada.Text_IO.End_Error;

   -- Error opening or saving PCode file.

   Name_Error : exception renames Ada.Text_IO.Name_Error;

   -- Error loading or saving PCode.

   IO_Error : exception;

   -- Line exceeds maximum length in pcode.

   Line_Error : exception;

   -- An unrecognized character found in pcode.

   Character_Error : exception;

   -- Load pcode length and instructions from a file.

   procedure Load
     (The_File_Name :        String;
      The_Code      : in out Code;
      The_Length    :    out Natural);

   -- Save pcode length and instructions to a file.

   procedure Save
     (The_File_Name : String;
      The_Code      : Code;
      The_Length    : Natural);

   -- List pcode to standard output.

   procedure List (The_Code : Code; The_Length : Natural);

end PCode_Package.IO_Package;

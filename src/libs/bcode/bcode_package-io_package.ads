-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Boolean_Package; use Boolean_Package;

-- An Input/Output package for BCode.

package BCode_Package.IO_Package is

   -- Error opening or saving BCode file.

   Name_Error : exception;

   -- Error loading or saving BCode.

   IO_Error : exception;

   -- Load bcode boolean words from a file.

   procedure Load
     (The_File_Name :     String;
      The_Variables : out Natural;
      The_Words     : out BCode_Words);

   -- Save bcode boolean words to a file.

   procedure Save
     (The_File_Name : String;
      The_Variables : Natural;
      The_Words     : BCode_Words);

   -- List bcode boolean words to standard output.

   procedure List
     (The_Variables  : Natural;
      The_Words      : BCode_Words;
      Boolean_Output : Boolean);

end BCode_Package.IO_Package;

-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

-- A package for holding input operand length, first value,
-- last value, and output length for PCode and BCode comparison
-- test purposes.

package Parameter_Package.IO_Package is
   pragma Elaborate_Body (IO_Package);

   -- Error opening or saving test file.

   Name_Error : exception;

   -- Error loading or saving test file.

   IO_Error : exception;

   -- Save input and output operand lengths to a file.

   procedure Save
     (The_Source_File_Name : String;
      The_Inputs           : Parameter_List_Type;
      The_Outputs          : Parameter_List_Type);

   -- Load a saved file of input operand lengths.

   procedure Load
     (The_Source_File_Name :     String;
      The_Inputs           : out Parameter_List_Type;
      The_Outputs          : out Parameter_List_Type);

end Parameter_Package.IO_Package;

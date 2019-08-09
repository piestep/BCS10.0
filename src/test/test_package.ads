-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with XML_Package; use XML_Package;
--

package Test_Package is

   FILES : constant String := "../../src/test/files";

   -- Maximum string length to return for the AUnit assert for string values.
   -- NOTE: (1) the string is truncated with periods "...".

   MAXIMUM_ASSERT_STRING : constant := 256;

   -- Unbounded strings used to compare to a file (Compare_File_To_Strings).

   type Array_Of_Unbounded_Strings is
     array (Positive range <>) of Unbounded_String;

   --generic AUnit assert for integer values.

   generic
      type Integer_Type is range <>;
   procedure Integer_Type_Assert (The_Result   : Integer_Type;
                                  The_Expected : Integer_Type;
                                  The_Test     : String;
                                  The_Text     : String := "");

   --generic AUnit assert for modular values.

   generic
      type Modular_Type is mod <>;
   procedure Modular_Type_Assert (The_Result   : Modular_Type;
                                  The_Expected : Modular_Type;
                                  The_Test     : String;
                                  The_Text     : String := "");

   --AUnit assert for boolean values.

   procedure Assert
     (The_Result   : Boolean;
      The_Expected : Boolean;
      The_Test     : String;
      The_Text     : String := "");

   --AUnit assert for Integer values.

   procedure Assert
     (The_Result   : Integer;
      The_Expected : Integer;
      The_Test     : String;
      The_Text     : String := "");

   --AUnit assert for string values.

   procedure Assert
     (The_Result   : String;
      The_Expected : String;
      The_Test     : String;
      The_Text     : String := "");

   --AUnit assert for unbounded string values.

   procedure Assert
     (The_Result   : Unbounded_String;
      The_Expected : Unbounded_String;
      The_Test     : String;
      The_Text     : String := "");

   -- Return true if two files are the same.

   function Compare_Files (The_Left, The_Right : in String) return Boolean;

   -- Return true if a file and a string are the same.

   function Compare_First_Line_Of_File_To_String
     (The_Filename : String;
      The_String   : String) return Boolean;

   -- Return true if a file and an unbounded array of strings are the same.

   function Compare_File_To_Strings
     (The_Filename : String;
      The_Strings  : Array_Of_Unbounded_Strings) return Boolean;

   -- Return true if a file and an vector of unbounded strings are the same.

   function Compare_File_To_Strings_Vector
     (The_Filename : String;
      The_Strings  : Strings_Vector.Vector) return Boolean;

   -- Save code to file.

   procedure Save
     (The_Source_File_Name : String;
      The_Code             : Strings_Vector.Vector);

   -- List code to standard output.

   procedure List
     (The_Code             : Strings_Vector.Vector;
      XML_Format           : Boolean := False);

end Test_Package;

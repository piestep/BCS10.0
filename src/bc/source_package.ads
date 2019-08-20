-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with System_Package; use System_Package;
with BC_Package;     use BC_Package;
--

-- A package to provide source program character input,
-- a source program listing, and source program error
-- reporting.

package Source_Package is

   Source_Debug : Boolean := False; -- source debug switch

   -- Maximum columns for input allowed.
   MAX_COLUMN_POSITION : constant := 100;

   -- Source location types

   type Line_Number is new SYSPositive;
   type Column_Position is new SYSPositive range 1 .. MAX_COLUMN_POSITION;

   type Source_Position is record
      The_Line   : Line_Number;
      The_Column : Column_Position;
   end record;

   -- Last character and character position read
   -- from the source program.

   The_Character : Character;
   The_Position  : Source_Position := (1, 1);

   -- Open the source program file.

   procedure Open (The_Source_File_Name : String);

   -- Close the source program file.

   procedure Close;

   -- Read next character and postion from the source program.

   procedure Next_Character;

   -- Return string image of line number and column sperated by a comma.

   function Image_Of (The_Position : Source_Position) return String;

end Source_Package;

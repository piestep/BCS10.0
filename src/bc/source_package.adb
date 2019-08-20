-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
--
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
--
with Debug_Package; use Debug_Package;
with Error_Package; use Error_Package;
--

package body Source_Package is

   package Number_IO is new Ada.Text_IO.Integer_IO (Line_Number);

   -- The source program file.

   The_Source_File : File_Type;

   -- current source program line.

   The_Line : String (1 .. MAX_COLUMN_POSITION) :=
     (1 .. MAX_COLUMN_POSITION => ' ');

   -- length of current source program line.

   The_Length : SYSNatural := 0;

   -- current line number and column postion.

   The_Number : Line_Number     := 1;
   The_Column : Column_Position := 1;

   -- Read next line from source program.

   procedure Read_Line is
   begin
      Debug (Source_Debug, "begin Read_Line");

      The_Column := 1;

      Get_Line (The_Source_File, The_Line, The_Length);

      while The_Length <= 0 loop
         The_Number := The_Number + 1;
         Get_Line (The_Source_File, The_Line, The_Length);
      end loop;

      if The_Length < The_Line'Length then
         The_Line (The_Length + 1) := Ada.Characters.Latin_1.LF;
         The_Length                := The_Length + 1;

      else
         Error_Package.Compiler_Error
           ((The_Number, Column_Position (The_Length)),
            "Maximum line length reached.");
      end if;

      Debug (Source_Debug, "end Read_Line");
   end Read_Line;

   -- Open the source program file.

   procedure Open (The_Source_File_Name : String) is
   begin
      Debug (Source_Debug, "begin Open");

      Open (The_Source_File, In_File, The_Source_File_Name);

      The_Number := 1;
      Read_Line;

      The_Character :=
        Ada.Characters.Handling.To_Upper (The_Line (SYSPositive (The_Column)));

      The_Position := (The_Number, The_Column);

      Debug (Source_Debug, "end Open");

   exception

      when Name_Error =>
         Compiler_Error ("File can not be found.");

      when Use_Error =>
         Compiler_Error ("Expected file extension.");

   end Open;

   -- Close the source program file.

   procedure Close is
   begin
      Close (The_Source_File);
   end Close;

   -- Read next character and postion from the source program.

   procedure Next_Character is
   begin
      Debug (Source_Debug, "begin Next_Character");

      if SYSNatural (The_Column) >= The_Length then
         The_Number := The_Number + 1;
         Read_Line;
         The_Character :=
           Ada.Characters.Handling.To_Upper
             (The_Line (SYSPositive (The_Column)));
      else
         The_Column    := The_Column + 1;
         The_Character :=
           Ada.Characters.Handling.To_Upper
             (The_Line (SYSPositive (The_Column)));
      end if;
      The_Position := (The_Number, The_Column);

      Debug (Source_Debug, "end Next_Character");
   end Next_Character;

   -- Return string image of line number and column sperated by a comma.

   function Image_Of (The_Position : Source_Position) return String is
   begin
      return Trim (Line_Number'Image (The_Position.The_Line), Left) &
        "," &
        Column_Position'Image (The_Position.The_Column);
   end Image_Of;

begin

   Debug (Debug_Initialization, "Source_Package");

end Source_Package;

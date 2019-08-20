-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Unchecked_Conversion;
--
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Containers;      use Ada.Containers;
--
with Variable_Package; use Variable_Package;
with Term_Package;     use Term_Package;
with Equation_Package; use Equation_Package;
with Storage_Package;  use Storage_Package;
with Word_Package;     use Word_Package;
--
with Word_Package.IO_Package; use Word_Package.IO_Package;

-- Load/save bcode to a file or list bcode to standard output.
-- Format: <number of variables> <length> { <boolean words> }

package body BCode_Package.IO_Package is

   -- Load bcode boolean words from a file.

   procedure Load
     (The_File_Name :     String;
      The_Variables : out Natural;
      The_Words     : out BCode_Words)
   is
      The_Object_File : Storage_IO.File_Type;
      The_Length      : Natural;
      The_Storage     : Storage_Type;
      The_Word        : Word_Type;
   begin
      Storage_IO.Open (The_Object_File, Storage_IO.In_File, The_File_Name);

      Storage_IO.Read (The_Object_File, The_Storage);
      The_Variables := Natural (The_Storage);

      Storage_IO.Read (The_Object_File, The_Storage);
      The_Length := Natural (The_Storage);

      The_Words.Set_Length (Count_Type (The_Length));

      for The_Index in 0 .. The_Length - 1 loop
         Read (The_Object_File, The_Word);
         Replace_Element (The_Words, The_Index, The_Word);
      end loop;

      Storage_IO.Close (The_Object_File);

   exception

      when Storage_IO.Name_Error =>
         raise BCode_Package.IO_Package.Name_Error;

      when Storage_IO
         .Status_Error | Storage_IO
         .Mode_Error | Storage_IO
         .Use_Error | Storage_IO
         .Device_Error | Storage_IO
         .End_Error | Storage_IO
         .Data_Error =>
         raise IO_Error;

   end Load;

   -- Save bcode boolean words to a file.

   procedure Save
     (The_File_Name : String;
      The_Variables : Natural;
      The_Words     : BCode_Words)
   is
      The_File : Storage_IO.File_Type;

      procedure Write (The_Position : Cursor) is
      begin
         Write (The_File, Element (The_Position));
      end Write;

   begin
      Storage_IO.Create (The_File, Storage_IO.Out_File, The_File_Name);

      Storage_IO.Write (The_File, Storage_Type (The_Variables));
      Storage_IO.Write
        (The_File,
         Storage_Type (Natural (Last_Index (The_Words) + 1)));

      Iterate (The_Words, Write'Access);

      Storage_IO.Close (The_File);

   exception

      when Storage_IO.Name_Error =>
         raise BCode_Package.IO_Package.Name_Error;

      when Storage_IO
         .Status_Error | Storage_IO
         .Mode_Error | Storage_IO
         .Use_Error | Storage_IO
         .Device_Error | Storage_IO
         .End_Error | Storage_IO
         .Data_Error =>
         raise IO_Error;

   end Save;

   -- List bcode boolean words to standard output.

   procedure List
     (The_Variables  : Natural;
      The_Words      : BCode_Words;
      Boolean_Output : Boolean)
   is

      Word_Index     : Natural := 0;
      Equation_Index : Natural := 0;

      -- List boolean equation to standard output.

      procedure List (The_Equation : Equation_Type) is
      begin
         Put (Word_Index, 4);
         Put (":");
         Put (Equation_Index, 3);
         Put (" ");

         if Boolean_Output then
            Put_Line (Boolean_Of (The_Equation));
         else
            Put_Line (Image_Of (The_Equation));
         end if;

         Equation_Index := Equation_Index + 1;
      end List;

      -- List bcode word to standard output.

      procedure List (The_Position : Cursor) is
      begin
         Equation_Index := 0;
         Iterate (Element (The_Position), List'Access);
         Word_Index := Word_Index + 1;
      end List;

   begin
      Put_Line ("Variables: " & Natural'Image (The_Variables));
      Iterate (The_Words, List'Access);
   end List;

end BCode_Package.IO_Package;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--
with Source_Package; use Source_Package;
with Error_Package;  use Error_Package;
--

package body List_Package is

   -- List the BC source program and errors.

   procedure List
     (The_Source_File_Name : String;
      XML_Format           : Boolean := False)
   is
      The_File      : File_Type;                    -- source program file.
      The_Line      : Line_Number := 1;             -- current line number.
      The_Character : Character;                    -- last character.

      procedure Listing
        (The_Position : Source_Position;
         The_Kind     : Message_Kind;
         The_Text     : String)
      is
      begin
         while not End_Of_File (The_File)
           and then The_Position.The_Line >= The_Line
         loop

            if XML_Format then
               Put ("<ln>");
            end if;

            Put (Integer (The_Line), 5);
            Put (": ");

            while not End_Of_File (The_File) and not End_Of_Line (The_File) loop
               Get (The_File, The_Character);
               Put (The_Character);
            end loop;

            if XML_Format then
               Put ("</ln>");
            end if;

            New_Line;

            if not End_Of_File (The_File) then
               Skip_Line (The_File);
            end if;

            The_Line := The_Line + 1;
         end loop;

         if XML_Format then
            Put ("<ln>");
         end if;

         Put ((1 .. Positive (The_Position.The_Column) + 5 => ' '));

         if The_Kind = Error_Kind then
            Put ("^ " & The_Text);

         else
            Put ("^ Warning: " & The_Text);
         end if;

         if XML_Format then
            Put ("<ln>");
         end if;

         New_Line;
      end Listing;

   begin

      Open (The_File, In_File, The_Source_File_Name);
      Iterate (Listing'Access);

      if XML_Format then
         Put_Line ("<listing>");
      end if;

      while not End_Of_File (The_File) loop

         if XML_Format then
            Put ("<ln>");
         end if;

         Put (Integer (The_Line), 5);
         Put (": ");

         while not End_Of_File (The_File) and not End_Of_Line (The_File) loop
            Get (The_File, The_Character);
            Put (The_Character);
         end loop;

         if XML_Format then
            Put ("</ln>");
         end if;

         New_Line;

         if not End_Of_File (The_File) then
            Skip_Line (The_File);
         end if;

         The_Line := The_Line + 1;
      end loop;

      if XML_Format then
         Put_Line ("</listing>");
      end if;

      Close (The_File);
   end List;

end List_Package;

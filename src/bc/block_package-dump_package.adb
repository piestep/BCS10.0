-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with Type_Package.Image_Package;       use Type_Package.Image_Package;
with Identifier_Package.Image_Package; use Identifier_Package.Image_Package;
--

package body Block_Package.Dump_Package is

   -- Dump block to standard output.

   procedure Dump_Block (The_Block : Block) is
      procedure Dump_Block (The_Position : Map_Package.Cursor) is
      begin
         Put ("[");
         Put (Image_Of (Key (The_Position)));
         Put (" | ");
         Put (Image_Of (Element (The_Position)));
         Put ("]");
         New_Line;
      end Dump_Block;
   begin
      Iterate (The_Block.The_Map, Dump_Block'Access);
   end Dump_Block;

   -- Dump current block to standard output.

   procedure Dump_Block is
   begin
      if The_Table = null then
         Put_Line ("NULL");
      else
         Dump_Block (The_Table);
      end if;
   end Dump_Block;

   -- Dump blocks to standard output.

   procedure Dump_Blocks (The_Block : Block) is
      The_Level   : Natural := 0;
      The_Pointer : Block;
   begin
      The_Pointer := The_Block;
      while The_Pointer /= null loop
         Put_Line ("** Level:" & Integer'Image (The_Level) & " **");
         Dump_Block (The_Pointer);
         The_Level   := The_Level + 1;
         The_Pointer := The_Pointer.The_Next;
      end loop;
   end Dump_Blocks;

   -- Dump current blocks to standard output.

   procedure Dump_Blocks is
   begin
      if The_Table = null then
         Put_Line ("NULL");
      else
         Dump_Blocks (The_Table);
      end if;
   end Dump_Blocks;

end Block_Package.Dump_Package;

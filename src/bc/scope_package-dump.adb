-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with System_Package; use System_Package;
with Identifier_Package.Image_Of;

-- Dump current and previous scopes.

procedure Scope_Package.Dump is
   The_Level : SYSNatural := 0;

   -- Dump an identifier.

   procedure Dump_Identifier (Position : Map_Package.Cursor) is
   begin
      Put_Line (Image_Of (Element (Position)));
   end Dump_Identifier;

   -- Dump a scope.

   procedure Dump_Identifiers (Position : Table_Package.Cursor) is
   begin
      Put_Line ("** Level:" & Integer'Image (The_Level) & " **");
      Iterate (Element (Position).all, Dump_Identifier'Access);
      The_Level := The_Level + 1;
   end Dump_Identifiers;

begin
   Iterate (The_Table, Dump_Identifiers'Access);
end Scope_Package.Dump;

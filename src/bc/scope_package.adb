-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Deallocation;
--
with Debug_Package; use Debug_Package;
--
with Identifier_Package.Image_Package; use Identifier_Package.Image_Package;
with Scope_Package.Dump_Package;       use Scope_Package.Dump_Package;
--

package body Scope_Package is

   -- Open a new scope.

   procedure Open is
   begin
      Debug (Scope_Debug, "begin Open");

      Append (The_Table, new Map);

      Debug (Scope_Debug, "end Open");
   end Open;

   -- Close the current scope and reopen the previous scope.

   procedure Close is
      The_Pointer : Map_Pointer;

      procedure Free is new Ada.Unchecked_Deallocation (Map, Map_Pointer);

   begin
      Debug (Scope_Debug or Scope_Dump, "begin Close");

      if Scope_Dump then
         Dump;
      end if;

      The_Pointer := Element (The_Table, Last_Index (The_Table));
      Delete_Last (The_Table);

      The_Pointer.all := Empty_Map;
      Free (The_Pointer);

      Debug (Scope_Debug or Scope_Dump, "end Close");
   end Close;

   -- Enter an identifier into the scope.

   procedure Enter (The_Identifier : Identifier_Pointer) is
      The_Pointer : Map_Pointer;
   begin
      Debug (Scope_Debug, "begin Enter(Identifier)");
      Debug (Scope_Debug, Image_Of (The_Identifier));

      The_Pointer := Element (Last (The_Table));
      Include (The_Pointer.all, The_Identifier.The_String, The_Identifier);

      Debug (Scope_Debug, "end Enter(Identifier)");
   end Enter;

   -- Return true if the identifier associated with the string is entered into
   -- the current or previous scopes.

   function Is_Identifier (The_String : Unbounded_String) return Boolean is
      The_Pointer : Map_Pointer;
      The_Cursor  : Table_Package.Cursor;
   begin
      The_Cursor := Last (The_Table);
      while The_Cursor /= Table_Package.No_Element loop

         The_Pointer := Element (The_Cursor);

         if Contains (The_Pointer.all, The_String) then
            return True;
         end if;

         The_Cursor := Previous (The_Cursor);
      end loop;

      return False;
   end Is_Identifier;

   -- Return the identifier associated with the string witin the current or
   -- previous scopes. Raise critical error if not found.

   function Look_Up (The_String : Unbounded_String) return Identifier_Pointer is
      The_Pointer : Map_Pointer;
      The_Cursor  : Table_Package.Cursor;
   begin
      The_Cursor := Last (The_Table);
      while The_Cursor /= Table_Package.No_Element loop

         The_Pointer := Element (The_Cursor);

         if Contains (The_Pointer.all, The_String) then
            return Element (The_Pointer.all, The_String);
         end if;

         The_Cursor := Previous (The_Cursor);
      end loop;

      raise Critical_Error;
   end Look_Up;

begin

   Debug (Debug_Initialization, "Scope_Package");

end Scope_Package;

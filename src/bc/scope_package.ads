-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
--
with BC_Package;         use BC_Package;
with Identifier_Package; use Identifier_Package;
--

-- A package to create and manage program source identifier scopes. Scopes are
-- used to determine which definitions and values should be used within the
-- program source. New scopes are created and stacked upon the previous scope
-- as needed. Seaching starts at the current scope and proceeds scope by scope
-- until the item is found. Closing a scope disposes the current scope replacing
-- it with the previous scope.

package Scope_Package is

   -- Scope debug switch.

   Scope_Debug : Boolean := False;

   --      Scope dump switch.

   Scope_Dump : Boolean := False;

   -- Open a new scope.

   procedure Open;

   -- Close the current scope and reopen the previous scope.

   procedure Close;

   -- Enter an identifier into the scope.

   procedure Enter (The_Identifier : Identifier_Pointer);

   -- Return true if the identifier associated with the string is entered into
   -- the current or previous scopes.

   function Is_Identifier (The_String : Unbounded_String) return Boolean;

   -- Return the identifier associated with the string witin the current or
   -- previous scopes. Raise critical error if not found.

   function Look_Up (The_String : Unbounded_String) return Identifier_Pointer;

private

   -- Identifier list.

   package Map_Package is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Identifier_Pointer,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   use Map_Package;

   type Map_Pointer is access Map_Package.Map;

   package Table_Package is new Ada.Containers.Vectors (Natural, Map_Pointer);
   use Table_Package;

   -- The current scope.

   The_Table : Table_Package.Vector := Table_Package.Empty_Vector;

end Scope_Package;

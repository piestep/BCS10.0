-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;
--

package XML_Package is
   pragma Elaborate_Body (XML_Package);

   -- Error opening test file.

   Name_Error : exception;

   -- Error loading test file.

   IO_Error : exception;

   package Strings_Vector is new Ada.Containers.Vectors
     (Positive,
      Unbounded_String);
   use Strings_Vector;

   type XML_Record is record
      The_Name    : Unbounded_String;
      The_Code    : Vector := Empty_Vector;
      The_Listing : Vector := Empty_Vector;
   end record;

   function Hash (Key : Unbounded_String) return Ada.Containers.Hash_Type;

   package Tests_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => XML_Record,
      Hash            => Hash,
      Equivalent_Keys => "=");
   use Tests_Map;

  function Encode (The_String : String) return String;

   procedure Load
     (The_Source_File_Name :     String;
      The_Tests            : out Tests_Map.Map);

private

   function Hash
     (Key : Unbounded_String) return Ada.Containers.Hash_Type is
     (Ada.Strings.Unbounded.Hash_Case_Insensitive (Key));

end XML_Package;

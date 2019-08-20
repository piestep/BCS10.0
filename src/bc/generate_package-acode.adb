-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Containers.Hashed_Maps;
--
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Containers;          use Ada.Containers;
--
with Ada.Environment_Variables; use Ada.Environment_Variables;
--
with System_Package;     use System_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
with Scanner_Package;    use Scanner_Package;
with Operand_Package;    use Operand_Package;
--

separate (Generate_Package)
procedure ACode (The_Unit : Compilation_Unit_Graph; The_Name : String) is

   The_Date    : String (1 .. 10);        -- generation date.
   The_Package : Unbounded_String; -- package name.

   function Hash (The_Type : Type_Pointer) return Hash_Type;

   package Definitions_Maps is new Hashed_Maps
     (Type_Pointer,
      Unbounded_String,
      Hash,
      "=",
      "=");
   use Definitions_Maps;

   The_Map : Map;

   function Hash (The_Type : Type_Pointer) return Hash_Type is
   begin
      if Is_Scalar (The_Type) then
         if Is_Signed (The_Type) then
            return 0;
         else
            return 1;
         end if;
      elsif Is_Array (The_Type) then
         return 3;
      else
         raise Critical_Error;
      end if;
   end Hash;

   -- Ada type of BC.

   function Type_Of (The_Package : String; The_Type : String) return String is
   begin
      if The_Type = "BOOLEAN" then
         return "BOOLEAN";
      elsif The_Type = "INTEGER" then
         return The_Type;
      else
         return The_Package & "." & The_Type;
      end if;
   end Type_Of;

   function Type_Of (The_Type : String) return String is
   begin
      if The_Type = "BOOLEAN" then
         return "BOOLEAN";
      elsif The_Type = "INTEGER" then
         return The_Type;
      else
         return The_Type;
      end if;
   end Type_Of;

   procedure Ada_Files
     (The_Name    : String;
      The_Package : Unbounded_String;
      The_Date    : String) is separate;

   procedure Gnat_File
     (The_Name    : String;
      The_Package : Unbounded_String;
      The_Date    : String) is separate;

   procedure ARun_File
     (The_Name    : String;
      The_Package : Unbounded_String;
      The_Date    : String) is separate;

begin

   -- Get date.

   Put (The_Date (1 .. 2), Ada.Calendar.Month (Ada.Calendar.Clock));
   The_Date (3) := '/';
   Put (The_Date (4 .. 5), Ada.Calendar.Day (Ada.Calendar.Clock));
   The_Date (6) := '/';
   Put (The_Date (7 .. 10), Ada.Calendar.Year (Ada.Calendar.Clock));

   -- Get package name.

   if The_Unit /= null
     and then The_Unit.The_Package /= null
     and then The_Unit.The_Package.The_Name /= null
   then
      The_Package := The_Unit.The_Package.The_Name.The_String;
   end if;

   -- Generate ACode files.

   Ada_Files (The_Name, The_Package, The_Date);
--      Generate_Ada_Specification_File(The_Name, The_Package, The_Date);
--      Generate_Ada_Implementation_File(The_Name, The_Package, The_Date);
   ARun_File (The_Name, The_Package, The_Date);
   Gnat_File (The_Name, The_Package, The_Date);

end ACode;

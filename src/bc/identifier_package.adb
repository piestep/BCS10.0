-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Deallocation;
--
with Ada.Text_IO; use Ada.Text_IO;
--
with Debug_Package; use Debug_Package;
--

package body Identifier_Package is

   -- Clear all linked identifiers.

   procedure Clear is
      The_Identifier : Identifier_Pointer;
   begin
      while The_Last /= null loop
         The_Identifier := The_Last;
         The_Last := The_Last.The_Previous;
         Dispose(The_Identifier);
      end loop;
   end Clear;

   -- Dispose identifier.

   procedure Dispose (The_Identifier : in out Identifier_Pointer) is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Identifier_Record'Class,
         Identifier_Pointer);
   begin
      Deallocate (The_Identifier);
   end Dispose;

   -- Return true if identifier is a package identifier.

   function Is_Package (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Package_Identifier'Class;
   end Is_Package;

   -- Return true if identifier is a procedure identifier.

   function Is_Procedure (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Procedure_Identifier'Class;
   end Is_Procedure;

   -- Return true if identifier is a type identifier.

   function Is_Type (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Type_Identifier'Class;
   end Is_Type;

   -- Return true if identifier is a typed identifier.

   function Is_Typed (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Typed_Identifier'Class;
   end Is_Typed;

   -- Return true if identifier is a constant identifier.

   function Is_Constant (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Constant_Identifier'Class;
   end Is_Constant;

   -- Return true if identifier is an addressable identifier.

   function Is_Addressable
     (The_Identifier : Identifier_Pointer) return Boolean
   is
   begin
      return The_Identifier.all in Addressable_Identifier'Class;
   end Is_Addressable;

   -- Return true if identifier is a variable identifier.

   function Is_Variable (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Variable_Identifier'Class;
   end Is_Variable;

   -- Return true if identifier is an index identifier.

   function Is_Index (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Index_Identifier'Class;
   end Is_Index;

   -- Return true if identifier is a parameter identifier.

   function Is_Parameter (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return The_Identifier.all in Parameter_Identifier'Class;
   end Is_Parameter;

begin

   Debug (Debug_Initialization, "Identifier_Package");

end Identifier_Package;

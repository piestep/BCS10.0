-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Ada.Text_IO;
--  with Ada.Strings;
--  with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with System.Address_Image;
--
with System.Storage_Elements; use System.Storage_Elements;
--

package body Pool_Package is

   subtype Address_String is String(1..16);

   package Address_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Address_String,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   The_Map : Address_Map.Map := Address_Map.Empty_Map;
   The_Next : Integer := 0;

   -- Mark allocations.

   procedure Mark_Allocations (The_Pool : in out Storage_Pool) is
   begin
      The_Pool.The_Marked_Allocations := The_Pool.The_Number_Of_Allocations -
        The_Pool.The_Number_Of_Deallocations;
   end Mark_Allocations;

   -- Return unmarked allocations

   function Unmarked_Allocations (The_Pool : Storage_Pool) return SYSNatural is
   begin
      return The_Pool.The_Number_Of_Allocations -
        (The_Pool.The_Marked_Allocations +
           The_Pool.The_Number_Of_Deallocations);
   end Unmarked_Allocations;

   -- Storage size

   overriding function Storage_Size
     (The_Pool : Storage_Pool) return System.Storage_Elements.Storage_Count
   is
   begin
      return System.Pool_Global.Storage_Size
        (System.Pool_Global.Global_Pool_Object);
   end Storage_Size;

   -- Allocate storage

   overriding procedure Allocate
     (The_Pool      : in out Storage_Pool;
      The_Address   :    out System.Address;
      The_Size      :        System.Storage_Elements.Storage_Count;
      The_Alignment :        System.Storage_Elements.Storage_Count)
   is
   begin
      The_Pool.The_Number_Of_Allocations :=
        The_Pool.The_Number_Of_Allocations + 1;

      The_Pool.The_Current_Size_Of_Allocations :=
        The_Pool.The_Current_Size_Of_Allocations + The_Size;

      if The_Pool.The_Maximum_Size_Of_Allocations <
        The_Pool.The_Current_Size_Of_Allocations
      then
         The_Pool.The_Maximum_Size_Of_Allocations :=
           The_Pool.The_Maximum_Size_Of_Allocations + The_Size;
      end if;

      System.Pool_Global.Allocate
        (System.Pool_Global.Global_Pool_Object,
         The_Address,
         The_Size,
         The_Alignment);

      if The_Pool.Allocate_Debug then
         Address_Map.Include(The_Map, System.Address_Image(The_Address), The_Next);
         Ada.Text_IO.Put_Line
           (System.Address_Image(The_Address) & "-" &
              Integer'Image(The_Next) &
              ": Allocate");
         The_Next := The_Next + 1;
      end if;

   end Allocate;

   -- Deallocate storage

   overriding procedure Deallocate
     (The_Pool      : in out Storage_Pool;
      The_Address   :        System.Address;
      The_Size      :        System.Storage_Elements.Storage_Count;
      The_Alignment :        System.Storage_Elements.Storage_Count)
   is
      The_Integer : Integer;
   begin
      The_Pool.The_Number_Of_Deallocations :=
        The_Pool.The_Number_Of_Deallocations + 1;

      The_Pool.The_Current_Size_Of_Allocations :=
        The_Pool.The_Current_Size_Of_Allocations - The_Size;

      System.Pool_Global.Deallocate
        (System.Pool_Global.Global_Pool_Object,
         The_Address,
         The_Size,
         The_Alignment);

      if The_Pool.Allocate_Debug then
         The_Integer := Address_Map.Element(The_Map, System.Address_Image(The_Address));
         Address_Map.Exclude(The_Map, System.Address_Image(The_Address));
         Ada.Text_IO.Put_Line
           (System.Address_Image(The_Address) & "-" &
              Integer'Image(The_Integer) &
              ": Deallocate");
      end if;

   end Deallocate;

end Pool_Package;

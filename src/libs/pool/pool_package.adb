-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with System.Storage_Elements; use System.Storage_Elements;
--

package body Pool_Package is

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
   end Allocate;

   -- Deallocate storage

   overriding procedure Deallocate
     (The_Pool      : in out Storage_Pool;
      The_Address   :        System.Address;
      The_Size      :        System.Storage_Elements.Storage_Count;
      The_Alignment :        System.Storage_Elements.Storage_Count)
   is
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
   end Deallocate;

end Pool_Package;

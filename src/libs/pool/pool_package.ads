-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with System.Storage_Elements;
with System.Pool_Global;
--
with System_Package; use System_Package;
--

-- A package to track memory usage.

package Pool_Package is

   -- Storage pool type.

   type Storage_Pool is new System.Pool_Global.Unbounded_No_Reclaim_Pool with
      record
         Allocate_Debug                  : Boolean := False;
         The_Marked_Allocations          : SYSNatural := 0;
         The_Number_Of_Allocations       : SYSNatural := 0;
         The_Number_Of_Deallocations     : SYSNatural := 0;
         The_Current_Size_Of_Allocations : System.Storage_Elements
           .Storage_Count :=
             0;
         The_Maximum_Size_Of_Allocations : System.Storage_Elements
           .Storage_Count :=
             0;
      end record;

   -- Mark allocations.

   procedure Mark_Allocations (The_Pool : in out Storage_Pool);

   -- Return unmarked allocations

   function Unmarked_Allocations (The_Pool : Storage_Pool) return SYSNatural;

   -- Storage size

   overriding function Storage_Size
     (The_Pool : Storage_Pool) return System.Storage_Elements.Storage_Count;

   -- Allocate storage

   overriding procedure Allocate
     (The_Pool      : in out Storage_Pool;
      The_Address   :    out System.Address;
      The_Size      :        System.Storage_Elements.Storage_Count;
      The_Alignment :        System.Storage_Elements.Storage_Count);

   -- Deallocate storage

   overriding procedure Deallocate
     (The_Pool      : in out Storage_Pool;
      The_Address   :        System.Address;
      The_Size      :        System.Storage_Elements.Storage_Count;
      The_Alignment :        System.Storage_Elements.Storage_Count);

end Pool_Package;

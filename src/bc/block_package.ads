-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Containers.Hashed_Maps;
--
with Ada.Containers; use Ada.Containers;
--
with Pool_Package;
--
with BC_Package;         use BC_Package;
with Identifier_Package; use Identifier_Package;
with Word_Package;       use Word_Package;
--

-- A package to create and manage bcode blocks. A bcode block is simular to a
-- pcode scope. It is used to determine which definitions and values should be
-- used within the program source. However, blocks have more options for opening
-- and closing a scope. Which option is used depends on the language construct.

package Block_Package is

   -- Block Debug switch.

   Block_Debug : Boolean := False;

   -- Block dump switch.

   Block_Dump : Boolean := False;

   -- Block type;

   type Block is private;

   -- Open a new block.

   procedure Open;

   -- Open a new block as a branch from the previous block.

   procedure Branch (The_Block : out Block);

   -- Close a block without additional translation. All values, variables and
   -- operands, assigned within the block must apply any new assignments to the
   -- previous block assignments.

   procedure Close;

   -- Close a block with the condition. All values, variable and operand,
   -- assigned within the block must apply the condition to the previous
   -- block assignments.

   procedure Close (The_Condition : in out Word_Type);

   -- Close a block with the condition and a branch. All values assigned within
   -- the block must apply the condition and the branch to the previous block
   -- assignments.

   procedure Close (The_Condition : in out Word_Type; The_Block : in out Block);

   -- Add an entry to the current block.

   procedure Assign (The_Identifier : Identifier_Pointer; The_Word : Word_Type);

   -- Return true if identifier is assigned in the current block.

   function Is_Assigned (The_Identifier : Identifier_Pointer) return Boolean;

   -- Return true if identifier is an entry in the current or previous blocks.

   function Is_Entry (The_Identifier : Identifier_Pointer) return Boolean;

   -- Look up identifier within current and previous blocks.

   function Look_Up (The_Identifier : Identifier_Pointer) return Word_Type;

private

   -- Return hash key of identifier (identifier string).

   function Hash_Of (The_Pointer : Identifier_Pointer) return Hash_Type;

   package Map_Package is new Ada.Containers.Hashed_Maps
     (Key_Type        => Identifier_Pointer,
      Element_Type    => Word_Type,
      Hash            => Hash_Of,
      Equivalent_Keys => "=");
   use Map_Package;

   The_Pool : Pool_Package.Storage_Pool;

   type Block_Record;
   type Block is access Block_Record;
   for Block'Storage_Pool use The_Pool;

   type Block_Record is record
      The_Map  : Map   := Empty_Map;
      The_Next : Block := null;
   end record;

   -- The current block.

   The_Table : Block := null;

end Block_Package;

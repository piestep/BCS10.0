-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded.Hash;
--
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Debug_Package; use Debug_Package;
with Type_Package;  use Type_Package;
with Word_Package;  use Word_Package;
--
with Block_Package.Dump_Package; use Block_Package.Dump_Package;
--

package body Block_Package is

   -- Deallocate a block.

   procedure Deallocate is new Ada.Unchecked_Deallocation (Block_Record, Block);

   -- Return hash key for identifier pointer (unbounded string hash).

   function Hash_Of (The_Pointer : Identifier_Pointer) return Hash_Type is
   begin
      return Hash (The_Pointer.The_String);
   end Hash_Of;

   -- Return true if the identifier is assigned in the block.

   function Is_Assigned
     (The_Identifier : Identifier_Pointer;
      The_Block      : Block) return Boolean
   is
   begin
      if The_Block = null then
         return False;
      end if;

      return Contains (The_Block.The_Map, The_Identifier);
   end Is_Assigned;

   -- Return true if the identifier is assigned in the block and previous
   -- blocks.

   function Is_Entry
     (The_Identifier : Identifier_Pointer;
      The_Block      : Block) return Boolean
   is
      The_Pointer : Block;
   begin
      if The_Block = null then
         return False;
      end if;

      The_Pointer := The_Block;
      while The_Pointer /= null loop
         if Is_Assigned (The_Identifier, The_Pointer) then
            return True;
         end if;
         The_Pointer := The_Pointer.The_Next;
      end loop;
      return False;
   end Is_Entry;

   -- Return boolean word of the identifier in the block and previous blocks.

   function Look_Up
     (The_Identifier : Identifier_Pointer;
      The_Block      : Block) return Word_Type
   is
      The_Pointer : Block;
   begin
      The_Pointer := The_Block;
      while The_Pointer /= null loop
         if Is_Assigned (The_Identifier, The_Pointer) then
            return Element (The_Pointer.The_Map, The_Identifier);
         end if;
         The_Pointer := The_Pointer.The_Next;
      end loop;

      raise Critical_Error;
   end Look_Up;

   -- Reassign the identifier.

   procedure Reassign
     (The_Identifier : Identifier_Pointer;
      And_The_Word   : Word_Type;
      The_Block      : Block)
   is
      The_Word : Word_Type;
   begin
      if Is_Assigned (The_Identifier, The_Block) then
         The_Word := Element (The_Block.The_Map, The_Identifier);
         Dispose (The_Word);
      end if;
      Include (The_Block.The_Map, The_Identifier, And_The_Word);
   end Reassign;

   -- Open a new block.

   procedure Open is
   begin
      Debug (Block_Debug, "begin Open");

      The_Table :=
        new Block_Record'(The_Map => Empty_Map, The_Next => The_Table);

      Debug (Block_Debug, "end Open");
   end Open;

   -- Open a new block as a branch from the previous block.

   procedure Branch (The_Block : out Block) is
   begin
      Debug (Block_Debug, "begin Branch");

      The_Block := The_Table;

      The_Table :=
        new Block_Record'(The_Map => Empty_Map, The_Next => The_Table.The_Next);

      Debug (Block_Debug, "end Branch");
   end Branch;

   -- Close a block without additional translation. All values, variables and
   -- operands, assigned within the block must apply any new assignments to the
   -- previous block assignments.

   procedure Close is

      The_Block : Block;

      procedure Reassign (The_Position : Cursor) is
         The_Identifier : Identifier_Pointer;
         The_Word       : Word_Type;
      begin
         The_Identifier := Key (The_Position);
         The_Word       := Element (The_Position);

         -- if in previous blocks then reassign to previous block.
         if Is_Entry (The_Identifier, The_Table.The_Next) then
            Reassign (The_Identifier, The_Word, The_Table.The_Next);
         else
            Dispose (The_Word);
         end if;
      end Reassign;

   begin
      Debug (Block_Debug or Block_Dump, "begin Close");

      if Block_Dump then
         Dump_Blocks (The_Table);
      end if;

      Iterate (The_Table.The_Map, Reassign'Access);
      The_Block := The_Table.The_Next;
      Deallocate (The_Table);
      The_Table := The_Block;

      Debug (Block_Debug or Block_Dump, "end Close");
   end Close;

   -- Close a block with the condition. All values, variable and operand,
   -- assigned within the block must apply the condition to the previous
   -- block assignments.

   procedure Close (The_Condition : in out Word_Type) is

      -- x0 if c then x1 end if
      -- (x0 and not c) or (x1 and c) => x0

      The_Block : Block;

      procedure Reassign (The_Position : Cursor) is
         The_Identifier : Identifier_Pointer;
         The_Word       : Word_Type;
         And_The_Word   : Word_Type;
      begin
         The_Identifier := Key (The_Position);
         The_Word       := Element (The_Position);

         -- if in previous blocks (else) then reassign to previous block.
         if Is_Entry (The_Identifier, The_Table.The_Next) then
            And_The_Word := Look_Up (The_Identifier, The_Table.The_Next);
            If_Else (The_Condition, The_Word, And_The_Word);
            Reassign (The_Identifier, The_Word, The_Table.The_Next);
         else
            Dispose (The_Word);
         end if;
      end Reassign;

   begin
      Debug (Block_Debug or Block_Dump, "begin Close(The_Condition)");
      if Block_Dump then
         Dump_Blocks (The_Table);
      end if;

      Iterate (The_Table.The_Map, Reassign'Access);
      Dispose (The_Condition);

      The_Block := The_Table.The_Next;
      Deallocate (The_Table);
      The_Table := The_Block;

      if Block_Debug then
         Put_Line ("****");
         Dump_Blocks (The_Table);
         New_Line;
      end if;
      Debug (Block_Debug or Block_Dump, "end Close(The_Condition)");
   end Close;

   -- Close a block with the condition and a branch. All values assigned within
   -- the block must apply the condition and the branch to the previous block
   -- assignments.

   procedure Close
     (The_Condition : in out Word_Type;
      The_Block     : in out Block)
   is

      -- if c then x0 else x1 end if (x0 and not c) or (x1 and c) => x0

      The_Previous : Block;

      procedure Reassign_Then (The_Position : Cursor) is
         The_Identifier : Identifier_Pointer;
         The_Word       : Word_Type;
         And_The_Word   : Word_Type;
      begin
         The_Identifier := Key (The_Position);
         The_Word       := Element (The_Position);

         if Is_Entry (The_Identifier, The_Table.The_Next) then
            if Is_Assigned (The_Identifier, The_Table) then
               And_The_Word := Look_Up (The_Identifier, The_Table);
               If_Else (The_Condition, The_Word, And_The_Word);
               Reassign (The_Identifier, The_Word, The_Table.The_Next);
            else
               And_The_Word := Look_Up (The_Identifier, The_Table.The_Next);
               If_Else (The_Condition, The_Word, And_The_Word);
               Reassign (The_Identifier, The_Word, The_Table.The_Next);
            end if;
         else
            Dispose (The_Word);
         end if;
      end Reassign_Then;

      procedure Reassign_Else (The_Position : Cursor) is
         The_Identifier : Identifier_Pointer;
         The_Word       : Word_Type;
         And_The_Word   : Word_Type;
      begin
         The_Identifier := Key (The_Position);
         The_Word       := Element (The_Position);

         if Is_Entry (The_Identifier, The_Table.The_Next) then
            if not Is_Assigned (The_Identifier, The_Block) then
               Copy
                 (Look_Up (The_Identifier, The_Table.The_Next),
                  And_The_Word);
               If_Else (The_Condition, And_The_Word, The_Word);
               Reassign (The_Identifier, And_The_Word, The_Table.The_Next);
            end if;
         end if;
         Dispose (The_Word);
      end Reassign_Else;

   begin
      Debug (Block_Debug or Block_Dump, "begin Close(The_Condition,The_Block)");

      if Block_Dump then
         Put_Line ("--- Then ---");
         Dump_Block (The_Block);
         Put_Line ("--- Else ---");
         Dump_Blocks;
      end if;

      Iterate (The_Block.The_Map, Reassign_Then'Access);
      Iterate (The_Table.The_Map, Reassign_Else'Access);
      Dispose (The_Condition);

      Deallocate (The_Block);

      The_Previous := The_Table.The_Next;
      Deallocate (The_Table);
      The_Table := The_Previous;

      if Block_Debug then
         Put_Line ("****");
         Dump_Blocks (The_Table);
         New_Line;
      end if;
      Debug (Block_Debug or Block_Dump, "end Close(The_Condition,The_Block)");
   end Close;

   -- Add an entry to the current block.

   procedure Assign
     (The_Identifier : Identifier_Pointer;
      The_Word       : Word_Type)
   is
   begin
      Debug (Block_Debug, "begin Assign");
      Debug (Block_Debug, To_String (The_Identifier.The_String));

      Include (The_Table.The_Map, The_Identifier, The_Word);

      Debug (Block_Debug, "end Assign");
   end Assign;

   -- Return true if identifier is assigned in the current block.

   function Is_Assigned (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return Is_Assigned (The_Identifier, The_Table);
   end Is_Assigned;

   -- Return true if identifier is an entry in the current or previous blocks.

   function Is_Entry (The_Identifier : Identifier_Pointer) return Boolean is
   begin
      return Is_Entry (The_Identifier, The_Table);
   end Is_Entry;

   -- Look up identifier within current and previous blocks.

   function Look_Up (The_Identifier : Identifier_Pointer) return Word_Type is
   begin
      return Look_Up (The_Identifier, The_Table);
   end Look_Up;

begin
   Debug (Debug_Initialization, "Block_Package");
end Block_Package;

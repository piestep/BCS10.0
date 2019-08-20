-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Unchecked_Conversion;
--
with Ada.Text_IO; use Ada.Text_IO;
--
with System_Package;  use System_Package;
with Boolean_Package; use Boolean_Package;
--

-- A pacakge to define BC pseduo code (pcode). PCode is a list of instructions
-- which can be executed with a PCode machine giving the same result as its
-- equivlant BCode.

package PCode_Package is

   -- pcode address and word size.

   ADDRESS_SIZE : constant := 8;
   WORD_SIZE    : constant := 8;

   -- pcode opcode image width.

   OP_CODE_WIDTH : constant := 4;

   -- pcode address and word types.

   type Address is mod 2 ** ADDRESS_SIZE;
   type Word is mod 2 ** WORD_SIZE;

   subtype Word_Boolean is Boolean_Array_Type (0 .. Word'Size - 1);

   -- pcode opcodes (instructions).

   type Op_Code is
     (hlt_op,        -- halt pcode,                  hlt
      nop_op,         -- no-op,                               nop
      incs_op,        -- increment stack,             incs size operand
      decs_op,        -- decrement stack,             decs size operand
      lit_op,         -- literal,                             lit size operand
      filb_op,        -- fill block,                  filb size operand
      lod_op,         -- load,                                lod size
      lodb_op,        -- load block,                  lodb size
      sto_op,         -- store,                               sto size
      stob_op,        -- store block,                 stob size
      rd_op,          -- read,                                rd  size
      rdb_op,         -- read block,                  rdb  size
      wrt_op,         -- write,                               wrt size
      wrtb_op,        -- write block,                 wrtb size
      jmp_op,         -- unconditional jump,  jmp address
      jz_op,          -- jump on zero,                jz  address
      equ_op,         -- equal,                               equ size
      neq_op,         -- not equal,                   neq size
      ls_op,          -- less than,                   ls  size
      leq_op,         -- less than equal,             leq size
      gtr_op,         -- greater than,                gtr size
      geq_op,         -- greater than equal,  geq size
      uls_op,         -- less than,                   unsigned ls  size
      uleq_op,        -- less than equal,             unsigned leq size
      ugtr_op,        -- greater than,                unsigned gtr size
      ugeq_op,        -- greater than equal,  unsigned geq size
      not_op,         -- not,                                 not size
      and_op,         -- and,                                 and size
      or_op,          -- or,                                  or  size
      xor_op,         -- xor,                                 xor size
      neg_op,         -- negate,                              neg size
      add_op,         -- add,                                 add size
      sub_op,         -- subtract,                    sub size
      mul_op,         -- multiply,                    mul size
      div_op,         -- divide,                              div size
      mod_op,         -- modulus,                             mod size
      rem_op,         -- remainder,                   rem size
      umul_op,        -- unsigned multiply,   unsigned mul size
      udiv_op,        -- unsigned divide,             unsigned div size
      umod_op,        -- unsigned modulus,    unsigned mod size
      urem_op);       -- unsigned remainder,  unsigned rem size

   type Instruction is record
      The_Code    : Op_Code;      -- opcode
      The_Size    : Word;         -- operand size
      The_Operand : Word;         -- pointer to operand
   end record;

   type Code is array (Address range <>) of Instruction;

   type Memory is record
      The_Size : Word;         -- size (bits) in word.
      The_Word : Word;         -- memory word value.
   end record;

   type Data is array (Address range <>) of Memory;

   -- IO packages

   package Word_IO is new Ada.Text_IO.Modular_IO (Word);
   package Address_IO is new Ada.Text_IO.Modular_IO (Address);

   -- Convert word to word boolean array.

   function To_Word_Boolean is new Ada.Unchecked_Conversion
     (Word,
      Word_Boolean);

   -- Convert word boolean array to word.

   function To_Word is new Ada.Unchecked_Conversion (Word_Boolean, Word);

   -- Convert integer to word.

   function To_Word (The_Integer : Integer) return Word;

   -- Convert modular to word.

   function To_Word (The_Modular : BCModular) return Word;

   -- Return simple string representation of instruction.

   function String_Of (The_Instruction : Instruction) return String;

end PCode_Package;

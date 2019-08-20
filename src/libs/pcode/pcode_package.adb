-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

package body PCode_Package is

   -- Convert integer to word.

   function To_Word (The_Integer : Integer) return Word is
   begin
      return To_Word (SYSInteger_To_Boolean_Array_Type (The_Integer) (0 .. Word'Size - 1));
   end To_Word;

   -- Convert modular to word.

   function To_Word (The_Modular : BCModular) return Word is
   begin
      return To_Word (BCModular_To_Array (The_Modular) (0 .. Word'Size - 1));
   end To_Word;

   -- Return simple string for opcode in opcode image width.

   function String_Of (The_Code : Op_Code) return String is
      The_Fill : Natural;
   begin
      The_Fill := (OP_CODE_WIDTH + 3) - Op_Code'Image (The_Code)'Length;
      return Op_Code'Image (The_Code)
        (1 .. (Op_Code'Image (The_Code)'Length - 3)) &
      (1 .. The_Fill => ' ');
   end String_Of;

   -- Return string repreentation of opcode, word size, and word.

   function String_Of
     (The_Code    : String;
      The_Size    : Word;
      The_Operand : Word) return String
   is
   begin
      return The_Code & Word'Image (The_Size) & Word'Image (The_Operand);
   end String_Of;

   -- Return string repreentation of opcode and word.

   function String_Of (The_Code : String; The_Operand : Word) return String is
   begin
      return The_Code & Word'Image (The_Operand);
   end String_Of;

   -- Return simple string for instruction.

   function String_Of (The_Instruction : Instruction) return String is
   begin
      case The_Instruction.The_Code is
         when lit_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size,
               The_Instruction.The_Operand);

         when filb_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size,
               The_Instruction.The_Operand);

         when lod_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);
         when lodb_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);

         when sto_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);
         when stob_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);

         when rd_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);
         when rdb_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);

         when wrt_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);
         when wrtb_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);

         when incs_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size,
               The_Instruction.The_Operand);

         when decs_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size,
               The_Instruction.The_Operand);

         when hlt_op =>
            return "HLT";

         when jmp_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Operand);

         when jz_op =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Operand);

         when nop_op =>
            return "NOP";

         when equ_op |
              neq_op    |
              ls_op     |
              leq_op    |
              gtr_op    |
              geq_op    |
              uls_op    |
              uleq_op   |
              ugtr_op   |
              ugeq_op   |
              not_op    |
              neg_op    |
              and_op    |
              or_op     |
              xor_op    |
              add_op    |
              sub_op    |
              mul_op    |
              div_op    |
              mod_op    |
              rem_op    |
              umul_op   |
              udiv_op   |
              umod_op   |
              urem_op   =>
            return String_Of
              (String_Of (The_Instruction.The_Code),
               The_Instruction.The_Size);
      end case;
   end String_Of;

end PCode_Package;

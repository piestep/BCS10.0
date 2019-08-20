-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Text_IO;
--
with Unchecked_Conversion;
--
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
--

package body PCode_Package.IO_Package is

   -- Load pcode length and instructions from a file.

   procedure Load
     (The_File_Name :        String;
      The_Code      : in out Code;
      The_Length    :    out Natural)
   is

      MAX_COLUMN : constant := 80;

      type Line_Number is new Positive;

      package Number_IO is new Ada.Text_IO.Integer_IO (Line_Number);

      The_Object_File : File_Type;

      The_Line        : String (1 .. MAX_COLUMN) := (1 .. MAX_COLUMN => ' ');
      The_Line_Length : Natural                  := 0;
      The_Number      : Line_Number := 1;             -- line number
      The_Position : Positive;                             -- chacter position
      The_Character   : Character;                    -- the character

      package Op_IO is new Ada.Text_IO.Enumeration_IO (Op_Code);

      -- last word read.

      The_Word_Length : Positive;
      The_Word        : String (1 .. MAX_COLUMN);

      The_Counter     : Address  := 0;         -- program counter.
      The_Last        : Positive := 1;
      The_Op          : Op_Code;
      The_Size        : Word;
      The_Operand     : Word;
      The_Instruction : Instruction;

      procedure Read_Line is
      begin
         The_Position := 1;

         Get_Line (The_Object_File, The_Line, The_Line_Length);

         -- skip blank lines.

         while The_Line_Length <= 0 loop
            The_Number := The_Number + 1;
            Get_Line (The_Object_File, The_Line, The_Line_Length);
         end loop;

         -- if line within line array add a line feed.
         if The_Line_Length < The_Line'Length then
            The_Line (The_Line_Length + 1) := Ada.Characters.Latin_1.LF;
            The_Line_Length                := The_Line_Length + 1;
         else
            -- line exceeds maximum line (1..MAX_COLUMN)
            raise Line_Error;
         end if;

      end Read_Line;

      procedure Skip_Line is
      begin
         The_Number := The_Number + 1;
         Read_Line;
         The_Character := The_Line (The_Position);
      end Skip_Line;

      procedure Next_Character is
      begin
         if The_Position >= The_Line_Length then
            Load.Skip_Line;
         else
            The_Position  := The_Position + 1;
            The_Character := The_Line (The_Position);
         end if;
      end Next_Character;

      -- Read next word from input.

      procedure Next_Word is
      begin
         loop
            -- skip whitespaces.

            while The_Character = ' ' or
              The_Character = Ada.Characters.Latin_1.HT or
              The_Character = Ada.Characters.Latin_1.LF or
              The_Character = Ada.Characters.Latin_1.CR
            loop
               Next_Character;
            end loop;

            The_Word_Length := 1;

            case The_Character is

               -- opcode

               when 'A' .. 'Z' | 'a' .. 'z' =>
                  The_Word (1) := The_Character;
                  Next_Character;

                  while The_Character in 'A' .. 'Z' or
                    The_Character in 'a' .. 'z'
                  loop
                     The_Word_Length            := The_Word_Length + 1;
                     The_Word (The_Word_Length) := The_Character;

                     Next_Character;
                  end loop;
                  exit;

               -- number

               when '0' .. '9' =>
                  The_Word (1) := The_Character;
                  Next_Character;

                  while The_Character in '0' .. '9' or The_Character = '_' loop
                     The_Word_Length            := The_Word_Length + 1;
                     The_Word (The_Word_Length) := The_Character;

                     Next_Character;
                  end loop;
                  exit;

               -- comment character

               when ';' =>
                  Next_Character;
                  while The_Character /= Ada.Characters.Latin_1.LF and
                    The_Character /= Ada.Characters.Latin_1.CR
                  loop
                     Next_Character;
                  end loop;
                  Next_Character;

               when others =>
                  raise Character_Error;
            end case;
         end loop;
      end Next_Word;

   begin
      Open (The_Object_File, In_File, The_File_Name);

      The_Number := 1;
      Read_Line;

      The_Character := The_Line (The_Position);
      Next_Word;

      loop
         Op_IO.Get (The_Word (1 .. The_Word_Length) & "_op", The_Op, The_Last);

         case The_Op is
            when hlt_op =>
               The_Instruction := (The_Op, 0, 0);

            when nop_op =>
               The_Instruction := (The_Op, 0, 0);
               Next_Word;

            -- opcode, size.

            when lod_op |
              lodb_op   |
              sto_op    |
              stob_op   |
              rd_op     |
              rdb_op    |
              wrt_op    |
              wrtb_op   |
              equ_op    |
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

               Next_Word;

               Word_IO.Get
                 (The_Word (1 .. The_Word_Length),
                  The_Size,
                  The_Last);

               The_Instruction := (The_Op, The_Size, 0);

               Load.Skip_Line;
               Next_Word;

            -- opcode, size, operand.

            when incs_op | decs_op | lit_op | filb_op =>

               Next_Word;
               Word_IO.Get
                 (The_Word (1 .. The_Word_Length),
                  The_Size,
                  The_Last);

               Next_Word;

               Word_IO.Get
                 (The_Word (1 .. The_Word_Length),
                  The_Operand,
                  The_Last);

               The_Instruction := (The_Op, The_Size, The_Operand);

               Load.Skip_Line;
               Next_Word;

            -- opcode, operand.

            when jmp_op | jz_op =>
               The_Instruction := (The_Op, 0, 0);

               Next_Word;

               Word_IO.Get
                 (The_Word (1 .. The_Word_Length),
                  The_Operand,
                  The_Last);

               The_Instruction := (The_Op, 0, The_Operand);

               Load.Skip_Line;
               Next_Word;

         end case;

         The_Code (The_Counter) := The_Instruction;
         The_Counter            := The_Counter + 1;

         exit when The_Instruction.The_Code = hlt_op;
      end loop;

      Close (The_Object_File);

      The_Length := Natural (The_Counter);

   exception

      when Ada.Text_IO.End_Error =>
         raise PCode_Package.IO_Package.End_Error;

      when Ada.Text_IO.Name_Error =>
         raise PCode_Package.IO_Package.Name_Error;

      when Ada.Text_IO
        .Status_Error | Ada.Text_IO
        .Mode_Error | Ada.Text_IO
        .Use_Error | Ada.Text_IO
        .Device_Error | Ada.Text_IO
        .Data_Error =>
         raise IO_Error;

   end Load;

   procedure Save
     (The_File_Name : String;
      The_Code      : Code;
      The_Length    : Natural)
   is
      The_File : File_Type;
   begin
      Create (The_File, Out_File, The_File_Name);
      Put_Line (The_File, "; " & The_File_Name);
      New_Line (The_File);
      for The_Index in 0 .. The_Length - 1 loop
         Put_Line (The_File, String_Of (The_Code (Address (The_Index))));
      end loop;
      Close (The_File);

   exception

      when Ada.Text_IO.Name_Error =>
         raise PCode_Package.IO_Package.Name_Error;

      when Ada.Text_IO
        .Status_Error | Ada.Text_IO
        .Mode_Error | Ada.Text_IO
        .Use_Error | Ada.Text_IO
        .Device_Error | Ada.Text_IO
        .End_Error | Ada.Text_IO
        .Data_Error | Ada.Text_IO
        .Layout_Error =>
         raise IO_Error;

   end Save;

   procedure List (The_Code : Code; The_Length : Natural) is
   begin
      for The_Index in 0 .. The_Length - 1 loop
         Put (The_Index, 4);
         Put_Line (" : " & String_Of (The_Code (Address (The_Index))));
      end loop;
   end List;

end PCode_Package.IO_Package;

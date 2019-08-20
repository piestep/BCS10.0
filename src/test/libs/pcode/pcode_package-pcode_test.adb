-- BCS Boolean Compiler System
-- Copyright (c) 2017 Paul Estep

with AUnit.Assertions; use AUnit.Assertions;
--
with Pool_Package;
with Test_Package; use Test_Package;
with System_Package;  use System_Package;
--

package body PCode_Package.PCode_Test is
   HLT_INSTRUCTION  : constant Instruction := (hlt_op, 0, 0);
   NOP_INSTRUCTION  : constant Instruction := (nop_op, 0, 0);
   INCS_INSTRUCTION : constant Instruction := (incs_op, 0, 0);
   DECS_INSTRUCTION : constant Instruction := (decs_op, 0, 0);
   LIT_INSTRUCTION  : constant Instruction := (lit_op, 0, 0);
   FILB_INSTRUCTION : constant Instruction := (filb_op, 0, 0);
   LOD_INSTRUCTION  : constant Instruction := (lod_op, 0, 0);
   LODB_INSTRUCTION : constant Instruction := (lodb_op, 0, 0);
   STO_INSTRUCTION  : constant Instruction := (sto_op, 0, 0);
   STOB_INSTRUCTION : constant Instruction := (stob_op, 0, 0);
   RD_INSTRUCTION   : constant Instruction := (rd_op,  0, 0);
   RDB_INSTRUCTION  : constant Instruction := (rdb_op, 0, 0);
   WRT_INSTRUCTION  : constant Instruction := (wrt_op, 0, 0);
   WRTB_INSTRUCTION : constant Instruction := (wrtb_op, 0, 0);
   JMP_INSTRUCTION  : constant Instruction := (jmp_op, 0, 0);
   JZ_INSTRUCTION   : constant Instruction := (jz_op,  0, 0);
   EQU_INSTRUCTION  : constant Instruction := (equ_op, 0, 0);
   NEQ_INSTRUCTION  : constant Instruction := (neq_op, 0, 0);
   LS_INSTRUCTION   : constant Instruction := (ls_op,  0, 0);
   LEQ_INSTRUCTION  : constant Instruction := (leq_op, 0, 0);
   GTR_INSTRUCTION  : constant Instruction := (gtr_op, 0, 0);
   GEQ_INSTRUCTION  : constant Instruction := (geq_op, 0, 0);
   ULS_INSTRUCTION  : constant Instruction := (uls_op, 0, 0);
   ULEQ_INSTRUCTION : constant Instruction := (uleq_op, 0, 0);
   UGTR_INSTRUCTION : constant Instruction := (ugtr_op, 0, 0);
   UGEQ_INSTRUCTION : constant Instruction := (ugeq_op, 0, 0);
   NOT_INSTRUCTION  : constant Instruction := (not_op, 0, 0);
   AND_INSTRUCTION  : constant Instruction := (and_op, 0, 0);
   OR_INSTRUCTION   : constant Instruction := (or_op,  0, 0);
   XOR_INSTRUCTION  : constant Instruction := (xor_op, 0, 0);
   NEG_INSTRUCTION  : constant Instruction := (neg_op, 0, 0);
   ADD_INSTRUCTION  : constant Instruction := (add_op, 0, 0);
   SUB_INSTRUCTION  : constant Instruction := (sub_op, 0, 0);
   MUL_INSTRUCTION  : constant Instruction := (mul_op, 0, 0);
   DIV_INSTRUCTION  : constant Instruction := (div_op, 0, 0);
   MOD_INSTRUCTION  : constant Instruction := (mod_op, 0, 0);
   REM_INSTRUCTION  : constant Instruction := (rem_op, 0, 0);
   UMUL_INSTRUCTION : constant Instruction := (umul_op, 0, 0);
   UDIV_INSTRUCTION : constant Instruction := (udiv_op, 0, 0);
   UMOD_INSTRUCTION : constant Instruction := (umod_op, 0, 0);
   UREM_INSTRUCTION : constant Instruction := (urem_op, 0, 0);

   ----------
   -- Name --
   ----------

   function Name (The_Test : in Test) return AUnit.Test_String is
   begin
      return AUnit.Format ("pcode_package.pcode_test!");
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (The_Test : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Repeat for each test routine:
      Register_Routine (The_Test, Test_String_Of'Access, "test_string_of!");
   end Register_Tests;

   --------------------
   -- Test_String_Of --
   -------------------0

   procedure Test_String_Of
     (The_Test : in out Test_Case'Class)
   is
      pragma Unreferenced (The_Test);

   begin
      Assert (String_Of (HLT_INSTRUCTION) = "HLT", "(1) Test string of.");
      Assert (String_Of (NOP_INSTRUCTION) = "NOP", "(2) Test string of.");
      Assert (String_Of (INCS_INSTRUCTION) = "INCS 0 0", "(3) Test string of.");
      Assert (String_Of (DECS_INSTRUCTION) = "DECS 0 0", "(4) Test string of.");
      Assert (String_Of (LIT_INSTRUCTION) = "LIT  0 0", "(5) Test string of." & String_Of (LIT_INSTRUCTION));
      Assert (String_Of (FILB_INSTRUCTION) = "FILB 0 0", "(6) Test string of.");
      Assert (String_Of (LOD_INSTRUCTION) = "LOD  0", "(7) Test string of.");
      Assert (String_Of (LODB_INSTRUCTION) = "LODB 0", "(8) Test string of.");
      Assert (String_Of (STO_INSTRUCTION) = "STO  0", "(9) Test string of.");
      Assert (String_Of (STOB_INSTRUCTION) = "STOB 0", "(10) Test string of.");
      Assert (String_Of (RD_INSTRUCTION) = "RD   0", "(11) Test string of.");
      Assert (String_Of (RDB_INSTRUCTION) = "RDB  0", "(12) Test string of.");
      Assert (String_Of (WRT_INSTRUCTION) = "WRT  0", "(13) Test string of.");
      Assert (String_Of (WRTB_INSTRUCTION) = "WRTB 0", "(14) Test string of.");
      Assert (String_Of (JMP_INSTRUCTION) = "JMP  0", "(15) Test string of.");
      Assert (String_Of (JZ_INSTRUCTION) = "JZ   0", "(16) Test string of.");
      Assert (String_Of (EQU_INSTRUCTION) = "EQU  0", "(17) Test string of.");
      Assert (String_Of (NEQ_INSTRUCTION) = "NEQ  0", "(18) Test string of.");
      Assert (String_Of (LS_INSTRUCTION) = "LS   0", "(19) Test string of.");
      Assert (String_Of (LEQ_INSTRUCTION) = "LEQ  0", "(20) Test string of.");
      Assert (String_Of (GTR_INSTRUCTION) = "GTR  0", "(21) Test string of.");
      Assert (String_Of (GEQ_INSTRUCTION) = "GEQ  0", "(22) Test string of.");
      Assert (String_Of (ULS_INSTRUCTION) = "ULS  0", "(23) Test string of.");
      Assert (String_Of (ULEQ_INSTRUCTION) = "ULEQ 0", "(24) Test string of.");
      Assert (String_Of (UGTR_INSTRUCTION) = "UGTR 0", "(25) Test string of.");
      Assert (String_Of (UGEQ_INSTRUCTION) = "UGEQ 0", "(26) Test string of.");
      Assert (String_Of (NOT_INSTRUCTION) = "NOT  0", "(27) Test string of.");
      Assert (String_Of (AND_INSTRUCTION) = "AND  0", "(28) Test string of.");
      Assert (String_Of (OR_INSTRUCTION) = "OR   0", "(29) Test string of.");
      Assert (String_Of (XOR_INSTRUCTION) = "XOR  0", "(30) Test string of.");
      Assert (String_Of (NEG_INSTRUCTION) = "NEG  0", "(31) Test string of.");
      Assert (String_Of (ADD_INSTRUCTION) = "ADD  0", "(32) Test string of.");
      Assert (String_Of (SUB_INSTRUCTION) = "SUB  0", "(33) Test string of.");
      Assert (String_Of (MUL_INSTRUCTION) = "MUL  0", "(34) Test string of.");
      Assert (String_Of (DIV_INSTRUCTION) = "DIV  0", "(35) Test string of.");
      Assert (String_Of (MOD_INSTRUCTION) = "MOD  0", "(36) Test string of.");
      Assert (String_Of (REM_INSTRUCTION) = "REM  0", "(37) Test string of.");
      Assert (String_Of (UMUL_INSTRUCTION) = "UMUL 0", "(38) Test string of.");
      Assert (String_Of (UDIV_INSTRUCTION) = "UDIV 0", "(39) Test string of.");
      Assert (String_Of (UMOD_INSTRUCTION) = "UMOD 0", "(40) Test string of.");
      Assert (String_Of (UREM_INSTRUCTION) = "UREM 0", "(41) Test string of.");
   end Test_String_Of;

end PCode_Package.PCode_Test;

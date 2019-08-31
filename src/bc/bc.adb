-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.IO_Exceptions;
--
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Directories;        use Ada.Directories;
--
with BC_Package;     use BC_Package;
with System_Package; use System_Package;
with Debug_Package;  use Debug_Package;
--
with Error_Package;
with Source_Package;
with List_Package;
with Scanner_Package;
with Graph_Package;
with Syntax_Package;
with Scope_Package;
with Semantics_Package;
with Optimize_Package;
with Generate_Package;
with PCode_Package;
with PCode_Package.IO_Package;
with Block_Package;
with BCode_Package;
with BCode_Package.IO_Package;
with Parameter_Package;
with Parameter_Package.IO_Package;
--
with Graph_Package.List;
with Graph_Package.Dump;
--
with System.Storage_Elements;
with Pool_Package;
with Type_Package;
with Identifier_Package;
with Operand_Package;
with Number_Package;
--

-- BC (Boolean Compiler) translates programs of fixed size input,
-- fixed size output, and of fixed memory size into
-- boolean equations called bcode.
--
-- In addiation it can translate the program into pseduo code or
-- pcode.
--
-- BC is the main Ada procedure for starting the compliation process.
--
-- BC ussage:
--
-- bc [ switches ] <filename>
--
-- switches:
--

procedure BC is

   List_PCode      : Boolean := False; -- list pcode switch
   List_BCode      : Boolean := False; -- list bcode switch
   No_Optimization : Boolean := False; -- optimize swith
   Boolean_Output  : Boolean := False; -- list bcode in boolean switch
   Generate_Test   : Boolean := False; -- generate test switch.

   Show_Memory     : Boolean := False;     -- memory switch
   List_Graph      : Boolean := False;     -- list graph switch
   Dump_Graph      : Boolean := False;     -- dump graph switch
   Full_Dump       : Boolean := False;     -- full dump graph switch

   PROGRAM_NAME    : constant String := "bc";
   PROGRAM_VERSION : constant String := "1.0";

   The_Source_File_Name : Unbounded_String;
   The_Output_Directory : Unbounded_String := Null_Unbounded_String;
   The_ACode_Directory  : Unbounded_String := Null_Unbounded_String;

   The_Argument : SYSPositive := 1;

   -- The Graph.

   The_Unit : Graph_Package.Compilation_Unit_Graph;

   -- The PCode.

   The_Code   : PCode_Package.Code (PCode_Package.Address'Range);
   The_Length : PCode_Package.Address;

   -- The BCode.

   The_Variables : SYSNatural;
   The_Words     : BCode_Package.BCode_Words;

   -- The Parameter (Parameter Data)

   The_Inputs  : Parameter_Package.Parameter_List_Type;
   The_Outputs : Parameter_Package.Parameter_List_Type;

   -- Temporary identifier

   The_Identifier : Identifier_Package.Identifier_Pointer;

   package Size_IO is new Integer_IO (System.Storage_Elements.Storage_Count);

   -- Print usage information.

   procedure Put_Usage is
   begin
      Put_Line ("usage:  bc [ switches ] <filename>");
      Put_Line ("  -h,  --help                      - print usage");
      Put_Line ("  -pc, --pcode                     - generate pcode");
      Put_Line
        ("  -o,  --output <directory>        - output files to directory");
      Put_Line
        ("  -ao, --acode_output <directory>  - acode output files to directory");
      Put_Line ("  -ac, --acode                     - generate acode");
      Put_Line ("  -nl, --no_listing                - no source listing");
      Put_Line ("  -no, --no_optimize               - no optimization");
      Put_Line ("  -lp. --list_pcode                - list pcode");
      Put_Line ("  -lb, --list_bcode                - list bcode");
      Put_Line ("  -bb, --boolean_bcode             - list bcode in boolean");
      Put_Line
        ("  --source_debug                   - source_package messages");
      Put_Line
        ("  --scanner_debug                  - scanner_package messages");
      Put_Line
        ("  --syntax_debug                   - syntax_package messages");
      Put_Line
        ("  --semenatics_debug               - semenatics_package messages");
      Put_Line
        ("  --optimize_debug                 - optimize_package messages");
      Put_Line ("  --list_graph                     - list graph syntax");
      Put_Line ("  --dump_graph                     - dump graph semenatics");
      Put_Line
        ("  --full_dump                      - fully dump graph semenatics");
      Put_Line ("  --block_debug                    - block_package messages");
      Put_Line ("  --pcode_debug                    - pcode_package messages");
      Put_Line
        ("  --pcode_dump                     - pcode_package dump immediately");
      Put_Line ("  --bcode_debug                    - bcode_package messages");
      Put_Line ("  --block_dump                     - block dumps");
      Put_Line ("  --scope_dump                     - scope dumps");
      Put_Line ("  --memory                         - memory usage");
   end Put_Usage;

   -- Return source code filename without file extension.

   function Source_Name_Of return String is
   begin
      return Base_Name (To_String (The_Source_File_Name));
   end Source_Name_Of;

   -- Return bcode/pcode directory.

   function Code_Directory_Of return String is
   begin
      return Base_Name (To_String (The_Source_File_Name));
   end Code_Directory_Of;

   -- Return acode directory.

   function ACode_Directory_Of return String is
   begin
      return Base_Name (To_String (The_Source_File_Name));
   end ACode_Directory_Of;

   -- Mark current memory usage of program data pointers.

   procedure Mark_Memory is
   begin
      -- Mark memory pools.
      Pool_Package.Mark_Allocations (Type_Package.The_Pool);
      Pool_Package.Mark_Allocations (Identifier_Package.The_Pool);
      Pool_Package.Mark_Allocations (Operand_Package.The_Pool);
      Pool_Package.Mark_Allocations (Graph_Package.The_Pool);

      Pool_Package.Mark_Allocations (Number_Package.The_Pool);
   end Mark_Memory;

   -- Print memory usage of bc.

   procedure Put_Memory is

      -- Print memory usage of program data pointers.

      procedure Put_Pool
        (The_Package : String;
         The_Pool    : Pool_Package.Storage_Pool)
      is
      begin
         Set_Col (1);
         Put (The_Package);
         Set_Col (20);
         Put (The_Pool.The_Marked_Allocations, 7);
         Set_Col (28);
         Put (The_Pool.The_Number_Of_Allocations, 11);
         Set_Col (40);
         Put (The_Pool.The_Number_Of_Deallocations, 13);
         Set_Col (54);
         Size_IO.Put (The_Pool.The_Maximum_Size_Of_Allocations, 14);
         Set_Col (69);
         Put
           (The_Pool.The_Number_Of_Allocations -
              (The_Pool.The_Marked_Allocations +
                   The_Pool.The_Number_Of_Deallocations),
            7);
         New_Line;
      end Put_Pool;

   begin
      -- Heading

      Set_Col (1);
      Put ("Packages");                -- 20
      Set_Col (20);
      Put ("Marked");                  -- 8
      Set_Col (28);
      Put ("Allocations");             -- 12
      Set_Col (40);
      Put ("Deallocations");   -- 14
      Set_Col (54);
      Put ("Maximum Size");    -- 15
      Set_Col (69);
      Put ("Current");                 -- 8
      New_Line;
      New_Line;

      -- Memory.
      Put_Pool ("Types", Type_Package.The_Pool);
      Put_Pool ("Identifiers", Identifier_Package.The_Pool);
      Put_Pool ("Operands", Operand_Package.The_Pool);
      Put_Pool ("Graph", Graph_Package.The_Pool);

      Put_Pool ("Numbers", Number_Package.The_Pool);
   end Put_Memory;

begin
   Put_Line (PROGRAM_NAME & " version " & PROGRAM_VERSION);

   -- parse command line.

   if Argument_Count < 1
     or else
       (Argument_Count = 1 and
          (Argument (The_Argument) = "-h" or Argument (1) = "--help"))
   then
      Put_Usage;
      return;
   end if;

   while The_Argument <= Argument_Count - 1 loop
      if Argument (The_Argument) = "-h" or
        Argument (The_Argument) = "--help"
      then
         Put_Usage;
         return;

      elsif Argument (The_Argument) = "-o" or
        Argument (The_Argument) = "--output"
      then
         The_Argument         := The_Argument + 1;
         The_Output_Directory := To_Unbounded_String (Argument (The_Argument));
      elsif Argument (The_Argument) = "-ao" or
        Argument (The_Argument) = "--acode_output"
      then
         The_Argument        := The_Argument + 1;
         The_ACode_Directory := To_Unbounded_String (Argument (The_Argument));

      elsif Argument (The_Argument) = "-nl" or
        Argument (The_Argument) = "--no_listing"
      then
         List_Package.List_Source := False;

      elsif Argument (The_Argument) = "-no" or
        Argument (The_Argument) = "--no_optimize"
      then
         No_Optimization := True;

      elsif Argument (The_Argument) = "-lp" or
        Argument (The_Argument) = "--list_pcode"
      then
         List_PCode := True;
      elsif Argument (The_Argument) = "-lb" or
        Argument (The_Argument) = "--list_bcode"
      then
         List_BCode := True;
      elsif Argument (The_Argument) = "-bb" or
        Argument (The_Argument) = "--boolean_bcode"
      then
         Boolean_Output := True;

      elsif Argument (The_Argument) = "-pc" or
        Argument (The_Argument) = "--pcode"
      then
         Generate_Package.Generate_PCode := True;
         Generate_Test                   := True;
      elsif Argument (The_Argument) = "-bc" or
        Argument (The_Argument) = "--bcode"
      then
         Generate_Package.Generate_BCode := True;
      elsif Argument (The_Argument) = "-ac" or
        Argument (The_Argument) = "--acode"
      then
         Generate_Package.Generate_ACode := True;
         Generate_Test                   := True;

      elsif Argument (The_Argument) = "--source_debug" then
         Source_Package.Source_Debug := True;
      elsif Argument (The_Argument) = "--scanner_debug" then
         Scanner_Package.Scanner_Debug := True;
      elsif Argument (The_Argument) = "--syntax_debug" then
         Syntax_Package.Syntax_Debug := True;
      elsif Argument (The_Argument) = "--scope_debug" then
         Scope_Package.Scope_Debug := True;
      elsif Argument (The_Argument) = "--semenatics_debug" then
         Semantics_Package.Semantics_Debug := True;
      elsif Argument (The_Argument) = "--optimize_debug" then
         Optimize_Package.Optimize_Debug := True;
      elsif Argument (The_Argument) = "--pcode_debug" then
         Generate_Package.PCode_Debug := True;
      elsif Argument (The_Argument) = "--pcode_dump" then
         Generate_Package.PCode_Dump := True;
      elsif Argument (The_Argument) = "--block_debug" then
         Block_Package.Block_Debug := True;
      elsif Argument (The_Argument) = "--bcode_debug" then
         Generate_Package.BCode_Debug := True;

      elsif Argument (The_Argument) = "--list_graph" then
         List_Graph := True;
      elsif Argument (The_Argument) = "--dump_graph" then
         Dump_Graph := True;
      elsif Argument (The_Argument) = "--full_dump" then
         Full_Dump := True;
      elsif Argument (The_Argument) = "--scope_dump" then
         Scope_Package.Scope_Dump := True;
      elsif Argument (The_Argument) = "--block_dump" then
         Block_Package.Block_Dump := True;
      elsif Argument (The_Argument) = "--memory" then
         Show_Memory := True;

      else
         Put_Usage;
         return;
      end if;
      The_Argument := The_Argument + 1;
   end loop;

   The_Source_File_Name := To_Unbounded_String (Argument (Argument_Count));

   Put_Line (To_String (The_Source_File_Name));

   -- Open source file and scanner.

   Source_Package.Open (To_String (The_Source_File_Name));
   Scanner_Package.Next_Symbol;

   -- Open initial scope.

   Scope_Package.Open;
   The_Identifier := new Identifier_Package.Type_Identifier'
     (The_Previous => Identifier_Package.The_Last,
      The_String => Scanner_Package.Boolean_String,
      The_Type   => Type_Package.Boolean_Type);
   Identifier_Package.The_Last := The_Identifier;
   Scope_Package.Enter (The_Identifier);

   The_Identifier := new Identifier_Package.Constant_Identifier'
     (The_Previous => Identifier_Package.The_Last,
      The_String => Scanner_Package.False_String,
      The_Type   => Type_Package.Universal_Boolean,
      --           The_Type   => Type_Package.Boolean_Type,
      The_Value  => 0);
   Identifier_Package.The_Last := The_Identifier;
   Scope_Package.Enter (The_Identifier);

   The_Identifier :=new Identifier_Package.Constant_Identifier'
     (The_Previous => Identifier_Package.The_Last,
      The_String => Scanner_Package.True_String,
      The_Type   => Type_Package.Universal_Boolean,
      --           The_Type   => Type_Package.Boolean_Type,
      The_Value  => 1);
   Identifier_Package.The_Last := The_Identifier;
   Scope_Package.Enter (The_Identifier);

   The_Identifier := new Identifier_Package.Type_Identifier'
     (The_Previous => Identifier_Package.The_Last,
      The_String => Scanner_Package.Integer_String,
      The_Type   => Type_Package.Integer_Type);
   Identifier_Package.The_Last := The_Identifier;
   Scope_Package.Enter (The_Identifier);

   -- Mark memory usage.

   if Show_Memory then
      Mark_Memory;
   end if;

   -- parse program.

   Syntax_Package.Parse (The_Unit);
   Source_Package.Close;

   -- List graph.

   if List_Graph and Error_Package.The_Number_Of_Errors = 0 then
      Graph_Package.List (The_Unit);
   end if;

   -- Check semenatics.

   if Error_Package.The_Number_Of_Errors = 0 then
      Semantics_Package.Parse (The_Unit);
   end if;

   -- Close scope.

   Scope_Package.Close;

   -- Optimize.

   if not No_Optimization and Error_Package.The_Number_Of_Errors = 0 then
      Optimize_Package.Optimize (The_Unit);
   end if;

   -- Dump graph if requested.

   if Dump_Graph and Error_Package.The_Number_Of_Errors = 0 then
      Put_Line ("Graph");
      Graph_Package.Dump (The_Unit, Full_Dump);
      New_Line;
   end if;

   -- Generate PCode.

   if Generate_Package.Generate_PCode and
     Error_Package.The_Number_Of_Errors = 0
   then

      Generate_Package.PCode (The_Unit, The_Code, The_Length);

      if Error_Package.The_Number_Of_Errors = 0 then
         if The_Output_Directory = Null_Unbounded_String then
            PCode_Package.IO_Package.Save
              (Source_Name_Of & PCODE_EXTENSION,
               The_Code,
               SYSNatural (The_Length));
         else
            PCode_Package.IO_Package.Save
              (To_String (The_Output_Directory) &
               '\' &
                 Source_Name_Of &
                 PCODE_EXTENSION,
               The_Code,
               SYSNatural (The_Length));
         end if;
      end if;
   end if;

   -- Generate BCode.

   if Generate_Package.Generate_BCode and
     Error_Package.The_Number_Of_Errors = 0
   then

      Generate_Package.BCode (The_Unit, The_Variables, The_Words);

      if Error_Package.The_Number_Of_Errors = 0 then
         if The_Output_Directory = Null_Unbounded_String then
            BCode_Package.IO_Package.Save
              (Source_Name_Of & BCODE_EXTENSION,
               The_Variables,
               The_Words);
         else
            BCode_Package.IO_Package.Save
              (To_String (The_Output_Directory) &
               '\' &
                 Source_Name_Of &
                 BCODE_EXTENSION,
               The_Variables,
               The_Words);
         end if;
      end if;
   end if;

   -- Generate ACode.

   if Generate_Package.Generate_ACode and
     Error_Package.The_Number_Of_Errors = 0
   then
      if The_ACode_Directory = Null_Unbounded_String then
         if The_Output_Directory = Null_Unbounded_String then
            Generate_Package.ACode (The_Unit, Source_Name_Of);
         else
            Generate_Package.ACode
              (The_Unit,
               To_String (The_Output_Directory) & '\' & Source_Name_Of);
         end if;
      else
         Generate_Package.ACode
           (The_Unit,
            To_String (The_ACode_Directory) & '\' & Source_Name_Of);
      end if;
   end if;

   -- Generate Test.
   -- Save additional bcode/pcode compiler information
   -- for testing purposes.

   if (Generate_Package.Generate_PCode or Generate_Package.Generate_BCode) and
     Generate_Test and
     Error_Package.The_Number_Of_Errors = 0
   then
      Generate_Package.Parameters (The_Unit, The_Inputs, The_Outputs);

      if Error_Package.The_Number_Of_Errors = 0 then
         if The_Output_Directory = Null_Unbounded_String then
            Parameter_Package.IO_Package.Save
              (Source_Name_Of & PBTEST_EXTENSION,
               The_Inputs,
               The_Outputs);
         else
            Parameter_Package.IO_Package.Save
              (To_String (The_Output_Directory) &
               '\' &
                 Source_Name_Of &
                 PBTEST_EXTENSION,
               The_Inputs,
               The_Outputs);
         end if;
      end if;
   end if;

   -- List errors and program source.

   if List_Package.List_Source then
      List_Package.List (To_String (The_Source_File_Name));
   else
      Error_Package.List_Messages;
   end if;

   if Error_Package.The_Number_Of_Errors = 0 and
     Error_Package.The_Number_Of_Warnings = 0
   then
      New_Line;
      Put_Line ("Compilation Complete: No Errors and Warnings Found.");

   elsif Error_Package.The_Number_Of_Errors = 0 and
     Error_Package.The_Number_Of_Warnings > 0
   then
      New_Line;
      Put_Line
        ("Compilation Complete: No Errors and" &
           SYSNatural'Image (Error_Package.The_Number_Of_Warnings) &
           " Warinings Found.");
   else
      New_Line;
      Put_Line
        ("Compilation Complete:" &
           SYSNatural'Image (Error_Package.The_Number_Of_Errors) &
           " Errors Found.");
   end if;

   -- List pcode if no errors.

   if Generate_Package.Generate_PCode and
     Error_Package.The_Number_Of_Errors = 0
   then
      if List_PCode then
         New_Line;
         PCode_Package.IO_Package.List (The_Code, SYSNatural (The_Length));
      end if;
   end if;

   -- List bcode if no errors.

   if Generate_Package.Generate_BCode and
     Error_Package.The_Number_Of_Errors = 0
   then
      if List_BCode then
         New_Line;
         BCode_Package.IO_Package.List
           (The_Variables,
            The_Words,
            Boolean_Output);
      end if;
   end if;

   -- Print memory usage.

   if Show_Memory and Error_Package.The_Number_Of_Errors = 0 then
      New_Line;
      Put_Memory;
   end if;

exception

   when Ada.IO_Exceptions.End_Error =>
      Put
        ("*** " &
           Source_Package.Line_Number'Image
           (Source_Package.The_Position.The_Line) &
           "," &
           Source_Package.Column_Position'Image
           (Source_Package.The_Position.The_Column) &
           ":");
      Put_Line (" Unexpected End Of File.");

   when PCode_Package.IO_Package
      .IO_Error | PCode_Package.IO_Package
      .Name_Error =>
      Error_Package.Compiler_Error ("Unable to save PCode.");

   when BCode_Package.IO_Package
      .IO_Error | BCode_Package.IO_Package
      .Name_Error =>
      Error_Package.Compiler_Error ("Unable to save BCode.");

   when Parameter_Package.IO_Package
      .Name_Error | Parameter_Package.IO_Package
      .IO_Error =>
      Error_Package.Compiler_Error ("Unable to save PCode test data.");

   when Error_Package.Compiler_Exception =>
      -- Critical error message reported by Error_Package.
      null;

end BC;

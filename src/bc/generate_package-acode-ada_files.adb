-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

separate (Generate_Package.ACode)
procedure Ada_Files
  (The_Name    : String;
   The_Package : Unbounded_String;
   The_Date    : String)
is

   -- BC Language constructs.

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Identifier : Identifier_Symbol_Graph);

   procedure Generate_Type
     (The_File       : Ada.Text_IO.File_Type;
      The_Identifier : Identifier_Symbol_Graph);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Expression_Graph);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Unary_Expression_Graph);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Binary_Expression_Graph);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Variable_Expression_Graph);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Operand_Expression_Graph);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Integer_Expression_Graph);

   procedure Generate
     (The_File     : Ada.Text_IO.File_Type;
      The_Variable : Variable_Graph);

   procedure Generate
     (The_File        : Ada.Text_IO.File_Type;
      The_Parameters  : Parameter_Graph;
      The_Indentation : Natural);

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Identifier : Identifier_Symbol_Graph)
   is
   begin
      if The_Identifier /= null then
         Put (The_File, " " & To_String (The_Identifier.The_String));
      end if;
   end Generate;

   procedure Generate_Type
     (The_File       : Ada.Text_IO.File_Type;
      The_Identifier : Identifier_Symbol_Graph)
   is
   begin
      if The_Identifier /= null then
         Put (The_File, " " & Type_Of (To_String (The_Identifier.The_String)));
      end if;
   end Generate_Type;

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Expression_Graph)
   is
   begin
      if The_Expression /= null then
         if The_Expression.all in Unary_Expression_Node then
            Generate (The_File, Unary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Binary_Expression_Node then
            Generate (The_File, Binary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Variable_Expression_Node then
            Generate (The_File, Variable_Expression_Graph (The_Expression));
         elsif The_Expression.all in Integer_Expression_Node then
            Generate (The_File, Integer_Expression_Graph (The_Expression));
         elsif The_Expression.all in Operand_Expression_Node then
            Generate (The_File, Operand_Expression_Graph (The_Expression));
         else
            raise Critical_Error;
         end if;
      end if;
   end Generate;

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Unary_Expression_Graph)
   is
   begin
      if The_Expression /= null then
         Put (The_File, " (");

         case The_Expression.The_Operator is
            when Not_Symbol =>
               Put (The_File, " not");
            when Minus_Symbol =>
               Put (The_File, " -");
            when others =>
               raise Critical_Error;
         end case;

         Generate (The_File, The_Expression.The_Right);

         Put (The_File, " )");
      end if;
   end Generate;

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Binary_Expression_Graph)
   is
   begin
      if The_Expression /= null then
         Put (The_File, " (");
         Generate (The_File, The_Expression.The_Left);

         case Binary_Expression_Node (The_Expression.all).The_Operator is

            when Equal_Symbol =>
               Put (The_File, " =");
            when Not_Equal_Symbol =>
               Put (The_File, " /=");
            when Less_Than_Symbol =>
               Put (The_File, " <");
            when Less_Than_Equal_Symbol =>
               Put (The_File, " <=");
            when Greater_Than_Symbol =>
               Put (The_File, " >");
            when Greater_Than_Equal_Symbol =>
               Put (The_File, " >=");
            when And_Symbol =>
               Put (The_File, " and");
            when Or_Symbol =>
               Put (The_File, " or");
            when Xor_Symbol =>
               Put (The_File, " xor");
            when Plus_Symbol =>
               Put (The_File, " +");
            when Minus_Symbol =>
               Put (The_File, " -");
            when Times_Symbol =>
               Put (The_File, " *");
            when Divide_Symbol =>
               Put (The_File, " /");
            when Mod_Symbol =>
               Put (The_File, " mod");
            when Rem_Symbol =>
               Put (The_File, " rem");
            when others =>
               raise Critical_Error;
         end case;

         Generate (The_File, The_Expression.The_Right);
         Put (The_File, " )");
      end if;
   end Generate;

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Variable_Expression_Graph)
   is
   begin
      if The_Expression /= null then
         Put (The_File, " ");
         Generate (The_File, The_Expression.The_Variable);
      end if;
   end Generate;

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Integer_Expression_Graph)
   is
   begin
      if The_Expression /= null then
         if Is_Boolean (The_Expression.The_Result.The_Type) then
            if Constant_Operand (The_Expression.The_Result.all).The_Value =
              Integer (Boolean_False)
            then
               Put (The_File, " False");
            else
               Put (The_File, " True");
            end if;
         else
            Put (The_File, " " & To_String (The_Expression.The_String));
         end if;
      end if;
   end Generate;

   procedure Generate
     (The_File       : Ada.Text_IO.File_Type;
      The_Expression : Operand_Expression_Graph)
   is
   begin
      if The_Expression /= null then
         if Is_Boolean (The_Expression.The_Result.The_Type) then
            if Constant_Operand (The_Expression.The_Result.all).The_Value =
              Integer (Boolean_False)
            then
               Put (The_File, " False");
            else
               Put (The_File, " True");
            end if;
         else
            Put
              (The_File,
               Integer'Image
                 (Constant_Operand (The_Expression.The_Result.all).The_Value));
         end if;
      end if;
   end Generate;

   procedure Generate
     (The_File     : Ada.Text_IO.File_Type;
      The_Variable : Variable_Graph)
   is
   begin
      if The_Variable /= null then
         Generate (The_File, The_Variable.The_Identifier);

         if The_Variable.The_Expression /= null then
            Put (The_File, " (");
            Generate (The_File, The_Variable.The_Expression);
            Put (The_File, " )");
         end if;
      end if;
   end Generate;

   procedure Generate
     (The_File        : Ada.Text_IO.File_Type;
      The_Parameters  : Parameter_Graph;
      The_Indentation : Natural)
   is
      The_Parameter : Parameter_Graph := The_Parameters;

      procedure Generate_Parameter (The_Parameter : Parameter_Graph) is
      begin
         Generate (The_File, The_Parameter.The_Identifier);
         Put (The_File, " :");
         if The_Parameter.Is_In then
            Put (The_File, " in");
         end if;
         if The_Parameter.Is_Out then
            Put (The_File, " out");
         end if;
         Generate_Type (The_File, The_Parameter.The_Definition);
      end Generate_Parameter;

   begin
      if The_Parameter /= null then
         Put (The_File, (1 .. The_Indentation => ' '));
         Put (The_File, "(");
         Generate_Parameter (The_Parameter);
         The_Parameter := The_Parameter.The_Next;

         while The_Parameter /= null loop
            Put (The_File, ";");
            New_Line (The_File);

            Put (The_File, (1 .. The_Indentation => ' ') & ' ');
            Generate_Parameter (The_Parameter);
            The_Parameter := The_Parameter.The_Next;
         end loop;
         Put (The_File, " )");
      end if;
   end Generate;

--      -- BC Boolean Compiler Ada Test Generator.
--      -- Generated from 'TEST.BC', 10/12/2007
--
--      package Test is
--
--              type Value is new Integer range -8 .. 7;
--
--              procedure Run (The_Input : Value; The_Output : out Value);
--
--      end Test;

   procedure Specification_File
     (The_Name    : String;
      The_Package : Unbounded_String;
      The_Date    : String)
   is

      The_File : File_Type;

      procedure Generate (The_Unit : Compilation_Unit_Graph);
      procedure Generate (The_Package : Package_Body_Graph);
      procedure Generate (The_Procedure : Procedure_Body_Graph);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Declarations     : Declaration_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Declaration      : Type_Declaration_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Declaration      : Identifier_Declaration_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Definition_Graph);

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Range_Definition_Graph);

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Mod_Definition_Graph);

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Array_Definition_Graph);

      procedure Generate (The_Unit : Compilation_Unit_Graph) is
      begin
         if The_Unit /= null then
            Put_Line (The_File, "-- BC Boolean Compiler Ada Test Generator.");
            Put_Line
              (The_File,
               "-- Generated from '" &
               The_Name &
               SOURCE_EXTENSION &
               "', " &
               The_Date &
               ".");
            New_Line (The_File);

            Put_Line (The_File, "with BC_Package; use BC_Package;");
            Put_Line (The_File, "--");
            New_Line (The_File);

            Generate (The_Unit.The_Package);
         end if;
      end Generate;

      procedure Generate (The_Package : Package_Body_Graph) is
      begin
         if The_Package /= null then
            Put (The_File, "package");
            Generate (The_File, The_Package.The_Name);
            Put (The_File, " is");
            New_Line (The_File);
            New_Line (The_File);

            Generate (The_File, The_Package.The_Declarations, 4);

            Generate (The_Package.The_Procedure);
            New_Line (The_File);

            Put (The_File, "end");
            Generate (The_File, The_Package.The_Identifier);
            Put (The_File, ";");
            New_Line (The_File);
         end if;
      end Generate;

      procedure Generate (The_Procedure : Procedure_Body_Graph) is
      begin
         if The_Procedure /= null then
            Put (The_File, "    procedure");
            Generate (The_File, The_Procedure.The_Name);
            New_Line (The_File);

            Generate (The_File, The_Procedure.The_Parameters, 4);

            Put (The_File, ";");
            New_Line (The_File);
         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Declarations     : Declaration_Graph;
         With_The_Indentation : Natural)
      is
         The_Declaration : Declaration_Graph;
      begin
         if The_Declarations /= null then
            The_Declaration := The_Declarations;

            while The_Declaration /= null loop

               if The_Declaration.all in Type_Declaration_Node then
                  Generate
                    (The_File,
                     Type_Declaration_Graph (The_Declaration),
                     With_The_Indentation);

               elsif The_Declaration.all in Identifier_Declaration_Node then
                  Generate
                    (The_File,
                     Identifier_Declaration_Graph (The_Declaration),
                     With_The_Indentation);

               else
                  raise Critical_Error;
               end if;
               Put (The_File, ";");
               New_Line (The_File);

               The_Declaration := The_Declaration.The_Next;
            end loop;

            New_Line (The_File);
         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Declaration      : Type_Declaration_Graph;
         With_The_Indentation : Natural)
      is
      begin
         if The_Declaration /= null then
            Put (The_File, (1 .. With_The_Indentation => ' '));
            Put (The_File, "type");
            Generate (The_File, The_Declaration.The_Identifier);
            Put (The_File, " is");
            Generate (The_File, The_Declaration.The_Definition);

            Include
              (The_Map,
               Type_Identifier (The_Declaration.The_Identifier.The_Pointer.all)
                 .The_Type,
               The_Declaration.The_Identifier.The_String);

         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Declaration      : Identifier_Declaration_Graph;
         With_The_Indentation : Natural)
      is
      begin
         if The_Declaration /= null then
            Put (The_File, (1 .. With_The_Indentation => ' '));
            Generate (The_File, The_Declaration.The_Identifier);
            Put (The_File, " :");
            if The_Declaration.Is_Constant then
               Put (The_File, " constant");
            end if;
            Generate_Type (The_File, The_Declaration.The_Definition);
            if The_Declaration.The_Expression /= null then
               Put (The_File, " :=");
               Generate (The_File, The_Declaration.The_Expression);
            end if;
         end if;
      end Generate;

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Definition_Graph)
      is
      begin
         if The_Definition /= null then
            if The_Definition.all in Array_Definition_Node then
               Generate (The_File, Array_Definition_Graph (The_Definition));
            elsif The_Definition.all in Range_Definition_Node then
               Generate (The_File, Range_Definition_Graph (The_Definition));
            elsif The_Definition.all in Mod_Definition_Node then
               Generate (The_File, Mod_Definition_Graph (The_Definition));
            else
               raise Critical_Error;
            end if;
         end if;
      end Generate;

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Range_Definition_Graph)
      is
      begin
         if The_Definition /= null then
            Put (The_File, " new");

            Generate_Type (The_File, The_Definition.The_Identifier);

            if The_Definition.The_First /= null then
               Put (The_File, " range");
               Generate (The_File, The_Definition.The_First);
               Put (The_File, " ..");
               Generate (The_File, The_Definition.The_Last);
            end if;
         end if;
      end Generate;

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Mod_Definition_Graph)
      is
      begin
         if The_Definition /= null then
            Put (The_File, " mod");
            Generate (The_File, The_Definition.The_Expression);
         end if;
      end Generate;

      procedure Generate
        (The_File       : Ada.Text_IO.File_Type;
         The_Definition : Array_Definition_Graph)
      is
      begin
         if The_Definition /= null then
            Put (The_File, " array (");

            if The_Definition.The_Index /= null then
               Generate_Type (The_File, The_Definition.The_Index);
               Put (The_File, " range");
            end if;

            Generate (The_File, The_Definition.The_First);
            Put (The_File, " ..");
            Generate (The_File, The_Definition.The_Last);
            Put (The_File, " ) of");
            Generate_Type (The_File, The_Definition.The_Element);
         end if;
      end Generate;

   begin
      Create (The_File, Out_File, The_Name & SPECIFICATION_EXTENSION);

      Generate (The_Unit);

      Close (The_File);
   end Specification_File;

--      -- BC Boolean Compiler Ada Test Generator.
--      -- Generated from 'TEST.BC', 10/12/2007
--
--      package body Test is
--
--              procedure Run (The_Input : Value; The_Output : out Value) is
--              begin
--                      The_Output := The_Input+1;
--              end Run;
--
--      end Test;

   procedure Implementation_File
     (The_Name    : String;
      The_Package : Unbounded_String;
      The_Date    : String)
   is

      The_File : File_Type;

      procedure Generate (The_Unit : Compilation_Unit_Graph);
      procedure Generate (The_Package : Package_Body_Graph);

      procedure Generate
        (The_File        : Ada.Text_IO.File_Type;
         The_Procedure   : Procedure_Body_Graph;
         The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statements       : Statement_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : Assignment_Statement_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : Null_Statement_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : If_Statement_Graph;
         With_The_Indentation : Natural);

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : For_Statement_Graph;
         With_The_Indentation : Natural);

      procedure Generate (The_Unit : Compilation_Unit_Graph) is
      begin
         if The_Unit /= null then
            Put_Line (The_File, "-- BC Boolean Compiler Ada Test Generator.");
            Put_Line
              (The_File,
               "-- Generated from '" &
               The_Name &
               SOURCE_EXTENSION &
               "', " &
               The_Date &
               ".");
            New_Line (The_File);

            Generate (The_Unit.The_Package);
         end if;
      end Generate;

      procedure Generate (The_Package : Package_Body_Graph) is
      begin
         if The_Package /= null then
            Put (The_File, "package body");
            Generate (The_File, The_Package.The_Name);
            Put (The_File, " is");
            New_Line (The_File);
            New_Line (The_File);

            Generate (The_File, The_Package.The_Procedure, 4);
            New_Line (The_File);

            Put (The_File, "end");
            Generate (The_File, The_Package.The_Identifier);
            Put (The_File, ";");
            New_Line (The_File);
         end if;
      end Generate;

      procedure Generate
        (The_File        : Ada.Text_IO.File_Type;
         The_Procedure   : Procedure_Body_Graph;
         The_Indentation : Natural)
      is
      begin
         if The_Procedure /= null then
            Put (The_File, (1 .. The_Indentation => ' '));
            Put (The_File, "procedure");
            Generate (The_File, The_Procedure.The_Name);
            New_Line (The_File);
            Generate (The_File, The_Procedure.The_Parameters, The_Indentation);
            New_Line (The_File);
            Put (The_File, (1 .. The_Indentation => ' '));
            Put_Line (The_File, "is");
            Put (The_File, (1 .. The_Indentation => ' '));
            Put_Line (The_File, "begin");
            Generate
              (The_File,
               The_Procedure.The_Statements,
               The_Indentation + 2);
            Put (The_File, (1 .. The_Indentation => ' '));
            Put (The_File, "end");
            Generate (The_File, The_Procedure.The_Identifier);
            Put_Line (The_File, ";");
         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statements       : Statement_Graph;
         With_The_Indentation : Natural)
      is
         The_Statement : Statement_Graph := The_Statements;
      begin
         while The_Statement /= null loop

            if The_Statement.all in Assignment_Statement_Node then
               Generate
                 (The_File,
                  Assignment_Statement_Graph (The_Statement),
                  With_The_Indentation);

            elsif The_Statement.all in Null_Statement_Node then
               Generate
                 (The_File,
                  Null_Statement_Graph (The_Statement),
                  With_The_Indentation);

            elsif The_Statement.all in If_Statement_Node then
               Generate
                 (The_File,
                  If_Statement_Graph (The_Statement),
                  With_The_Indentation);

            elsif The_Statement.all in For_Statement_Node then
               Generate
                 (The_File,
                  For_Statement_Graph (The_Statement),
                  With_The_Indentation);

            else
               raise Critical_Error;
            end if;
            Put (The_File, ";");
            New_Line (The_File);

            The_Statement := The_Statement.The_Next;
         end loop;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : Assignment_Statement_Graph;
         With_The_Indentation : Natural)
      is
      begin
         if The_Statement /= null then
            Put (The_File, (1 .. With_The_Indentation => ' '));
            Generate (The_File, The_Statement.The_Variable);
            Put (The_File, " :=");
            Generate (The_File, The_Statement.The_Expression);
         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : Null_Statement_Graph;
         With_The_Indentation : Natural)
      is
      begin
         if The_Statement /= null then
            Put (The_File, (1 .. With_The_Indentation => ' '));
            Put (The_File, " null");
         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : If_Statement_Graph;
         With_The_Indentation : Natural)
      is
      begin
         if The_Statement /= null then
            Put (The_File, (1 .. With_The_Indentation => ' '));
            Put (The_File, "if");

            Generate (The_File, The_Statement.The_Expression);

            Put (The_File, " then");
            New_Line (The_File);

            Generate
              (The_File,
               The_Statement.The_Statements,
               With_The_Indentation + 2);

            if The_Statement.The_Alternates /= null then
               Put (The_File, (1 .. With_The_Indentation => ' '));
               Put (The_File, "else");
               New_Line (The_File);

               Generate
                 (The_File,
                  The_Statement.The_Alternates,
                  With_The_Indentation + 2);
            end if;

            Put (The_File, (1 .. With_The_Indentation => ' '));
            Put (The_File, "end if");
         end if;
      end Generate;

      procedure Generate
        (The_File             : Ada.Text_IO.File_Type;
         The_Statement        : For_Statement_Graph;
         With_The_Indentation : Natural)
      is
      begin
         if The_Statement /= null then
            Put (The_File, (1 .. With_The_Indentation => ' '));
            Put (The_File, "for");
            Generate (The_File, The_Statement.The_Index);
            Put (The_File, " in");

            if The_Statement.Is_Reverse then
               Put (The_File, " reverse");
            end if;

            if The_Statement.The_Definition /= null then
               Generate (The_File, The_Statement.The_Definition);
               Put (The_File, " range");
            end if;

            Generate (The_File, The_Statement.The_First);
            Put (The_File, " ..");
            Generate (The_File, The_Statement.The_Last);

            Put (The_File, " loop");
            New_Line (The_File);

            Generate
              (The_File,
               The_Statement.The_Statements,
               With_The_Indentation + 2);

            Put (The_File, (1 .. With_The_Indentation => ' '));
            Put (The_File, "end loop");
         end if;
      end Generate;

   begin
      Create (The_File, Out_File, The_Name & IMPLEMENTATION_EXTENSION);
      Generate (The_Unit);
      Close (The_File);
   end Implementation_File;

begin

   Specification_File (The_Name, The_Package, The_Date);
   Implementation_File (The_Name, The_Package, The_Date);

end Ada_Files;

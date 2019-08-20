-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with System_Package; use System_Package;
--

-- List a Boolean Compiler (BC) graph to standard output.

procedure Graph_Package.List (The_Unit : Compilation_Unit_Graph) is

   -- BC Language constructs.

   procedure List
     (The_Unit             : Compilation_Unit_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Package          : Package_Body_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Procedure        : Procedure_Body_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Parameters       : Parameter_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Declarations     : Declaration_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Declaration      : Type_Declaration_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Declaration      : Identifier_Declaration_Graph;
      With_The_Indentation : SYSNatural);

   procedure List (The_Definition : Definition_Graph);
   procedure List (The_Definition : Range_Definition_Graph);
   procedure List (The_Definition : Mod_Definition_Graph);
   procedure List (The_Definition : Array_Definition_Graph);

   procedure List
     (The_Statements       : Statement_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Statement        : Assignment_Statement_Graph;
      With_The_Indentation : SYSNatural);

   procedure List (The_Variable : Variable_Graph; With_Indent : Boolean);

   procedure List
     (The_Statement        : Null_Statement_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Statement        : If_Statement_Graph;
      With_The_Indentation : SYSNatural);

   procedure List
     (The_Statement        : For_Statement_Graph;
      With_The_Indentation : SYSNatural);

   procedure List (The_Expression : Expression_Graph; With_Indent : Boolean);
   procedure List
     (The_Expression : Unary_Expression_Graph;
      With_Indent    : Boolean);
   procedure List
     (The_Expression : Binary_Expression_Graph;
      With_Indent    : Boolean);
   procedure List
     (The_Expression : Variable_Expression_Graph;
      With_Indent    : Boolean);
   procedure List
     (The_Expression : Attribute_Expression_Graph;
      With_Indent    : Boolean);
   procedure List
     (The_Expression : Integer_Expression_Graph;
      With_Indent    : Boolean);

   procedure List
     (The_Identifier : Identifier_Symbol_Graph;
      With_Indent    : Boolean);

   procedure List
     (The_Unit             : Compilation_Unit_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Unit /= null then
         List (The_Unit.The_Package, With_The_Indentation);
      end if;
   end List;

   procedure List
     (The_Package          : Package_Body_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Package /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("package body");
         List (The_Package.The_Name, True);
         Put_Line (" is");
         New_Line;
         List (The_Package.The_Declarations, With_The_Indentation + 2);
         List (The_Package.The_Procedure, With_The_Indentation + 2);
         New_Line;
         Put ((1 .. With_The_Indentation => ' '));
         Put ("end");
         List (The_Package.The_Identifier, True);
         Put_Line (";");
      end if;
   end List;

   procedure List
     (The_Procedure        : Procedure_Body_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Procedure /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("procedure");
         List (The_Procedure.The_Name, True);
         New_Line;
         List (The_Procedure.The_Parameters, With_The_Indentation + 2);
         New_Line;
         Put ((1 .. With_The_Indentation => ' '));
         Put_Line ("is");
         Put ((1 .. With_The_Indentation => ' '));
         Put_Line ("begin");
         List (The_Procedure.The_Statements, With_The_Indentation + 2);
         Put ((1 .. With_The_Indentation => ' '));
         Put ("end");
         List (The_Procedure.The_Identifier, True);
         Put_Line (";");
      end if;
   end List;

   procedure List
     (The_Parameters       : Parameter_Graph;
      With_The_Indentation : SYSNatural)
   is
      The_Parameter : Parameter_Graph := The_Parameters;

      procedure List_Parameter
        (The_Parameter : Parameter_Graph;
         With_Indent   : Boolean)
      is
      begin
         List (The_Parameter.The_Identifier, With_Indent);
         Put (" :");
         if The_Parameter.Is_In then
            Put (" in");
         end if;
         if The_Parameter.Is_Out then
            Put (" out");
         end if;
         List (The_Parameter.The_Definition, True);
      end List_Parameter;

   begin
      if The_Parameter /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("(");
         List_Parameter (The_Parameter, False);
         The_Parameter := The_Parameter.The_Next;

         while The_Parameter /= null loop
            Put (";");
            New_Line;

            Put ((1 .. With_The_Indentation => ' '));
            List_Parameter (The_Parameter, True);
            The_Parameter := The_Parameter.The_Next;
         end loop;
         Put (")");
      end if;
   end List;

   procedure List
     (The_Declarations     : Declaration_Graph;
      With_The_Indentation : SYSNatural)
   is
      The_Declaration : Declaration_Graph;
   begin
      if The_Declarations /= null then
         The_Declaration := The_Declarations;

         while The_Declaration /= null loop

            if The_Declaration.all in Type_Declaration_Node then
               List
                 (Type_Declaration_Graph (The_Declaration),
                  With_The_Indentation);

            elsif The_Declaration.all in Identifier_Declaration_Node then
               List
                 (Identifier_Declaration_Graph (The_Declaration),
                  With_The_Indentation);

            else
               raise Critical_Error;
            end if;
            Put (";");
            New_Line;

            The_Declaration := The_Declaration.The_Next;
         end loop;

         New_Line;
      end if;
   end List;

   procedure List
     (The_Declaration      : Type_Declaration_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Declaration /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("type");
         List (The_Declaration.The_Identifier, True);
         Put (" is");
         List (The_Declaration.The_Definition);
      end if;
   end List;

   procedure List
     (The_Declaration      : Identifier_Declaration_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Declaration /= null then
         Put ((1 .. With_The_Indentation => ' '));
         List (The_Declaration.The_Identifier, False);
         Put (" :");
         if The_Declaration.Is_Constant then
            Put (" constant");
         end if;
         List (The_Declaration.The_Definition, True);
         if The_Declaration.The_Expression /= null then
            Put (" :=");
            List (The_Declaration.The_Expression, True);
         end if;
      end if;
   end List;

   procedure List (The_Definition : Definition_Graph) is
   begin
      if The_Definition /= null then
         if The_Definition.all in Array_Definition_Node then
            List (Array_Definition_Graph (The_Definition));

         elsif The_Definition.all in Range_Definition_Node then
            List (Range_Definition_Graph (The_Definition));

         elsif The_Definition.all in Mod_Definition_Node then
            List (Mod_Definition_Graph (The_Definition));

         else
            raise Critical_Error;
         end if;
      end if;
   end List;

   procedure List (The_Definition : Range_Definition_Graph) is
   begin
      if The_Definition /= null then
         Put (" new");

         List (The_Definition.The_Identifier, True);

         if The_Definition.The_First /= null then
            Put (" range");
            List (The_Definition.The_First, True);
            Put (" ..");
            List (The_Definition.The_Last, True);
         end if;
      end if;
   end List;

   procedure List (The_Definition : Mod_Definition_Graph) is
   begin
      if The_Definition /= null then
         Put (" mod");
         List (The_Definition.The_Expression, True);
      end if;
   end List;

   procedure List (The_Definition : Array_Definition_Graph) is
   begin
      if The_Definition /= null then
         Put (" array (");

         if The_Definition.The_Index /= null then
            List (The_Definition.The_Index, False);
            Put (" range");
            List (The_Definition.The_First, True);
         else
            List (The_Definition.The_First, False);
         end if;

         Put (" ..");
         List (The_Definition.The_Last, True);
         Put (") of");
         List (The_Definition.The_Element, True);
      end if;
   end List;

   procedure List
     (The_Statements       : Statement_Graph;
      With_The_Indentation : SYSNatural)
   is
      The_Statement : Statement_Graph := The_Statements;
   begin
      while The_Statement /= null loop

         if The_Statement.all in Assignment_Statement_Node then
            List
              (Assignment_Statement_Graph (The_Statement),
               With_The_Indentation);

         elsif The_Statement.all in Null_Statement_Node then
            List (Null_Statement_Graph (The_Statement), With_The_Indentation);

         elsif The_Statement.all in If_Statement_Node then
            List (If_Statement_Graph (The_Statement), With_The_Indentation);

         elsif The_Statement.all in For_Statement_Node then
            List (For_Statement_Graph (The_Statement), With_The_Indentation);

         else
            raise Critical_Error;
         end if;
         Put (";");
         New_Line;

         The_Statement := The_Statement.The_Next;
      end loop;
   end List;

   procedure List
     (The_Statement        : Assignment_Statement_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Statement /= null then
         Put ((1 .. With_The_Indentation => ' '));
         List (The_Statement.The_Variable, False);
         Put (" :=");
         List (The_Statement.The_Expression, True);
      end if;
   end List;

   procedure List (The_Variable : Variable_Graph; With_Indent : Boolean) is
   begin
      if The_Variable /= null then
         List (The_Variable.The_Identifier, With_Indent);

         if The_Variable.The_Expression /= null then
            Put (" (");
            List (The_Variable.The_Expression, False);
            Put (")");
         end if;
      end if;
   end List;

   procedure List
     (The_Statement        : Null_Statement_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Statement /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("null");
      end if;
   end List;

   procedure List
     (The_Statement        : If_Statement_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Statement /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("if");

         List (The_Statement.The_Expression, True);

         Put (" then");
         New_Line;

         List (The_Statement.The_Statements, With_The_Indentation + 2);

         if The_Statement.The_Alternates /= null then
            Put ((1 .. With_The_Indentation => ' '));
            Put ("else");
            New_Line;

            List (The_Statement.The_Alternates, With_The_Indentation + 2);
         end if;

         Put ((1 .. With_The_Indentation => ' '));
         Put ("end if");
      end if;
   end List;

   procedure List
     (The_Statement        : For_Statement_Graph;
      With_The_Indentation : SYSNatural)
   is
   begin
      if The_Statement /= null then
         Put ((1 .. With_The_Indentation => ' '));
         Put ("for");
         List (The_Statement.The_Index, True);
         Put (" in");

         if The_Statement.Is_Reverse then
            Put (" reverse");
         end if;

         if The_Statement.The_Definition /= null then
            List (The_Statement.The_Definition, True);
            Put (" range");
         end if;

         List (The_Statement.The_First, True);
         Put (" ..");
         List (The_Statement.The_Last, True);

         Put (" loop");
         New_Line;

         List (The_Statement.The_Statements, With_The_Indentation + 2);

         Put ((1 .. With_The_Indentation => ' '));
         Put ("end loop");
      end if;
   end List;

   procedure List (The_Expression : Expression_Graph; With_Indent : Boolean) is
   begin
      if The_Expression /= null then
         if The_Expression.all in Unary_Expression_Node then
            List (Unary_Expression_Graph (The_Expression), With_Indent);
         elsif The_Expression.all in Binary_Expression_Node then
            List (Binary_Expression_Graph (The_Expression), With_Indent);
         elsif The_Expression.all in Variable_Expression_Node then
            List (Variable_Expression_Graph (The_Expression), With_Indent);
         elsif The_Expression.all in Attribute_Expression_Node then
            List (Attribute_Expression_Graph (The_Expression), With_Indent);
         elsif The_Expression.all in Integer_Expression_Node then
            List (Integer_Expression_Graph (The_Expression), With_Indent);
         else
            raise Critical_Error;
         end if;
      end if;
   end List;

   procedure List
     (The_Expression : Unary_Expression_Graph;
      With_Indent    : Boolean)
   is
   begin
      if The_Expression /= null then
         if With_Indent then
            Put (" ");
         end if;
         Put ("(");

         case The_Expression.The_Operator is
            when Not_Symbol =>
               Put (" not");
            when Minus_Symbol =>
               Put (" -");
            when others =>
               raise Critical_Error;
         end case;

         List (The_Expression.The_Right, True);

         Put (")");
      end if;
   end List;

   procedure List
     (The_Expression : Binary_Expression_Graph;
      With_Indent    : Boolean)
   is
   begin
      if The_Expression /= null then
         if With_Indent then
            Put (" ");
         end if;
         Put ("(");
         List (The_Expression.The_Left, False);

         case The_Expression.The_Operator is
            when Equal_Symbol =>
               Put (" =");
            when Not_Equal_Symbol =>
               Put (" /=");
            when Less_Than_Symbol =>
               Put (" <");
            when Greater_Than_Symbol =>
               Put (" >");
            when Less_Than_Equal_Symbol =>
               Put (" <=");
            when Greater_Than_Equal_Symbol =>
               Put (" >=");
            when And_Symbol =>
               Put (" and");
            when Or_Symbol =>
               Put (" or");
            when Xor_Symbol =>
               Put (" xor");
            when Plus_Symbol =>
               Put (" +");
            when Minus_Symbol =>
               Put (" -");
            when Times_Symbol =>
               Put (" *");
            when Divide_Symbol =>
               Put (" /");
            when Mod_Symbol =>
               Put (" mod");
            when Rem_Symbol =>
               Put (" rem");
            when others =>
               raise Critical_Error;
         end case;

         List (The_Expression.The_Right, True);
         Put (")");
      end if;
   end List;

   procedure List
     (The_Expression : Variable_Expression_Graph;
      With_Indent    : Boolean)
   is
   begin
      if The_Expression /= null then
         if With_Indent then
            Put (" ");
         end if;
         List (The_Expression.The_Variable, False);
      end if;
   end List;

   procedure List
     (The_Expression : Attribute_Expression_Graph;
      With_Indent    : Boolean)
   is
   begin
      if The_Expression /= null then
         if With_Indent then
            Put (" ");
         end if;
         List (The_Expression.The_Identifier, False);
         Put ("'");
         Put (To_String (The_Expression.The_String));
      end if;
   end List;

   procedure List
     (The_Expression : Integer_Expression_Graph;
      With_Indent    : Boolean)
   is
   begin
      if The_Expression /= null
        and then The_Expression.The_String /= Null_Unbounded_String
      then
         if With_Indent then
            Put (" ");
         end if;
         Put (To_String (The_Expression.The_String));
      end if;
   end List;

   procedure List
     (The_Identifier : Identifier_Symbol_Graph;
      With_Indent    : Boolean)
   is
   begin
      if The_Identifier /= null
        and then The_Identifier.The_String /= Null_Unbounded_String
      then
         if With_Indent then
            Put (" ");
         end if;
         Put (To_String (The_Identifier.The_String));
      end if;
   end List;

begin
   Put_Line ("Graph");
   List (The_Unit, 4);
   New_Line;
end Graph_Package.List;

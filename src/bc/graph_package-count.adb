-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with System_Package;     use System_Package;
--

-- Count nodes of a Boolean Compiler (BC) graph.

function Graph_Package.Count
  (The_Unit : Compilation_Unit_Graph) return SYSNatural
is

   The_Count : SYSNatural := 0;

   -- BC Language constructs.

   procedure Count (The_Package : Package_Body_Graph);
   procedure Count (The_Procedure : Procedure_Body_Graph);
   procedure Count (The_Parameters : Parameter_Graph);
   procedure Count (The_Declarations : Declaration_Graph);
   procedure Count (The_Declaration : Type_Declaration_Graph);
   procedure Count (The_Declaration : Identifier_Declaration_Graph);
   procedure Count (The_Definition : Definition_Graph);
   procedure Count (The_Definition : Range_Definition_Graph);
   procedure Count (The_Definition : Mod_Definition_Graph);
   procedure Count (The_Definition : Array_Definition_Graph);
   procedure Count (The_Statements : Statement_Graph);
   procedure Count (The_Statement : Assignment_Statement_Graph);
   procedure Count (The_Variable : Variable_Graph);
   procedure Count (The_Statement : Null_Statement_Graph);
   procedure Count (The_Statement : If_Statement_Graph);
   procedure Count (The_Statement : For_Statement_Graph);
   procedure Count (The_Expression : Expression_Graph);
   procedure Count (The_Expression : Unary_Expression_Graph);
   procedure Count (The_Expression : Binary_Expression_Graph);
   procedure Count (The_Expression : Variable_Expression_Graph);
   procedure Count (The_Expression : Attribute_Expression_Graph);
   procedure Count (The_Expression : Integer_Expression_Graph);
   procedure Count (The_Expression : Operand_Expression_Graph);
   procedure Count (The_Identifier : Identifier_Symbol_Graph);

   procedure Count (The_Package : Package_Body_Graph) is
   begin
      if The_Package /= null then
         Count (The_Package.The_Name);
         Count (The_Package.The_Declarations);
         Count (The_Package.The_Procedure);
         Count (The_Package.The_Identifier);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Procedure : Procedure_Body_Graph) is
   begin
      if The_Procedure /= null then
         Count (The_Procedure.The_Name);
         Count (The_Procedure.The_Parameters);
         Count (The_Procedure.The_Statements);
         Count (The_Procedure.The_Identifier);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Parameters : Parameter_Graph) is
      The_Parameter : Parameter_Graph := The_Parameters;

   begin
      if The_Parameter /= null then
         Count (The_Parameter.The_Identifier);
         Count (The_Parameter.The_Definition);

         The_Count := The_Count + 1;

         The_Parameter := The_Parameter.The_Next;

         while The_Parameter /= null loop
            Count (The_Parameter.The_Identifier);
            Count (The_Parameter.The_Definition);

            The_Count := The_Count + 1;

            The_Parameter := The_Parameter.The_Next;
         end loop;
      end if;
   end Count;

   procedure Count (The_Declarations : Declaration_Graph) is
      The_Declaration : Declaration_Graph := The_Declarations;
   begin
      while The_Declaration /= null loop

         if The_Declaration.all in Type_Declaration_Node then
            Count (Type_Declaration_Graph (The_Declaration));

         elsif The_Declaration.all in Identifier_Declaration_Node then
            Count (Identifier_Declaration_Graph (The_Declaration));
         else
            raise Critical_Error;
         end if;

         The_Declaration := The_Declaration.The_Next;
      end loop;
   end Count;

   procedure Count (The_Declaration : Type_Declaration_Graph) is
   begin
      if The_Declaration /= null then
         Count (The_Declaration.The_Identifier);
         Count (The_Declaration.The_Definition);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Declaration : Identifier_Declaration_Graph) is
   begin
      if The_Declaration /= null then
         Count (The_Declaration.The_Identifier);
         Count (The_Declaration.The_Definition);
         Count (The_Declaration.The_Expression);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Definition : Definition_Graph) is
   begin
      if The_Definition /= null then

         if The_Definition.all in Array_Definition_Node'Class then
            Count (Array_Definition_Graph (The_Definition));

         elsif The_Definition.all in Range_Definition_Node'Class then
            Count (Range_Definition_Graph (The_Definition));

         elsif The_Definition.all in Mod_Definition_Node'Class then
            Count (Mod_Definition_Graph (The_Definition));
         else
            raise Critical_Error;
         end if;
      end if;
   end Count;

   procedure Count (The_Definition : Range_Definition_Graph) is
   begin
      if The_Definition /= null then
         Count (The_Definition.The_Identifier);
         Count (The_Definition.The_First);
         Count (The_Definition.The_Last);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Definition : Mod_Definition_Graph) is
   begin
      if The_Definition /= null then
         Count (The_Definition.The_Expression);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Definition : Array_Definition_Graph) is
   begin
      if The_Definition /= null then
         Count (The_Definition.The_Index);
         Count (The_Definition.The_First);
         Count (The_Definition.The_Last);
         Count (The_Definition.The_Element);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Statements : Statement_Graph) is
      The_Statement : Statement_Graph := The_Statements;
   begin
      while The_Statement /= null loop

         if The_Statement.all in Assignment_Statement_Node'Class then
            Count (Assignment_Statement_Graph (The_Statement));

         elsif The_Statement.all in Null_Statement_Node'Class then
            Count (Null_Statement_Graph (The_Statement));

         elsif The_Statement.all in If_Statement_Node'Class then
            Count (If_Statement_Graph (The_Statement));

         elsif The_Statement.all in For_Statement_Node'Class then
            Count (For_Statement_Graph (The_Statement));

         else
            raise Critical_Error;
         end if;

         The_Statement := The_Statement.The_Next;
      end loop;
   end Count;

   procedure Count (The_Statement : Assignment_Statement_Graph) is
   begin
      if The_Statement /= null then
         Count (The_Statement.The_Variable);
         Count (The_Statement.The_Expression);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Variable : Variable_Graph) is
   begin
      if The_Variable /= null then
         Count (The_Variable.The_Expression);
         Count (The_Variable.The_Identifier);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Statement : Null_Statement_Graph) is
   begin
      if The_Statement /= null then
         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Statement : If_Statement_Graph) is
   begin
      if The_Statement /= null then
         Count (The_Statement.The_Expression);
         Count (The_Statement.The_Statements);
         Count (The_Statement.The_Alternates);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Statement : For_Statement_Graph) is
   begin
      if The_Statement /= null then
         Count (The_Statement.The_Index);
         Count (The_Statement.The_Definition);
         Count (The_Statement.The_First);
         Count (The_Statement.The_Last);
         Count (The_Statement.The_Statements);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Expression : Expression_Graph) is
   begin
      if The_Expression /= null then
         if The_Expression.all in Unary_Expression_Node'Class then
            Count (Unary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Binary_Expression_Node'Class then
            Count (Binary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Variable_Expression_Node'Class then
            Count (Variable_Expression_Graph (The_Expression));
         elsif The_Expression.all in Attribute_Expression_Node'Class then
            Count (Attribute_Expression_Graph (The_Expression));
         elsif The_Expression.all in Integer_Expression_Node'Class then
            Count (Integer_Expression_Graph (The_Expression));
         elsif The_Expression.all in Operand_Expression_Node'Class then
            Count (Operand_Expression_Graph (The_Expression));
         else
            raise Critical_Error;
         end if;
      end if;
   end Count;

   procedure Count (The_Expression : Unary_Expression_Graph) is
   begin
      if The_Expression /= null then
         Count (The_Expression.The_Right);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Expression : Binary_Expression_Graph) is
   begin
      if The_Expression /= null then
         Count (The_Expression.The_Left);
         Count (The_Expression.The_Right);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Expression : Variable_Expression_Graph) is
   begin
      if The_Expression /= null then
         Count (The_Expression.The_Variable);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Expression : Attribute_Expression_Graph) is
   begin
      if The_Expression /= null then
         Count (The_Expression.The_Identifier);

         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Expression : Integer_Expression_Graph) is
   begin
      if The_Expression /= null then
         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Expression : Operand_Expression_Graph) is
   begin
      if The_Expression /= null then
         The_Count := The_Count + 1;
      end if;
   end Count;

   procedure Count (The_Identifier : Identifier_Symbol_Graph) is
   begin
      if The_Identifier /= null then
         The_Count := The_Count + 1;
      end if;
   end Count;

begin

   if The_Unit /= null then
      Count (The_Unit.The_Package);
      The_Count := The_Count + 1;
   end if;

   return The_Count;
end Graph_Package.Count;

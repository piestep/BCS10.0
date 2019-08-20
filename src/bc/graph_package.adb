-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Unchecked_Deallocation;
--
with Debug_Package; use Debug_Package;
with Type_Package;  use Type_Package;
--

package body Graph_Package is

   -- Identifier Symbol

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Identifier : in out Identifier_Symbol_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Identifier_Symbol_Node,
         Identifier_Symbol_Graph);
   begin
      if The_Identifier /= null then
         The_Identifier.The_String := Null_Unbounded_String;
         Free (The_Identifier);
      end if;
   end Dispose;

   -- Return ending position of symbol.

   function Position_Of
     (The_Identifier : access Identifier_Symbol_Node) return Source_Position
   is
   begin
      return The_Identifier.The_Position;
   end Position_Of;

   -- Variable

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Variable : in out Variable_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Variable_Node,
         Variable_Graph);
   begin
      if The_Variable /= null then
         Dispose (The_Variable.The_Identifier);
         Dispose (The_Variable.The_Expression);
         Dispose (The_Variable.The_Result);
         Free (The_Variable);
      end if;
   end Dispose;

   -- Return ending position of variable.

   function Position_Of
     (The_Variable : access Variable_Node) return Source_Position
   is
   begin
      if The_Variable.The_Expression = null then
         return The_Variable.The_Identifier.The_Position;
      else
         return Position_Of (The_Variable.The_Expression);
      end if;
   end Position_Of;

   -- Expression

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Expression_Graph) is
   begin
      if The_Expression /= null then
         if The_Expression.all in Integer_Expression_Node then
            Dispose (Integer_Expression_Graph (The_Expression));
         elsif The_Expression.all in Variable_Expression_Node then
            Dispose (Variable_Expression_Graph (The_Expression));
         elsif The_Expression.all in Attribute_Expression_Node then
            Dispose (Attribute_Expression_Graph (The_Expression));
         elsif The_Expression.all in Unary_Expression_Node then
            Dispose (Unary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Binary_Expression_Node then
            Dispose (Binary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Operand_Expression_Node then
            Dispose (Operand_Expression_Graph (The_Expression));
         else
            raise Critical_Error;
         end if;
      end if;
   end Dispose;

   -- Integer Expression

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Integer_Expression_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Integer_Expression_Node,
         Integer_Expression_Graph);
   begin
      if The_Expression /= null then
         The_Expression.The_String := Null_Unbounded_String;
         Dispose (The_Expression.The_Result);
         Free (The_Expression);
      end if;
   end Dispose;

   -- Return position of last operand.

   function Position_Of
     (The_Expression : access Integer_Expression_Node) return Source_Position
   is
   begin
      return The_Expression.The_Position;
   end Position_Of;

   -- Variable Expression

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Variable_Expression_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Variable_Expression_Node,
         Variable_Expression_Graph);
   begin
      if The_Expression /= null then
         Dispose (The_Expression.The_Variable);
         Dispose (The_Expression.The_Result);
         Free (The_Expression);
      end if;
   end Dispose;

   -- Return position of last operand.

   function Position_Of
     (The_Expression : access Variable_Expression_Node) return Source_Position
   is
   begin
      return Position_Of (The_Expression.The_Variable);
   end Position_Of;

   -- Attribute Expression

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Attribute_Expression_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Attribute_Expression_Node,
         Attribute_Expression_Graph);
   begin
      if The_Expression /= null then
         Dispose (The_Expression.The_Identifier);
         Dispose (The_Expression.The_Result);
         Free (The_Expression);
      end if;
   end Dispose;

   -- Return position of last operand.

   function Position_Of
     (The_Expression : access Attribute_Expression_Node) return Source_Position
   is
   begin
      return The_Expression.The_Position;
   end Position_Of;

   -- Unary Expression

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Unary_Expression_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Unary_Expression_Node,
         Unary_Expression_Graph);
   begin
      if The_Expression /= null then
         Dispose (The_Expression.The_Right);
         Dispose (The_Expression.The_Result);
         Free (The_Expression);
      end if;
   end Dispose;

   -- Return position of last operand.

   function Position_Of
     (The_Expression : access Unary_Expression_Node) return Source_Position
   is
   begin
      return Position_Of (The_Expression.The_Right);
   end Position_Of;

   -- Binary Expression

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Binary_Expression_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Binary_Expression_Node,
         Binary_Expression_Graph);
   begin
      if The_Expression /= null then
         Dispose (The_Expression.The_Left);
         Dispose (The_Expression.The_Right);
         Dispose (The_Expression.The_Result);
         Free (The_Expression);
      end if;
   end Dispose;

   -- Return position of last operand.

   function Position_Of
     (The_Expression : access Binary_Expression_Node) return Source_Position
   is
   begin
      return Position_Of (The_Expression.The_Right);
   end Position_Of;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Operand_Expression_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Operand_Expression_Node,
         Operand_Expression_Graph);
   begin
      Dispose (The_Expression.The_Result);
      Free (The_Expression);
   end Dispose;

   -- Return position of last operand.

   function Position_Of
     (The_Expression : access Operand_Expression_Node) return Source_Position
   is
   begin
      return The_Expression.The_Position;
   end Position_Of;

   -- Definition

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Definition_Graph) is
   begin
      if The_Definition /= null then
         if The_Definition.all in Array_Definition_Node then
            Dispose (Array_Definition_Graph (The_Definition));
         elsif The_Definition.all in Range_Definition_Node then
            Dispose (Range_Definition_Graph (The_Definition));
         elsif The_Definition.all in Mod_Definition_Node then
            Dispose (Mod_Definition_Graph (The_Definition));
         else
            raise Critical_Error;
         end if;
      end if;
   end Dispose;

   -- Array Definition

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Array_Definition_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Array_Definition_Node,
         Array_Definition_Graph);
   begin
      if The_Definition /= null then
         Dispose (The_Definition.The_Index);
         Dispose (The_Definition.The_First);
         Dispose (The_Definition.The_Last);
         Dispose (The_Definition.The_Element);
         Free (The_Definition);
      end if;
   end Dispose;

   -- Return ending position of definition.

   function Position_Of
     (The_Definition : access Array_Definition_Node) return Source_Position
   is
   begin
      return The_Definition.The_Element.The_Position;
   end Position_Of;

   -- Range Definition

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Range_Definition_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Range_Definition_Node,
         Range_Definition_Graph);
   begin
      if The_Definition /= null then
         Dispose (The_Definition.The_Identifier);
         Dispose (The_Definition.The_First);
         Dispose (The_Definition.The_Last);
         Free (The_Definition);
      end if;
   end Dispose;

   -- Return ending position of definition.

   function Position_Of
     (The_Definition : access Range_Definition_Node) return Source_Position
   is
   begin
      if The_Definition.The_Last = null then
         return The_Definition.The_Identifier.The_Position;
      else
         return Position_Of (The_Definition.The_Last);
      end if;
   end Position_Of;

   -- Mod Definition

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Mod_Definition_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Mod_Definition_Node,
         Mod_Definition_Graph);
   begin
      if The_Definition /= null then
         Dispose (The_Definition.The_Expression);
         Free (The_Definition);
      end if;
   end Dispose;

   -- Return ending position of definition.

   function Position_Of
     (The_Definition : access Mod_Definition_Node) return Source_Position
   is
   begin
      return Position_Of (The_Definition.The_Expression);
   end Position_Of;

   -- Declaration

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Declaration : in out Declaration_Graph) is
   begin
      if The_Declaration /= null then
         if The_Declaration.all in Identifier_Declaration_Node then
            Dispose (Identifier_Declaration_Graph (The_Declaration));
         elsif The_Declaration.all in Type_Declaration_Node then
            Dispose (Type_Declaration_Graph (The_Declaration));
         else
            raise Critical_Error;
         end if;
      end if;
   end Dispose;

   -- Identifier Declaration

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Declaration : in out Identifier_Declaration_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Identifier_Declaration_Node,
         Identifier_Declaration_Graph);
   begin
      if The_Declaration /= null then
         Dispose (The_Declaration.The_Identifier);
         Dispose (The_Declaration.The_Definition);
         Dispose (The_Declaration.The_Expression);
         Dispose (The_Declaration.The_Next);
         Free (The_Declaration);
      end if;
   end Dispose;

   -- Return ending position of declaration.

   function Position_Of
     (The_Declaration : access Identifier_Declaration_Node)
      return Source_Position
   is
   begin
      if The_Declaration.The_Expression = null then
         return The_Declaration.The_Definition.The_Position;
      else
         return Position_Of (The_Declaration.The_Expression);
      end if;
   end Position_Of;

   -- Type Declaration

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Declaration : in out Type_Declaration_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Type_Declaration_Node,
         Type_Declaration_Graph);
   begin
      if The_Declaration /= null then
         Dispose (The_Declaration.The_Identifier);
         Dispose (The_Declaration.The_Definition);
         Dispose (The_Declaration.The_Next);
         Free (The_Declaration);
      end if;
   end Dispose;

   -- Return ending position of declaration.

   function Position_Of
     (The_Declaration : access Type_Declaration_Node) return Source_Position
   is
   begin
      return Position_Of (The_Declaration.The_Definition);
   end Position_Of;

   -- Statement

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out Statement_Graph) is
   begin
      if The_Statement /= null then
         if The_Statement.all in Assignment_Statement_Node then
            Dispose (Assignment_Statement_Graph (The_Statement));
         elsif The_Statement.all in Null_Statement_Node then
            Dispose (Null_Statement_Graph (The_Statement));
         elsif The_Statement.all in If_Statement_Node then
            Dispose (If_Statement_Graph (The_Statement));
         elsif The_Statement.all in For_Statement_Node then
            Dispose (For_Statement_Graph (The_Statement));
         else
            raise Critical_Error;
         end if;
      end if;
   end Dispose;

   -- Assignment Statement

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out Assignment_Statement_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Assignment_Statement_Node,
         Assignment_Statement_Graph);
   begin
      if The_Statement /= null then
         Dispose (The_Statement.The_Variable);
         Dispose (The_Statement.The_Expression);
         Dispose (The_Statement.The_Next);
         Free (The_Statement);
      end if;
   end Dispose;

   -- Return ending position of statement.

   function Position_Of
     (The_Statement : access Assignment_Statement_Node) return Source_Position
   is
   begin
      return Position_Of (The_Statement.The_Expression);
   end Position_Of;

   -- Null Statement

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out Null_Statement_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Null_Statement_Node,
         Null_Statement_Graph);
   begin
      if The_Statement /= null then
         Dispose (The_Statement.The_Next);
         Free (The_Statement);
      end if;
   end Dispose;

   -- If Statement

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out If_Statement_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (If_Statement_Node,
         If_Statement_Graph);
   begin
      if The_Statement /= null then
         Dispose (The_Statement.The_Expression);
         Dispose (The_Statement.The_Statements);
         Dispose (The_Statement.The_Alternates);
         Dispose (The_Statement.The_Next);
         Free (The_Statement);
      end if;
   end Dispose;

   -- Return ending position of statement.

   function Position_Of
     (The_Statement : access If_Statement_Node) return Source_Position
   is
   begin
      return Position_Of (The_Statement.The_Expression);
   end Position_Of;

   -- For Statement

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out For_Statement_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (For_Statement_Node,
         For_Statement_Graph);
   begin
      if The_Statement /= null then
         Dispose (The_Statement.The_Index);
         Dispose (The_Statement.The_Definition);
         Dispose (The_Statement.The_First);
         Dispose (The_Statement.The_Last);
         Dispose (The_Statement.The_Statements);
         Dispose (The_Statement.The_Next);
         Free (The_Statement);
      end if;
   end Dispose;

   -- Return ending position of statement.

   function Position_Of
     (The_Statement : access For_Statement_Node) return Source_Position
   is
   begin
      return Position_Of (The_Statement.The_Last);
   end Position_Of;

   -- Parameter

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Parameter : in out Parameter_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parameter_Node,
         Parameter_Graph);
   begin
      if The_Parameter /= null then
         Dispose (The_Parameter.The_Identifier);
         Dispose (The_Parameter.The_Definition);
         Dispose (The_Parameter.The_Next);
         Free (The_Parameter);
      end if;
   end Dispose;

   -- Procedure Body

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Procedure : in out Procedure_Body_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Procedure_Body_Node,
         Procedure_Body_Graph);
   begin
      if The_Procedure /= null then
         Dispose (The_Procedure.The_Name);
         Dispose (The_Procedure.The_Parameters);
         Dispose (The_Procedure.The_Statements);
         Dispose (The_Procedure.The_Identifier);
         Free (The_Procedure);
      end if;
   end Dispose;

   -- Package Body

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Package : in out Package_Body_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Package_Body_Node,
         Package_Body_Graph);
   begin
      if The_Package /= null then
         Dispose (The_Package.The_Name);
         Dispose (The_Package.The_Declarations);
         Dispose (The_Package.The_Procedure);
         Dispose (The_Package.The_Identifier);
         Free (The_Package);
      end if;
   end Dispose;

   -- Compilation Unit

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Unit : in out Compilation_Unit_Graph) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Compilation_Unit_Node,
         Compilation_Unit_Graph);
   begin
      if The_Unit /= null then
         Dispose (The_Unit.The_Package);
         Free (The_Unit);
      end if;
   end Dispose;

begin

   Debug (Debug_Initialization, "Graph_Package");

end Graph_Package;

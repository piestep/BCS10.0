-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Pool_Package; use Pool_Package;
--
with BC_Package;         use BC_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Identifier_Package; use Identifier_Package;
with Operand_Package;    use Operand_Package;
--

-- A package to define the Boolean Compiler (BC) program source graph.

-- Boolean Compiler (BC) Syntax.
--
-- Compilation_Unit => Package_Body.
--
-- Package_Body => 'package' 'body' <identifier> 'is'
--                               Declarations
--                               Procedure_Body
--                            'end' <identifier> ';'.
--
-- Declarations => { Declaration ';' }.
--
-- Declaration => Type_Declaration.
--
-- Declaration => Identifier_Declaration.
--
-- Type_Declaration => 'type' <identifier> 'is'
--                              ( Array_Definition |
--                                Range_Definition |
--                                Mod_Definition ).
--
-- Array_Defintion => 'array' '(' <identifier> range
--                              Expression .. Expression ')' 'of' <identifier>.
--
-- Range_Definition => 'new' <identifier> [ 'range' Expression '..' Expression
-- ].
--
-- Mod_Definition => 'mod' Expression.
--
-- Identifier_Declaration => <identifier> ':' [ 'constant' ] <identifier>
--                              [ ':=' Expression ].
--
-- Procedure_Body => 'procedure' <identifier> Parameters 'is'
--                           'begin'
--                               Statements
--                           'end' <identifier> ';'.
-- Parameters => '(' Parameter { ';' Parameter } ')'.
--
-- Parameter => <identifier> ':' ( 'in' | 'out' | 'in' 'out' ) <identifier>.
--
-- Statements => Statement ';' { Statement ';' }.
--
-- Statement => Null_Statement.
--
-- Statement => Assignment_Statement
--
-- Statement => If_Statement
--
-- Statement => For_Statement
--
-- Assignment_Statement => Variable ':=' Expression.
--
-- Variable => <variable identifier> [ '(' Expression ')' ].
--
-- Null_Statement => 'null'
--
-- If_Statement => 'if' Expresion 'then' Statements
--                           [ 'else' Statements ]
--                           'end' 'if'.
--
-- For_Statement => 'for' <identifier> 'in' [ 'reverse' ]
--                           <identifier> range Expression .. Expression 'loop'
--                              Statements
--                           'end' 'loop'.
--
-- Expression => Simple_Expression
--                           [ <relational_operators> Simple_Expression ].
--
-- Simple_Expression => Term_Expression
--                           [ <mulplication_operators> Term_Expression ].
--
-- Term_Expression => ( '+' | '-' ) Factor
--                           [ <addition_operators> Factor ].
--
-- Factor => <identifier> [ '(' Expression ')' ].
--
-- Factor => <identifier> ''' <identifier>.
--
-- Factor => <integer>.
--
-- Factor => 'not' Factor.
--
-- Factor => '(' Expression ')'.


package Graph_Package is

   The_Pool : Storage_Pool;

   type Graph_Node is abstract tagged null record;
   type Graph_Pointer is access all Graph_Node'Class;
   for Graph_Pointer'Storage_Pool use The_Pool;

   -- Identiier Symbol

   type Identifier_Symbol_Node is new Graph_Node with record
      The_String   : Unbounded_String   := Null_Unbounded_String;
      The_Position : Source_Position;
      The_Pointer  : Identifier_Pointer := null;
   end record;

   type Identifier_Symbol_Graph is access Identifier_Symbol_Node;
   for Identifier_Symbol_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Identifier : in out Identifier_Symbol_Graph);

   -- Return ending position of symbol.

   function Position_Of
     (The_Identifier : access Identifier_Symbol_Node) return Source_Position;

   -- Expressions

   type Expression_Node;
   type Expression_Graph is access all Expression_Node'Class;
   for Expression_Graph'Storage_Pool use The_Pool;

   type Expression_Node is abstract new Graph_Node with record
      The_Result : Operand_Pointer := null;
   end record;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Expression_Graph);

   -- Return ending position of last operand.

   function Position_Of
     (The_Expression : access Expression_Node)
      return Source_Position is abstract;

   -- Variable

   type Variable_Node is new Graph_Node with record
      The_Identifier : Identifier_Symbol_Graph := null;
      The_Expression : Expression_Graph        := null;
      The_Result     : Operand_Pointer         := null;
   end record;

   type Variable_Graph is access Variable_Node;
   for Variable_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Variable : in out Variable_Graph);

   -- Return ending position of variable.

   function Position_Of
     (The_Variable : access Variable_Node) return Source_Position;

   -- Integer Expression

   type Integer_Expression_Node is new Expression_Node with record
      The_String   : Unbounded_String := Null_Unbounded_String;
      The_Position : Source_Position;
   end record;

   type Integer_Expression_Graph is access all Integer_Expression_Node;
   for Integer_Expression_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Integer_Expression_Graph);

   -- Return position of last operand.

   overriding function Position_Of
     (The_Expression : access Integer_Expression_Node) return Source_Position;

   -- Variable Expression

   type Variable_Expression_Node is new Expression_Node with record
      The_Variable : Variable_Graph := null;
   end record;

   type Variable_Expression_Graph is access all Variable_Expression_Node;
   for Variable_Expression_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Variable_Expression_Graph);

   -- Return position of last operand.

   overriding function Position_Of
     (The_Expression : access Variable_Expression_Node) return Source_Position;

   -- Attribute Expression

   type Attribute_Expression_Node is new Expression_Node with record
      The_Identifier : Identifier_Symbol_Graph := null;
      The_String   : Unbounded_String   := Null_Unbounded_String;
      The_Position : Source_Position;
   end record;

   type Attribute_Expression_Graph is access all Attribute_Expression_Node;
   for Attribute_Expression_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Attribute_Expression_Graph);

   -- Return position of last operand.

   overriding function Position_Of
     (The_Expression : access Attribute_Expression_Node) return Source_Position;

   -- Unary Expression

   type Unary_Expression_Node is new Expression_Node with record
      The_Operator : Symbol;
      The_Right    : Expression_Graph := null;
   end record;

   type Unary_Expression_Graph is access all Unary_Expression_Node;
   for Unary_Expression_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Unary_Expression_Graph);

   -- Return position of last operand.

   overriding function Position_Of
     (The_Expression : access Unary_Expression_Node) return Source_Position;

   -- Binary Expression

   type Binary_Expression_Node is new Expression_Node with record
      The_Operator : Symbol;
      The_Left     : Expression_Graph := null;
      The_Right    : Expression_Graph := null;
   end record;

   type Binary_Expression_Graph is access all Binary_Expression_Node;
   for Binary_Expression_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Binary_Expression_Graph);

   -- Return position of last operand.

   overriding function Position_Of
     (The_Expression : access Binary_Expression_Node) return Source_Position;

   -- Operand Expression

   type Operand_Expression_Node is new Expression_Node with record
      The_Position : Source_Position;
   end record;

   type Operand_Expression_Graph is access all Operand_Expression_Node;
   for Operand_Expression_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Expression : in out Operand_Expression_Graph);

   -- Return position of last operand.

   overriding function Position_Of
     (The_Expression : access Operand_Expression_Node) return Source_Position;

   -- Definitions

   type Definition_Node;
   type Definition_Graph is access all Definition_Node'Class;
   for Definition_Graph'Storage_Pool use The_Pool;

   type Definition_Node is abstract new Graph_Node with null record;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Definition_Graph);

   -- Return ending position of definition.

   function Position_Of
     (The_Definition : access Definition_Node)
      return Source_Position is abstract;

   -- Array Definition

   type Array_Definition_Node is new Definition_Node with record
      The_Index   : Identifier_Symbol_Graph := null;
      The_First   : Expression_Graph        := null;
      The_Last    : Expression_Graph        := null;
      The_Element : Identifier_Symbol_Graph := null;
   end record;

   type Array_Definition_Graph is access all Array_Definition_Node;
   for Array_Definition_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Array_Definition_Graph);

   -- Return ending position of definition.

   overriding function Position_Of
     (The_Definition : access Array_Definition_Node) return Source_Position;

   -- Range Definition

   type Range_Definition_Node is new Definition_Node with record
      The_Identifier : Identifier_Symbol_Graph := null;
      The_First      : Expression_Graph        := null;
      The_Last       : Expression_Graph        := null;
   end record;

   type Range_Definition_Graph is access all Range_Definition_Node;
   for Range_Definition_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Range_Definition_Graph);

   -- Return ending position of definition.

   overriding function Position_Of
     (The_Definition : access Range_Definition_Node) return Source_Position;

   -- Mod Definition

   type Mod_Definition_Node is new Definition_Node with record
      The_Expression : Expression_Graph := null;
   end record;

   type Mod_Definition_Graph is access all Mod_Definition_Node;
   for Mod_Definition_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Definition : in out Mod_Definition_Graph);

   -- Return ending position of definition.

   overriding function Position_Of
     (The_Definition : access Mod_Definition_Node) return Source_Position;

   -- Declarations

   type Declaration_Node;
   type Declaration_Graph is access all Declaration_Node'Class;
   for Declaration_Graph'Storage_Pool use The_Pool;

   type Declaration_Node is abstract new Graph_Node with record
      The_Next : Declaration_Graph := null;
   end record;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Declaration : in out Declaration_Graph);

   -- Return ending position of declaration.

   function Position_Of
     (The_Declaration : access Declaration_Node)
      return Source_Position is abstract;

   -- Identifier Declaration

   type Identifier_Declaration_Node is new Declaration_Node with record
      The_Identifier : Identifier_Symbol_Graph := null;
      Is_Constant    : Boolean                 := False;
      The_Definition : Identifier_Symbol_Graph := null;
      The_Expression : Expression_Graph        := null;
   end record;

   type Identifier_Declaration_Graph is access all Identifier_Declaration_Node;
   for Identifier_Declaration_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Declaration : in out Identifier_Declaration_Graph);

   -- Return ending position of declaration.

   overriding function Position_Of
     (The_Declaration : access Identifier_Declaration_Node)
      return Source_Position;

   -- Type Declaration

   type Type_Declaration_Node is new Declaration_Node with record
      The_Identifier : Identifier_Symbol_Graph := null;
      The_Definition : Definition_Graph        := null;
   end record;

   type Type_Declaration_Graph is access all Type_Declaration_Node;
   for Type_Declaration_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Declaration : in out Type_Declaration_Graph);

   -- Return position of statement.

   overriding function Position_Of
     (The_Declaration : access Type_Declaration_Node) return Source_Position;

   -- Statements

   type Statement_Node;
   type Statement_Graph is access all Statement_Node'Class;
   for Statement_Graph'Storage_Pool use The_Pool;

   type Statement_Node is abstract new Graph_Node with record
      The_Next : Statement_Graph := null;
   end record;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out Statement_Graph);

   -- Assignment Statement

   type Assignment_Statement_Node is new Statement_Node with record
      The_Variable   : Variable_Graph   := null;
      The_Expression : Expression_Graph := null;
   end record;

   type Assignment_Statement_Graph is access all Assignment_Statement_Node;
   for Assignment_Statement_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out Assignment_Statement_Graph);

   -- Return ending position of statement.

   function Position_Of
     (The_Statement : access Assignment_Statement_Node) return Source_Position;

   -- Null Statement

   type Null_Statement_Node is new Statement_Node with null record;
   type Null_Statement_Graph is access all Null_Statement_Node;
   for Null_Statement_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out Null_Statement_Graph);

   -- If Statement

   type If_Statement_Node is new Statement_Node with record
      The_Expression : Expression_Graph := null;
      The_Statements : Statement_Graph  := null;
      The_Alternates : Statement_Graph  := null;
   end record;

   type If_Statement_Graph is access all If_Statement_Node;
   for If_Statement_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out If_Statement_Graph);

   -- Return ending position of statement.

   function Position_Of
     (The_Statement : access If_Statement_Node) return Source_Position;

   -- For Statement

   type For_Statement_Node is new Statement_Node with record
      The_Index      : Identifier_Symbol_Graph := null;
      The_Definition : Identifier_Symbol_Graph := null;
      Is_Reverse     : Boolean                 := False;
      The_First      : Expression_Graph        := null;
      The_Last       : Expression_Graph        := null;
      The_Statements : Statement_Graph         := null;
   end record;

   type For_Statement_Graph is access all For_Statement_Node;
   for For_Statement_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Statement : in out For_Statement_Graph);

   -- Return ending position of statement.

   function Position_Of
     (The_Statement : access For_Statement_Node) return Source_Position;

   -- Parameter

   type Parameter_Node;
   type Parameter_Graph is access Parameter_Node;
   for Parameter_Graph'Storage_Pool use The_Pool;

   type Parameter_Node is new Graph_Node with record
      The_Identifier : Identifier_Symbol_Graph := null;
      Is_In          : Boolean                 := False;
      Is_Out         : Boolean                 := False;
      The_Definition : Identifier_Symbol_Graph := null;
      The_Next       : Parameter_Graph         := null;
   end record;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Parameter : in out Parameter_Graph);

   -- Procedure Body

   type Procedure_Body_Node is new Graph_Node with record
      The_Name       : Identifier_Symbol_Graph := null;
      The_Parameters : Parameter_Graph         := null;
      The_Statements : Statement_Graph         := null;
      The_Identifier : Identifier_Symbol_Graph := null;
   end record;

   type Procedure_Body_Graph is access Procedure_Body_Node;
   for Procedure_Body_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Procedure : in out Procedure_Body_Graph);

   -- Package Body

   type Package_Body_Node is new Graph_Node with record
      The_Name         : Identifier_Symbol_Graph := null;
      The_Declarations : Declaration_Graph       := null;
      The_Procedure    : Procedure_Body_Graph    := null;
      The_Identifier   : Identifier_Symbol_Graph := null;
   end record;

   type Package_Body_Graph is access Package_Body_Node;
   for Package_Body_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Package : in out Package_Body_Graph);

   -- Compilation Unit

   type Compilation_Unit_Node is new Graph_Node with record
      The_Package : Package_Body_Graph := null;
   end record;

   type Compilation_Unit_Graph is access Compilation_Unit_Node;
   for Compilation_Unit_Graph'Storage_Pool use The_Pool;

   -- Dispose graph node and all subgraph nodes.

   procedure Dispose (The_Unit : in out Compilation_Unit_Graph);

end Graph_Package;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Debug_Package;      use Debug_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
with Error_Package;      use Error_Package;
--

package body Syntax_Package is

   -- Define program symbols and routines.

   type Set_Of_Symbols is array (Symbol'Range) of Boolean;

   type Array_Of_Symbols is array (Natural range <>) of Symbol;

   function Set_Of (The_Symbol : Symbol) return Set_Of_Symbols is
      The_Set : Set_Of_Symbols := (Symbol'Range => False);
   begin
      The_Set (The_Symbol) := True;
      return The_Set;
   end Set_Of;

   function Set_Of (The_Symbols : Array_Of_Symbols) return Set_Of_Symbols is
      The_Set : Set_Of_Symbols := (Symbol'Range => False);
   begin
      for The_Index in The_Symbols'Range loop
         The_Set (The_Symbols (The_Index)) := True;
      end loop;
      return The_Set;
   end Set_Of;

   -- Symbol starters of language constructs.

   Declaration_Starters : Set_Of_Symbols :=
     Set_Of ((Type_Symbol, Identifier_Symbol));

   Statement_Starters : Set_Of_Symbols :=
     Set_Of ((Identifier_Symbol, Null_Symbol, If_Symbol, For_Symbol));

   Expression_Starters : Set_Of_Symbols :=
     Set_Of
       ((Plus_Symbol,
         Minus_Symbol,
         Identifier_Symbol,
         Integer_Symbol,
        Left_Paren_Symbol));

   Factor_Starters : Set_Of_Symbols :=
     Set_Of
       ((Left_Paren_Symbol, Not_Symbol, Identifier_Symbol, Integer_Symbol));

   Addition_Operators : Set_Of_Symbols :=
     Set_Of ((Or_Symbol, Xor_Symbol, Plus_Symbol, Minus_Symbol));

   Mulplication_Operators : Set_Of_Symbols :=
     Set_Of ((And_Symbol, Times_Symbol, Divide_Symbol, Rem_Symbol, Mod_Symbol));

   Relational_Operators : Set_Of_Symbols :=
     Set_Of
       ((Equal_Symbol,
         Not_Equal_Symbol,
         Less_Than_Symbol,
         Less_Than_Equal_Symbol,
         Greater_Than_Symbol,
         Greater_Than_Equal_Symbol));

   -- Handle a syntax error.

   procedure Syntax_Error (The_Message : String) is
   begin
      Error_Package.Source_Error (Source_Package.The_Position, The_Message);
   end Syntax_Error;

   -- Check syntax routines.

   procedure Skip_To (The_Symbols : Set_Of_Symbols) is
   begin
      while not The_Symbols (The_Symbol) loop
         Next_Symbol;
      end loop;
   end Skip_To;

   function Check
     (The_Starters  : Set_Of_Symbols;
      The_Followers : Set_Of_Symbols) return Boolean
   is
   begin
      if The_Starters (The_Symbol) then
         return True;
      else
         Syntax_Error (Symbol'Image (The_Symbol) & " not a starting symbol.");
      end if;

      Skip_To (The_Starters or The_Followers);

      if The_Starters (The_Symbol) then
         return True;
      else
         return False;
      end if;
   end Check;

   procedure Check (The_Followers : Set_Of_Symbols) is
   begin
      if not The_Followers (The_Symbol) then
         Syntax_Error (Symbol'Image (The_Symbol) & " not a following symbol.");
         Skip_To (The_Followers);
      end if;
   end Check;

   procedure Accept_Symbol (The_Symbol : Symbol) is
   begin
      if Scanner_Package.The_Symbol = The_Symbol then
         Next_Symbol;
      else
         Syntax_Error ("Expected " & Symbol'Image (The_Symbol));
      end if;
   end Accept_Symbol;

   -- BC Language constructs.

   procedure Compilation_Unit (The_Unit : out Compilation_Unit_Graph);
   procedure Package_Body (The_Package : out Package_Body_Graph);
   procedure Procedure_Body
     (The_Followers :     Set_Of_Symbols;
      The_Procedure : out Procedure_Body_Graph);
   procedure Parameters
     (The_Followers  :     Set_Of_Symbols;
      The_Parameters : out Parameter_Graph);
   procedure Declarations
     (The_Followers    :     Set_Of_Symbols;
      The_Declarations : out Declaration_Graph);
   procedure Type_Declaration
     (The_Followers   :     Set_Of_Symbols;
      The_Declaration : out Declaration_Graph);
   procedure Identifier_Declaration
     (The_Followers   :     Set_Of_Symbols;
      The_Declaration : out Declaration_Graph);
   procedure Array_Definition
     (The_Followers  :     Set_Of_Symbols;
      The_Definition : out Definition_Graph);
   procedure Range_Definition
     (The_Followers  :     Set_Of_Symbols;
      The_Definition : out Definition_Graph);
   procedure Mod_Definition
     (The_Followers  :     Set_Of_Symbols;
      The_Definition : out Definition_Graph);
   procedure Statements
     (The_Followers  :     Set_Of_Symbols;
      The_Statements : out Statement_Graph);
   procedure Assignment_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph);
   procedure Variable
     (The_Followers  :     Set_Of_Symbols;
      The_Identifier :     Identifier_Symbol_Graph;
      The_Variable   : out Variable_Graph);
   procedure Null_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph);
   procedure If_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph);
   procedure For_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph);
   procedure Expression
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph);
   procedure Simple_Expression
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph);
   procedure Term_Expression
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph);
   procedure Factor
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph);

   procedure Compilation_Unit (The_Unit : out Compilation_Unit_Graph) is
   begin
      Debug (Syntax_Debug, "begin Compilation_Unit");
      The_Unit := new Compilation_Unit_Node;
      Package_Body (The_Unit.The_Package);
      Debug (Syntax_Debug, "end Compilation_Unit");
   end Compilation_Unit;

   procedure Package_Body (The_Package : out Package_Body_Graph) is
   begin
      Debug (Syntax_Debug, "begin Package_Body");
      The_Package := new Package_Body_Node;

      Accept_Symbol (Package_Symbol);
      Accept_Symbol (Body_Symbol);

      if The_Symbol = Identifier_Symbol then
         The_Package.The_Name :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      Accept_Symbol (Is_Symbol);

      Declarations (Set_Of (Procedure_Symbol), The_Package.The_Declarations);
      Procedure_Body (Set_Of (End_Symbol), The_Package.The_Procedure);
      Accept_Symbol (End_Symbol);

      if The_Symbol = Identifier_Symbol then
         The_Package.The_Identifier :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      if The_Symbol = Semicolon_Symbol then
         begin
            Next_Symbol;
            Syntax_Error ("Expected End Of File.");
         exception
            when End_Error =>
               null;
         end;
      end if;

      Debug (Syntax_Debug, "end Package_Body");
   end Package_Body;

   procedure Procedure_Body
     (The_Followers :     Set_Of_Symbols;
      The_Procedure : out Procedure_Body_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Procedure_Body");
      The_Procedure := new Procedure_Body_Node;

      Accept_Symbol (Procedure_Symbol);

      if The_Symbol = Identifier_Symbol then
         The_Procedure.The_Name :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      if The_Symbol = Left_Paren_Symbol then
         Accept_Symbol (Left_Paren_Symbol);
         Parameters
           (Set_Of ((Right_Paren_Symbol, Is_Symbol)) or The_Followers,
            The_Procedure.The_Parameters);
         Accept_Symbol (Right_Paren_Symbol);
      end if;

      Accept_Symbol (Is_Symbol);
      Accept_Symbol (Begin_Symbol);

      Statements
        (Set_Of (End_Symbol) or The_Followers,
         The_Procedure.The_Statements);
      Accept_Symbol (End_Symbol);

      if The_Symbol = Identifier_Symbol then
         The_Procedure.The_Identifier :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);
      Accept_Symbol (Semicolon_Symbol);

      Debug (Syntax_Debug, "end Procedure_Body");
   end Procedure_Body;

   procedure Parameters
     (The_Followers  :     Set_Of_Symbols;
      The_Parameters : out Parameter_Graph)
   is

      The_Parameter : Parameter_Graph;
      The_Last      : Parameter_Graph := null;

      procedure Parameter
        (The_Followers :     Set_Of_Symbols;
         The_Parameter : out Parameter_Graph)
      is
      begin
         Debug (Syntax_Debug, "begin Parameter");
         The_Parameter := new Parameter_Node;

         if The_Symbol = Identifier_Symbol then
            The_Parameter.The_Identifier :=
              new Identifier_Symbol_Node'
                (The_String   => Scanner_Package.The_String,
                 The_Position => Scanner_Package.The_Position,
                 The_Pointer  => null);
         end if;
         Accept_Symbol (Identifier_Symbol);
         Accept_Symbol (Colon_Symbol);

         if The_Symbol = Identifier_Symbol then
            The_Parameter.Is_In := True;
         else
            if The_Symbol = In_Symbol then
               Accept_Symbol (In_Symbol);
               The_Parameter.Is_In := True;
            end if;

            if The_Symbol = Out_Symbol then
               Accept_Symbol (Out_Symbol);
               The_Parameter.Is_Out := True;
            end if;
         end if;

         if The_Symbol = Identifier_Symbol then
            The_Parameter.The_Definition :=
              new Identifier_Symbol_Node'
                (The_String   => Scanner_Package.The_String,
                 The_Position => Scanner_Package.The_Position,
                 The_Pointer  => null);
         end if;
         Accept_Symbol (Identifier_Symbol);

         Debug (Syntax_Debug, "end Parameter");
      end Parameter;

   begin
      Debug (Syntax_Debug, "begin Parameters");
      The_Parameters := null;

      Parameter (Set_Of (Semicolon_Symbol) or The_Followers, The_Parameter);

      The_Parameters := The_Parameter;
      The_Last       := The_Parameter;

      Check (Set_Of (Semicolon_Symbol) or The_Followers);

      while The_Symbol = Semicolon_Symbol loop
         Accept_Symbol (Semicolon_Symbol);

         Parameter (Set_Of (Semicolon_Symbol) or The_Followers, The_Parameter);

         The_Last.The_Next := The_Parameter;
         The_Last          := The_Parameter;

         Check (Set_Of (Semicolon_Symbol) or The_Followers);
      end loop;

      Debug (Syntax_Debug, "end Parameters");
   end Parameters;

   procedure Declarations
     (The_Followers    :     Set_Of_Symbols;
      The_Declarations : out Declaration_Graph)
   is

      The_Declaration : Declaration_Graph;
      The_Last        : Declaration_Graph := null;

      procedure Declaration
        (The_Followers   :     Set_Of_Symbols;
         The_Declaration : out Declaration_Graph)
      is
      begin
         Debug (Syntax_Debug, "begin Declaration");
         The_Declaration := null;

         case The_Symbol is
            when Type_Symbol =>
               Type_Declaration (The_Followers, The_Declaration);
            when Identifier_Symbol =>
               Identifier_Declaration
                 (Set_Of (Type_Symbol) or The_Followers,
                  The_Declaration);
            when others =>
               raise Critical_Error;
         end case;

         Check (Set_Of (Type_Symbol) or The_Followers);

         Debug (Syntax_Debug, "end Declaration");
      end Declaration;

   begin
      Debug (Syntax_Debug, "begin Declarations");
      The_Declarations := null;

      while Declaration_Starters (The_Symbol) loop
         Declaration
           (Set_Of (Semicolon_Symbol) or The_Followers,
            The_Declaration);
         if The_Declarations = null then
            The_Declarations := The_Declaration;
         else
            The_Last.The_Next := The_Declaration;
         end if;
         The_Last := The_Declaration;

         Accept_Symbol (Semicolon_Symbol);

         Check (Declaration_Starters or The_Followers);
      end loop;

      Debug (Syntax_Debug, "end Declarations");
   end Declarations;

   procedure Type_Declaration
     (The_Followers   :     Set_Of_Symbols;
      The_Declaration : out Declaration_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Type_Declaration");
      The_Declaration := new Type_Declaration_Node;

      Accept_Symbol (Type_Symbol);

      if The_Symbol = Identifier_Symbol then
         Type_Declaration_Node (The_Declaration.all).The_Identifier :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      Accept_Symbol (Is_Symbol);

      case The_Symbol is
         when Array_Symbol =>
            Array_Definition
              (Set_Of (Type_Symbol) or The_Followers,
               Type_Declaration_Node (The_Declaration.all).The_Definition);
         when Mod_Symbol =>
            Mod_Definition
              (Set_Of (Type_Symbol) or The_Followers,
               Type_Declaration_Node (The_Declaration.all).The_Definition);
         when others =>
            Range_Definition
              (Set_Of (Type_Symbol) or The_Followers,
               Type_Declaration_Node (The_Declaration.all).The_Definition);
      end case;

      Check (Set_Of (Type_Symbol) or The_Followers);

      Debug (Syntax_Debug, "end Type_Declaration");
   end Type_Declaration;

   procedure Identifier_Declaration
     (The_Followers   :     Set_Of_Symbols;
      The_Declaration : out Declaration_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Identifier_Declaration");
      The_Declaration := new Identifier_Declaration_Node;

      if The_Symbol = Identifier_Symbol then
         Identifier_Declaration_Node (The_Declaration.all).The_Identifier :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      Accept_Symbol (Colon_Symbol);

      if The_Symbol = Constant_Symbol then
         Identifier_Declaration_Node (The_Declaration.all).Is_Constant := True;
         Accept_Symbol (Constant_Symbol);
      end if;

      if The_Symbol = Identifier_Symbol then
         Identifier_Declaration_Node (The_Declaration.all).The_Definition :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      if The_Symbol = Becomes_Symbol then
         Accept_Symbol (Becomes_Symbol);

         Expression
           (Set_Of (Semicolon_Symbol) or The_Followers,
            Identifier_Declaration_Node (The_Declaration.all).The_Expression);
      end if;

      Check (The_Followers);

      Debug (Syntax_Debug, "end Identifier_Declaration");
   end Identifier_Declaration;

   procedure Array_Definition
     (The_Followers  :     Set_Of_Symbols;
      The_Definition : out Definition_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Array_Definition");
      The_Definition := new Array_Definition_Node;

      Accept_Symbol (Array_Symbol);
      Accept_Symbol (Left_Paren_Symbol);

      if The_Symbol = Identifier_Symbol then
         Array_Definition_Node (The_Definition.all).The_Index :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);
      Accept_Symbol (Range_Symbol);

      Expression
        (Set_Of (Thru_Symbol) or The_Followers,
         Array_Definition_Node (The_Definition.all).The_First);

      Accept_Symbol (Thru_Symbol);

      Expression
        (Set_Of ((Right_Paren_Symbol, Of_Symbol)) or The_Followers,
         Array_Definition_Node (The_Definition.all).The_Last);

      Accept_Symbol (Right_Paren_Symbol);
      Accept_Symbol (Of_Symbol);

      if The_Symbol = Identifier_Symbol then
         Array_Definition_Node (The_Definition.all).The_Element :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      Check (The_Followers);

      Debug (Syntax_Debug, "end Array_Definition");
   end Array_Definition;

   procedure Range_Definition
     (The_Followers  :     Set_Of_Symbols;
      The_Definition : out Definition_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Range_Definition");
      The_Definition := null;

      if Check
          (Set_Of ((New_Symbol, Identifier_Symbol, Range_Symbol)),
           The_Followers)
      then
         Accept_Symbol (New_Symbol);

         The_Definition := new Range_Definition_Node;

         if The_Symbol = Identifier_Symbol then
            Range_Definition_Node (The_Definition.all).The_Identifier :=
              new Identifier_Symbol_Node'
                (The_String   => Scanner_Package.The_String,
                 The_Position => Scanner_Package.The_Position,
                 The_Pointer  => null);
         end if;
         Accept_Symbol (Identifier_Symbol);

         if The_Symbol = Range_Symbol then
            Accept_Symbol (Range_Symbol);

            Expression
              (Set_Of (Thru_Symbol) or The_Followers,
               Range_Definition_Node (The_Definition.all).The_First);

            Accept_Symbol (Thru_Symbol);

            Expression
              (The_Followers,
               Range_Definition_Node (The_Definition.all).The_Last);
         end if;

         Check (The_Followers);
      end if;

      Debug (Syntax_Debug, "end Range_Definition");
   end Range_Definition;

   procedure Mod_Definition
     (The_Followers  :     Set_Of_Symbols;
      The_Definition : out Definition_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Mod_Definition");
      The_Definition := new Mod_Definition_Node;

      Accept_Symbol (Mod_Symbol);

      Expression
        (The_Followers,
         Mod_Definition_Node (The_Definition.all).The_Expression);

      Check (The_Followers);

      Debug (Syntax_Debug, "end Mod_Definition");
   end Mod_Definition;

   procedure Statements
     (The_Followers  :     Set_Of_Symbols;
      The_Statements : out Statement_Graph)
   is

      The_Statement : Statement_Graph;
      The_Last      : Statement_Graph := null;

      procedure Statement
        (The_Followers :     Set_Of_Symbols;
         The_Statement : out Statement_Graph)
      is
      begin
         Debug (Syntax_Debug, "begin Statement");
         The_Statement := null;

         if Check (Statement_Starters, The_Followers) then
            case The_Symbol is
               when Null_Symbol =>
                  Null_Statement (The_Followers, The_Statement);
               when Identifier_Symbol =>
                  Assignment_Statement (The_Followers, The_Statement);
               when If_Symbol =>
                  If_Statement (The_Followers, The_Statement);
               when For_Symbol =>
                  For_Statement (The_Followers, The_Statement);
               when others =>
                  raise Critical_Error;
            end case;
            Check (The_Followers);
         end if;

         Debug (Syntax_Debug, "end Statement");
      end Statement;

   begin
      Debug (Syntax_Debug, "begin Statements");
      The_Statements := null;

      Statement (Set_Of (Semicolon_Symbol) or The_Followers, The_Statement);

      The_Statements := The_Statement;
      The_Last       := The_Statement;
      Accept_Symbol (Semicolon_Symbol);
      Check (Statement_Starters or The_Followers);

      while Statement_Starters (The_Symbol) loop
         Statement (Set_Of (Semicolon_Symbol) or The_Followers, The_Statement);

         The_Last.The_Next := The_Statement;
         The_Last          := The_Statement;

         Accept_Symbol (Semicolon_Symbol);

         Check (Statement_Starters or The_Followers);
      end loop;

      Debug (Syntax_Debug, "end Statements");
   end Statements;

   procedure Assignment_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph)
   is
      The_Identifier : Identifier_Symbol_Graph;
   begin
      Debug (Syntax_Debug, "begin Assignment_Statement");
      The_Statement := new Assignment_Statement_Node;

      if The_Symbol = Identifier_Symbol then
         The_Identifier :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      Variable
        (Set_Of (Becomes_Symbol) or The_Followers,
         The_Identifier,
         Assignment_Statement_Node (The_Statement.all).The_Variable);

      Accept_Symbol (Becomes_Symbol);

      Expression
        (The_Followers,
         Assignment_Statement_Node (The_Statement.all).The_Expression);

      Debug (Syntax_Debug, "end Assignment_Statement");
   end Assignment_Statement;

   procedure Variable
     (The_Followers  :     Set_Of_Symbols;
      The_Identifier :     Identifier_Symbol_Graph;
      The_Variable   : out Variable_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Variable");
      The_Variable := new Variable_Node;

      Variable_Node (The_Variable.all).The_Identifier := The_Identifier;

      if The_Symbol = Left_Paren_Symbol then
         Accept_Symbol (Left_Paren_Symbol);

         Expression
           (Set_Of (Right_Paren_Symbol) or The_Followers,
            Variable_Node (The_Variable.all).The_Expression);

         Accept_Symbol (Right_Paren_Symbol);
      end if;

      Debug (Syntax_Debug, "end Variable");
   end Variable;

   procedure Null_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin Null_Statement");
      The_Statement := new Null_Statement_Node;

      Accept_Symbol (Null_Symbol);

      Debug (Syntax_Debug, "end Null_Statement");
   end Null_Statement;

   procedure If_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin If_Statement");
      The_Statement := new If_Statement_Node;

      Accept_Symbol (If_Symbol);

      Expression
        (Set_Of ((Then_Symbol, Else_Symbol, End_Symbol)) or The_Followers,
         If_Statement_Node (The_Statement.all).The_Expression);

      Accept_Symbol (Then_Symbol);

      Statements
        (Set_Of ((Then_Symbol, Else_Symbol, End_Symbol)) or The_Followers,
         If_Statement_Node (The_Statement.all).The_Statements);

      if The_Symbol = Else_Symbol then
         Accept_Symbol (Else_Symbol);

         Statements
           (Set_Of ((End_Symbol)) or The_Followers,
            If_Statement_Node (The_Statement.all).The_Alternates);
      end if;

      Accept_Symbol (End_Symbol);
      Accept_Symbol (If_Symbol);

      Debug (Syntax_Debug, "end If_Statement");
   end If_Statement;

   procedure For_Statement
     (The_Followers :     Set_Of_Symbols;
      The_Statement : out Statement_Graph)
   is
   begin
      Debug (Syntax_Debug, "begin For_Statement");
      The_Statement := new For_Statement_Node;

      Accept_Symbol (For_Symbol);

      if The_Symbol = Identifier_Symbol then
         For_Statement_Node (The_Statement.all).The_Index :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);

      Accept_Symbol (In_Symbol);

      if The_Symbol = Reverse_Symbol then
         For_Statement_Node (The_Statement.all).Is_Reverse := True;
         Accept_Symbol (Reverse_Symbol);
      end if;

      if The_Symbol = Identifier_Symbol then
         For_Statement_Node (The_Statement.all).The_Definition :=
           new Identifier_Symbol_Node'
             (The_String   => Scanner_Package.The_String,
              The_Position => Scanner_Package.The_Position,
              The_Pointer  => null);
      end if;
      Accept_Symbol (Identifier_Symbol);
      Accept_Symbol (Range_Symbol);

      Expression
        (Set_Of (Thru_Symbol) or The_Followers,
         For_Statement_Node (The_Statement.all).The_First);

      Accept_Symbol (Thru_Symbol);

      Expression
        (Set_Of (Loop_Symbol) or The_Followers,
         For_Statement_Node (The_Statement.all).The_Last);

      Accept_Symbol (Loop_Symbol);

      Statements
        (Set_Of (End_Symbol) or The_Followers,
         For_Statement_Node (The_Statement.all).The_Statements);

      Accept_Symbol (End_Symbol);
      Accept_Symbol (Loop_Symbol);

      Check (The_Followers);

      Debug (Syntax_Debug, "end For_Statement");
   end For_Statement;

   procedure Expression
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph)
   is
      The_Left : Expression_Graph;
   begin
      Debug (Syntax_Debug, "begin Expression");

      Simple_Expression (Relational_Operators or The_Followers, The_Left);

      The_Expression := The_Left;

      if Relational_Operators (The_Symbol) then

         The_Expression := new Binary_Expression_Node;
         Binary_Expression_Node (The_Expression.all).The_Left := The_Left;

         case The_Symbol is
            when Equal_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Equal_Symbol);
            when Not_Equal_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Not_Equal_Symbol);
            when Less_Than_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Less_Than_Symbol);
            when Less_Than_Equal_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Less_Than_Equal_Symbol);
            when Greater_Than_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Greater_Than_Symbol);
            when Greater_Than_Equal_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Greater_Than_Equal_Symbol);
            when others =>
               raise Critical_Error;
         end case;

         Simple_Expression
           (The_Followers,
            Binary_Expression_Node (The_Expression.all).The_Right);

         The_Left := The_Expression;
      end if;

      Debug (Syntax_Debug, "end Expression");
   end Expression;

   procedure Simple_Expression
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph)
   is
      The_Left       : Expression_Graph;
      The_Operator   : Symbol;
      Unary_Operator : Boolean := False;
   begin
      Debug (Syntax_Debug, "begin Simple_Expression");
      The_Expression := null;

      if The_Symbol = Plus_Symbol or The_Symbol = Minus_Symbol then
         case The_Symbol is
            when Plus_Symbol =>
               Accept_Symbol (Plus_Symbol);
            when Minus_Symbol =>
               Unary_Operator := True;
               The_Operator   := The_Symbol;
               Accept_Symbol (Minus_Symbol);
            when others =>
               raise Critical_Error;
         end case;
      end if;

      Term_Expression (Addition_Operators or The_Followers, The_Left);

      if Unary_Operator then
         The_Expression := new Unary_Expression_Node;
         Unary_Expression_Node (The_Expression.all).The_Operator :=
           The_Operator;
         Unary_Expression_Node (The_Expression.all).The_Right := The_Left;
         The_Left                                             := The_Expression;
      end if;

      The_Expression := The_Left;

      while Addition_Operators (The_Symbol) loop

         The_Expression := new Binary_Expression_Node;
         Binary_Expression_Node (The_Expression.all).The_Left := The_Left;

         case The_Symbol is
            when Or_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Or_Symbol);
            when Xor_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Xor_Symbol);
            when Plus_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Plus_Symbol);
            when Minus_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Minus_Symbol);
            when others =>
               raise Critical_Error;
         end case;

         Term_Expression
           (Addition_Operators or The_Followers,
            Binary_Expression_Node (The_Expression.all).The_Right);

         The_Left := The_Expression;
      end loop;

      Debug (Syntax_Debug, "end Simple_Expression");
   end Simple_Expression;

   procedure Term_Expression
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph)
   is
      The_Left : Expression_Graph;
   begin
      Debug (Syntax_Debug, "begin Term_Expression");

      Factor (Mulplication_Operators or The_Followers, The_Left);

      The_Expression := The_Left;

      while Mulplication_Operators (The_Symbol) loop

         The_Expression := new Binary_Expression_Node;
         Binary_Expression_Node (The_Expression.all).The_Left := The_Left;

         case The_Symbol is
            when And_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (And_Symbol);
            when Times_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Times_Symbol);
            when Divide_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Divide_Symbol);
            when Rem_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Rem_Symbol);
            when Mod_Symbol =>
               Binary_Expression_Node (The_Expression.all).The_Operator :=
                 The_Symbol;
               Accept_Symbol (Mod_Symbol);
            when others =>
               raise Critical_Error;
         end case;

         Factor
           (Mulplication_Operators or The_Followers,
            Binary_Expression_Node (The_Expression.all).The_Right);

         The_Left := The_Expression;
      end loop;

      Debug (Syntax_Debug, "end Term_Expression");
   end Term_Expression;

   procedure Factor
     (The_Followers  :     Set_Of_Symbols;
      The_Expression : out Expression_Graph)
   is
      The_Identifier : Identifier_Symbol_Graph;
   begin
      Debug (Syntax_Debug, "begin Factor");

      The_Expression := null;

      if Check (Factor_Starters, The_Followers) then
         case The_Symbol is
            when Identifier_Symbol =>

               if The_Symbol = Identifier_Symbol then
                  The_Identifier :=
                    new Identifier_Symbol_Node'
                      (The_String   => Scanner_Package.The_String,
                       The_Position => Scanner_Package.The_Position,
                       The_Pointer  => null);
               end if;
               Accept_Symbol (Identifier_Symbol);

               if The_Symbol = Tick_Symbol then
                  Accept_Symbol (Tick_Symbol);

                  if The_Symbol = Identifier_Symbol then
                     The_Expression :=
                       new Attribute_Expression_Node'
                         (The_Identifier => The_Identifier,
                          The_String     => Scanner_Package.The_String,
                          The_Position   => Scanner_Package.The_Position,
                          The_Result     => null);
                  end if;
                  Accept_Symbol (Identifier_Symbol);

               else
                  The_Expression := new Variable_Expression_Node;
                  Variable
                    (The_Followers,
                     The_Identifier,
                     Variable_Expression_Node (The_Expression.all)
                       .The_Variable);
               end if;

            when Integer_Symbol =>
               The_Expression :=
                 new Integer_Expression_Node'
                   (The_String   => Scanner_Package.The_String,
                    The_Position => Scanner_Package.The_Position,
                    The_Result   => null);
               Accept_Symbol (Integer_Symbol);

            when Not_Symbol =>
               Accept_Symbol (Not_Symbol);
               The_Expression := new Unary_Expression_Node;
               Unary_Expression_Node (The_Expression.all).The_Operator :=
                 Not_Symbol;
               Factor
                 (The_Followers,
                  Unary_Expression_Node (The_Expression.all).The_Right);

            when Left_Paren_Symbol =>
               Accept_Symbol (Left_Paren_Symbol);
               Expression
                 (Set_Of (Right_Paren_Symbol) or The_Followers,
                  The_Expression);
               Accept_Symbol (Right_Paren_Symbol);
            when others =>
               raise Critical_Error;
         end case;

         Check (Factor_Starters or The_Followers);
      end if;

      Debug (Syntax_Debug, "end Factor");
   end Factor;

   -- Parse Boolean Compiler (BC) source code.

   procedure Parse (The_Unit : out Compilation_Unit_Graph) is
   begin
      Compilation_Unit (The_Unit);
   end Parse;

begin
   Debug (Debug_Initialization, "Syntax_Package");

end Syntax_Package;

-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Debug_Package;      use Debug_Package;
with Error_Package;      use Error_Package;
with Scanner_Package;    use Scanner_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
--
with Operand_Package.Image_Package;
with Ada.Text_IO;

-- NOTES:
--  j = 0
--  for i in 1 .. 4 loop
--     j = j + 1
--  end loop
--
--  j = j + 1
--  j = j + 1
--  j = j + 1
--  j = j + 1
--
--  -----
--  a range 1 .. 4
--  j = 0
--  for i in a .. 4 loop
--     j = j + 1
--  end loop
--
--  if a <= 4 then
--     i = a
--     i = i + 1
--     j = j + 1
--     if i <= 4 then
--        i = i + 1
--        j = j + 1
--        if i <= 4 then
--           i = i + 1
--           j = j + 1
--           if i <= 4 then
--              i = i + 1
--              j = j + 1
--           end if
--        end if
--     end if
--  end if
--
--  -----
--  a range 1 .. 4
--  j = 0
--  for i in 1 .. a loop
--     j = j + 1
--  end loop
--
--  if 1 <= a then
--     i = a
--     i = i + 1
--     j = j + 1
--     if i <= a then
--        i = i + 1
--        j = j + 1
--        if i <= a then
--           i = i + 1
--           j = j + 1
--           if i <= a then
--              i = i + 1
--              j = j + 1
--           end if
--        end if
--     end if
--  end if
--
--  -----
--  a range 1 .. 4
--  b range 1 .. 4
--  j = 0
--  for i in a .. b loop
--     j = j + 1
--  end loop
--
--  if a <= b then
--     i = a
--     i = i + 1
--     j = j + 1
--     if i <= b then
--        i = i + 1
--        j = j + 1
--        if i <= b then
--           i = i + 1
--           j = j + 1
--           if i <= b then
--              i = i + 1
--              j = j + 1
--           end if
--        end if
--     end if
--  end if

-- Error messages:
--
-- Expression not within type.                    --

package body Optimize_Package is

   -- Handle a Optimize error.

   procedure Optimize_Error
     (The_Position : Source_Position;
      The_Message  : String)
   is
   begin
      Error_Package.Source_Error (The_Position, The_Message);
   end Optimize_Error;

   -- Assign a new operand expression graph to the expression. Dispose of the
   -- old expression graph.

   procedure Assign
     (The_Expression : in out Expression_Graph;
      The_Operand    :        Operand_Pointer;
      The_Position   :        Source_Position)
   is
      The_Pointer : Expression_Graph;
   begin
      The_Pointer :=
        new Operand_Expression_Node'
          (The_Result => The_Operand, The_Position => The_Position);

      Dispose (The_Expression);
      The_Expression := The_Pointer;
   end Assign;

   -- Assign the left expression of a binary expression graph to the expression.
   -- Dispose of the old binary expression graph.

   procedure Assign_Left (The_Expression : in out Expression_Graph) is
      The_Pointer : Expression_Graph;
   begin
      The_Pointer := Binary_Expression_Node (The_Expression.all).The_Left;

      Binary_Expression_Node (The_Expression.all).The_Left := null;

      Dispose (The_Expression);
      The_Expression := The_Pointer;
   end Assign_Left;

   -- Assign the right expression of a binary expression graph to the
   -- expression. Dispose of the old binary expression graph.

   procedure Assign_Right (The_Expression : in out Expression_Graph) is
      The_Pointer : Expression_Graph;
   begin
      The_Pointer := Binary_Expression_Node (The_Expression.all).The_Right;

      Binary_Expression_Node (The_Expression.all).The_Right := null;

      Dispose (The_Expression);
      The_Expression := The_Pointer;
   end Assign_Right;

   -- BC Language constructs for optimization.

   procedure Compilation_Unit (The_Unit : in out Compilation_Unit_Graph);
   procedure Package_Body (The_Package : in out Package_Body_Graph);
   procedure Procedure_Body (The_Procedure : in out Procedure_Body_Graph);
   procedure Declarations (The_Declarations : in out Declaration_Graph);
   procedure Type_Declaration (The_Declaration : in out Declaration_Graph);
   procedure Identifier_Declaration (The_Declaration : in out Declaration_Graph);
   procedure Definition (The_Definition : in out Definition_Graph);
   procedure Range_Definition (The_Definition : in out Definition_Graph);
   procedure Mod_Definition (The_Definition : in out Definition_Graph);
   procedure Array_Definition (The_Definition : in out Definition_Graph);
   procedure Statements (The_Statements : in out Statement_Graph);
   procedure Assignment_Statement (The_Statement : in out Statement_Graph);

   procedure Variable
     (The_Variable : in out Variable_Graph;
      The_Operand  :    out Operand_Pointer);

   procedure If_Statement (The_Statement : in out Statement_Graph);
   procedure For_Statement (The_Statement : in out Statement_Graph);

   procedure Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer);

   procedure Unary_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer);

   procedure Binary_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer);

   procedure Variable_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer);

   procedure Integer_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer);

   procedure Compilation_Unit (The_Unit : in out Compilation_Unit_Graph) is
   begin
      Debug (Optimize_Debug, "begin Compilation_Unit");
      Package_Body (The_Unit.The_Package);
      Debug (Optimize_Debug, "end Compilation_Unit");
   end Compilation_Unit;

   procedure Package_Body (The_Package : in out Package_Body_Graph) is
   begin
      Debug (Optimize_Debug, "begin Package_Body");
      Declarations (The_Package.The_Declarations);
      Procedure_Body (The_Package.The_Procedure);
      Debug (Optimize_Debug, "end Package_Body");
   end Package_Body;

   procedure Procedure_Body (The_Procedure : in out Procedure_Body_Graph) is
   begin
      Debug (Optimize_Debug, "begin Procedure_Body");
      -- No optimization needed for Parameters.
      Statements (The_Procedure.The_Statements);
      Debug (Optimize_Debug, "end Procedure_Body");
   end Procedure_Body;

   procedure Declarations (The_Declarations : in out Declaration_Graph) is
      The_Declaration : Declaration_Graph := The_Declarations;
   begin
      Debug (Optimize_Debug, "begin Declarations");

      while The_Declaration /= null loop
         if The_Declaration.all in Type_Declaration_Node then
            Type_Declaration (The_Declaration);
         elsif The_Declaration.all in Identifier_Declaration_Node then
            Identifier_Declaration (The_Declaration);
         else
            raise Critical_Error;
         end if;

         The_Declaration := The_Declaration.The_Next;
      end loop;

      Debug (Optimize_Debug, "end Declarations");
   end Declarations;

   procedure Type_Declaration (The_Declaration : in out Declaration_Graph) is
   begin
      Debug (Optimize_Debug, "begin Type_Declaration");
      Definition (Type_Declaration_Node (The_Declaration.all).The_Definition);
      Debug (Optimize_Debug, "end Type_Declaration");
   end Type_Declaration;

   procedure Identifier_Declaration
     (The_Declaration : in out Declaration_Graph)
   is
      The_Expression : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin Identifier_Declaration");
      if Identifier_Declaration_Node (The_Declaration.all).The_Expression /=
        null
      then
         Expression
           (Identifier_Declaration_Node (The_Declaration.all).The_Expression,
            The_Expression);

         -- Dispose operand.
         Dispose (The_Expression);
      end if;

      Debug (Optimize_Debug, "end Identifier_Declaration");
   end Identifier_Declaration;

   procedure Definition (The_Definition : in out Definition_Graph) is
   begin
      Debug (Optimize_Debug, "begin Definition");

      if The_Definition.all in Array_Definition_Node then
         Array_Definition (The_Definition);
      elsif The_Definition.all in Range_Definition_Node then
         Range_Definition (The_Definition);
      elsif The_Definition.all in Mod_Definition_Node then
         Mod_Definition (The_Definition);
      else
         raise Critical_Error;
      end if;

      Debug (Optimize_Debug, "end Definition");
   end Definition;

   procedure Range_Definition (The_Definition : in out Definition_Graph) is
      The_First : Operand_Pointer;
      The_Last  : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin Range_Definition");
      if Range_Definition_Node (The_Definition.all).The_First /= null then
         Expression
           (Range_Definition_Node (The_Definition.all).The_First,
            The_First);
         Expression
           (Range_Definition_Node (The_Definition.all).The_Last,
            The_Last);

         -- Dispose operands.
         Dispose (The_First);
         Dispose (The_Last);
      end if;

      Debug (Optimize_Debug, "end Range_Definition");
   end Range_Definition;

   procedure Mod_Definition (The_Definition : in out Definition_Graph) is
      The_Expression : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin Mod_Definition");
      if Mod_Definition_Node (The_Definition.all).The_Expression /= null then
         Expression
           (Mod_Definition_Node (The_Definition.all).The_Expression,
            The_Expression);

         -- Dispose operand.
         Dispose (The_Expression);
      end if;
      Debug (Optimize_Debug, "end Mod_Definition");
   end Mod_Definition;

   procedure Array_Definition (The_Definition : in out Definition_Graph) is
      The_First : Operand_Pointer;
      The_Last  : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin Array_Definition");
      if Array_Definition_Node (The_Definition.all).The_First /= null then
         Expression
           (Array_Definition_Node (The_Definition.all).The_First,
            The_First);
         Expression
           (Array_Definition_Node (The_Definition.all).The_Last,
            The_Last);

         -- Dispose operands.
         Dispose (The_First);
         Dispose (The_Last);
      end if;
      Debug (Optimize_Debug, "end Array_Definition");
   end Array_Definition;

   procedure Statements (The_Statements : in out Statement_Graph) is
      The_Statement : Statement_Graph := The_Statements;
   begin
      Debug (Optimize_Debug, "begin Statements");
      while The_Statement /= null loop

         if The_Statement.all in Assignment_Statement_Node then
            Assignment_Statement (The_Statement);
         elsif The_Statement.all in Null_Statement_Node then
            -- No optimization needed for null statements.
            null;
         elsif The_Statement.all in If_Statement_Node then
            If_Statement (The_Statement);
         elsif The_Statement.all in For_Statement_Node then
            For_Statement (The_Statement);
         else
            raise Critical_Error;
         end if;

         The_Statement := The_Statement.The_Next;
      end loop;
      Debug (Optimize_Debug, "end Statements");
   end Statements;

   procedure Assignment_Statement (The_Statement : in out Statement_Graph) is
      The_Left  : Operand_Pointer;
      The_Right : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin Assignment_Statement");

      Variable
        (Assignment_Statement_Node (The_Statement.all).The_Variable,
         The_Left);

      if The_Left /= null then
         Expression
           (Assignment_Statement_Node (The_Statement.all).The_Expression,
            The_Right);

         if The_Right /= null and then Is_Constant (The_Right) then
            if not Is_Within
              (Constant_Operand (The_Right.all).The_Value,
               The_Left.The_Type)
            then
               -- not within type should be caught at optimize binary expression
               -- semantics
               raise Critical_Error;
            end if;
         end if;

         -- Dispose operands.
         Dispose (The_Left);
         Dispose (The_Right);
      end if;

      Debug (Optimize_Debug, "end Assignment_Statement");
   end Assignment_Statement;

   procedure Variable
     (The_Variable : in out Variable_Graph;
      The_Operand  :    out Operand_Pointer)
   is
      The_Type       : Type_Pointer;
      The_Expression : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin Variable");
      The_Operand := null;

      if Is_Array
        (Typed_Identifier (The_Variable.The_Identifier.The_Pointer.all)
         .The_Type)
      then

         Expression (The_Variable.The_Expression, The_Expression);

         if The_Expression /= null then

            The_Type :=
              Typed_Identifier (The_Variable.The_Identifier.The_Pointer.all)
              .The_Type;
            if Is_Constant (The_Expression) then
               if Is_Within
                 (Constant_Operand (The_Expression.all).The_Value,
                  The_Type)
               then
                  The_Operand :=
                    new Array_Operand'
                      (The_Type       => Array_Type (The_Type.all).The_Element,
                       The_Identifier =>
                         The_Variable.The_Identifier.The_Pointer,
                       The_Index => The_Expression);
                  The_Variable.The_Result := Copy (The_Operand);
               else
                  Optimize_Error
                    (Position_Of (The_Variable.The_Expression),
                     "Expression not within array index type (O1).");
               end if;
            else
               The_Operand :=
                 new Array_Operand'
                   (The_Type       => Array_Type (The_Type.all).The_Element,
                    The_Identifier => The_Variable.The_Identifier.The_Pointer,
                    The_Index      => The_Expression);
               The_Variable.The_Result := Copy (The_Operand);
            end if;
         end if;
      else
         The_Type :=
           Typed_Identifier (The_Variable.The_Identifier.The_Pointer.all)
           .The_Type;

         The_Operand :=
           new Identifier_Operand'
             (The_Type       => The_Type,
              The_Identifier => The_Variable.The_Identifier.The_Pointer);
         The_Variable.The_Result := Copy (The_Operand);
      end if;

      Debug (Optimize_Debug, "end Variable");
   end Variable;

   procedure If_Statement (The_Statement : in out Statement_Graph) is
      The_Expression : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin If_Statement");
      Expression
        (If_Statement_Node (The_Statement.all).The_Expression,
         The_Expression);

      Statements (If_Statement_Node (The_Statement.all).The_Statements);
      if If_Statement_Node (The_Statement.all).The_Alternates /= null then
         Statements (If_Statement_Node (The_Statement.all).The_Alternates);
      end if;

      -- Dispose operand.
      Dispose (The_Expression);

      Debug (Optimize_Debug, "end If_Statement");
   end If_Statement;

   procedure For_Statement (The_Statement : in out Statement_Graph) is
      The_Type  : Type_Pointer;
      The_First : Operand_Pointer;
      The_Last  : Operand_Pointer;
   begin
      Debug (Optimize_Debug, "begin For_Statement");

      Expression (For_Statement_Node (The_Statement.all).The_First, The_First);

      The_Type :=
        Index_Identifier
          (For_Statement_Node (The_Statement.all).The_Index.The_Pointer.all)
            .The_Type;

      if The_First /= null and then Is_Constant (The_First) then
         if not Is_Within
           (Constant_Operand (The_First.all).The_Value,
            The_Type)
         then
            -- not within type should be caught by optimize binary expression or
            -- semantics.
            raise Critical_Error;
         end if;
      end if;

      Expression (For_Statement_Node (The_Statement.all).The_Last, The_Last);

      if The_Last /= null and then Is_Constant (The_Last) then
         if not Is_Within
           (Constant_Operand (The_Last.all).The_Value,
            The_Type)
         then
            -- not within type should be caught by optimize binary expression or
            -- semantics.
            raise Critical_Error;
         end if;
      end if;

      Statements (For_Statement_Node (The_Statement.all).The_Statements);

      -- Dispose operands.
      Dispose (The_First);
      Dispose (The_Last);

      Debug (Optimize_Debug, "end For_Statement");
   end For_Statement;

   procedure Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer)
   is
   begin
      Debug (Optimize_Debug, "begin Expression");

      if The_Expression /= null then
         if The_Expression.all in Unary_Expression_Node then
            Unary_Expression (The_Expression, The_Operand);
         elsif The_Expression.all in Binary_Expression_Node then
            Binary_Expression (The_Expression, The_Operand);
         elsif The_Expression.all in Variable_Expression_Node then
            Variable_Expression (The_Expression, The_Operand);
         elsif The_Expression.all in Integer_Expression_Node then
            Integer_Expression (The_Expression, The_Operand);
         else
            raise Critical_Error;
         end if;
      end if;

      Debug (Optimize_Debug, "end Expression");
   end Expression;

   procedure Unary_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer)
   is
      The_Right : Operand_Pointer;
      The_Value : Integer;
   begin
      Debug (Optimize_Debug, "begin Unary_Expression");
      The_Operand := null;

      Expression
        (Unary_Expression_Node (The_Expression.all).The_Right,
         The_Right);

      case Unary_Expression_Node (The_Expression.all).The_Operator is
         when Not_Symbol =>
            if The_Right /= null and then Is_Constant (The_Right) then

               The_Value :=
                 Constant_Operation
                   (Unary_Expression_Node (The_Expression.all).The_Operator,
                    Constant_Operand (The_Right.all).The_Value);

               if Is_Within (The_Value, The_Right.The_Type) then
                  The_Operand :=
                    new Constant_Operand'
                      (The_Type => The_Right.The_Type, The_Value => The_Value);

                  Assign
                    (The_Expression,
                     Copy (The_Operand),
                     Position_Of
                       (Unary_Expression_Node (The_Expression.all).The_Right));
               else
                  Optimize_Error
                    (Position_Of
                       (Unary_Expression_Node (The_Expression.all).The_Right),
                     "Expression not within type (O1).");
               end if;

            else
               The_Operand :=
                 new Variable_Operand'(The_Type => The_Right.The_Type);
               The_Expression.The_Result := Copy (The_Operand);
            end if;

         when Minus_Symbol =>
            if The_Right /= null and then Is_Constant (The_Right) then

               The_Value :=
                 Constant_Operation
                   (Unary_Expression_Node (The_Expression.all).The_Operator,
                    Constant_Operand (The_Right.all).The_Value);

               if Is_Within (The_Value, The_Right.The_Type) then

                  The_Operand :=
                    new Constant_Operand'
                      (The_Type => The_Right.The_Type, The_Value => The_Value);

                  Assign
                    (The_Expression,
                     Copy (The_Operand),
                     Position_Of
                       (Unary_Expression_Node (The_Expression.all).The_Right));

               else
                  -- Note: In order to reach this code you must have a variable
                  -- expression such as (A * 0) to appy unary minus. Since
                  -- the only expression is something times zero any expression
                  -- would optimize to - 0 which is zero. Any expression with
                  -- a zero must be compatiable with the variable.
                  raise Critical_Error;
               end if;

            else
               The_Operand :=
                 new Variable_Operand'(The_Type => The_Right.The_Type);

               The_Expression.The_Result := Copy (The_Operand);
            end if;

         when others =>
            raise Critical_Error;
      end case;

      -- Dispose the operand.

      Dispose (The_Right);

      Debug (Optimize_Debug, "end Unary_Expression");
   end Unary_Expression;

   procedure Binary_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer)
   is
      The_Type    : Type_Pointer;
      The_Left    : Operand_Pointer;
      The_Right   : Operand_Pointer;
      The_Value   : Integer;
      The_Pointer : Expression_Graph;

   begin
      Debug (Optimize_Debug, "begin Binary_Expression");
      The_Operand := null;

      Expression
        (Binary_Expression_Node (The_Expression.all).The_Left,
         The_Left);
      Expression
        (Binary_Expression_Node (The_Expression.all).The_Right,
         The_Right);

      case Binary_Expression_Node (The_Expression.all).The_Operator is

         when Equal_Symbol           |
              Not_Equal_Symbol          |
              Less_Than_Symbol          |
              Less_Than_Equal_Symbol    |
              Greater_Than_Symbol       |
              Greater_Than_Equal_Symbol =>

            if (The_Right /= null and The_Left /= null)
              and then (Is_Constant (The_Left) and Is_Constant (The_Right))
            then

               The_Value :=
                 Constant_Operation
                   (Binary_Expression_Node (The_Expression.all).The_Operator,
                    Constant_Operand (The_Left.all).The_Value,
                    Constant_Operand (The_Right.all).The_Value);

               The_Operand :=
                 new Constant_Operand'
                   (The_Type => Boolean_Type, The_Value => The_Value);

               Assign
                 (The_Expression,
                  Copy (The_Operand),
                  Position_Of
                    (Binary_Expression_Node (The_Expression.all).The_Right));

            else
               The_Operand := new Variable_Operand'(The_Type => Boolean_Type);
               The_Expression.The_Result := Copy (The_Operand);
            end if;

         when And_Symbol | Or_Symbol | Xor_Symbol =>

            Debug(True, "1 " & Operand_Package.Image_Package.Image_Of(The_Left));
            Debug(True, "2 " & Operand_Package.Image_Package.Image_Of(The_Right));
            Debug(True, Integer'Image(Error_Package.The_Number_Of_Errors));
            The_Type := Best_Of (The_Left.The_Type, The_Right.The_Type);

            if (The_Right /= null and The_Left /= null)
              and then (Is_Constant (The_Left) and Is_Constant (The_Right))
            then

               The_Value :=
                 Constant_Operation
                   (Binary_Expression_Node (The_Expression.all).The_Operator,
                    Constant_Operand (The_Left.all).The_Value,
                    Constant_Operand (The_Right.all).The_Value);

               if Is_Within (The_Value, The_Type) then

                  The_Operand :=
                    new Constant_Operand'
                      (The_Type => The_Type, The_Value => The_Value);

                  Assign
                    (The_Expression,
                     Copy (The_Operand),
                     Position_Of
                       (Binary_Expression_Node (The_Expression.all).The_Right));

               else
                  Optimize_Error
                    (Position_Of
                       (Binary_Expression_Node (The_Expression.all).The_Right),
                     "Expression not within type (O2).");
               end if;

            elsif The_Right /= null and then Is_Constant (The_Right) then

               case Binary_Expression_Node (The_Expression.all).The_Operator is
                  when And_Symbol =>
                     if Constant_Operand (The_Right.all).The_Value =
                       Boolean_False
                     then
                        if Is_Within (Boolean_False, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type  => The_Type,
                                The_Value => Boolean_False);

                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));
                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O3).");
                        end if;

                     elsif Constant_Operand (The_Right.all).The_Value =
                       Boolean_True
                     then
                        Assign_Left (The_Expression);
                        The_Operand := Copy (The_Left);
                     end if;

                  when Or_Symbol =>
                     if Constant_Operand (The_Right.all).The_Value =
                       Boolean_False
                     then
                        Assign_Left (The_Expression);
                        The_Operand := Copy (The_Right);

                     elsif Constant_Operand (The_Right.all).The_Value =
                       Boolean_True
                     then
                        if Is_Within (Boolean_True, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type  => The_Type,
                                The_Value => Boolean_True);
                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));
                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O4).");
                        end if;
                     end if;

                  when Xor_Symbol =>
                     The_Operand := new Variable_Operand'(The_Type => The_Type);
                     The_Expression.The_Result := Copy (The_Operand);

                  when others =>
                     raise Critical_Error;
               end case;

            elsif The_Left /= null and then Is_Constant (The_Left) then

               case Binary_Expression_Node (The_Expression.all).The_Operator is
                  when And_Symbol =>
                     if Constant_Operand (The_Left.all).The_Value =
                       Boolean_False
                     then
                        if Is_Within (Boolean_False, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type  => The_Type,
                                The_Value => Boolean_False);
                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));
                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O5).");
                        end if;

                     elsif Constant_Operand (The_Left.all).The_Value =
                       Boolean_True
                     then
                        Assign_Right (The_Expression);
                        The_Operand := Copy (The_Right);
                     end if;

                  when Or_Symbol =>
                     if Constant_Operand (The_Left.all).The_Value =
                       Boolean_False
                     then
                        Assign_Right (The_Expression);
                        The_Operand := Copy (The_Left);

                     elsif Constant_Operand (The_Left.all).The_Value =
                       Boolean_True
                     then
                        if Is_Within (Boolean_True, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type  => The_Type,
                                The_Value => Boolean_True);
                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));
                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O6).");
                        end if;
                     end if;

                  when Xor_Symbol =>
                     The_Operand := new Variable_Operand'(The_Type => The_Type);
                     The_Expression.The_Result := Copy (The_Operand);

                  when others =>
                     raise Critical_Error;
               end case;
            else
               The_Operand := new Variable_Operand'(The_Type => The_Type);
               The_Expression.The_Result := Copy (The_Operand);
            end if;

         when Plus_Symbol | Minus_Symbol | Times_Symbol | Divide_Symbol =>
            The_Type := Best_Of (The_Left.The_Type, The_Right.The_Type);

            if (The_Right /= null and The_Left /= null)
              and then (Is_Constant (The_Left) and Is_Constant (The_Right))
            then

               The_Value :=
                 Constant_Operation
                   (Binary_Expression_Node (The_Expression.all).The_Operator,
                    Constant_Operand (The_Left.all).The_Value,
                    Constant_Operand (The_Right.all).The_Value);

               if Is_Within (The_Value, The_Type) then

                  The_Operand :=
                    new Constant_Operand'
                      (The_Type => The_Type, The_Value => The_Value);

                  Assign
                    (The_Expression,
                     Copy (The_Operand),
                     Position_Of
                       (Binary_Expression_Node (The_Expression.all).The_Right));

               else
                  Optimize_Error
                    (Position_Of
                       (Binary_Expression_Node (The_Expression.all).The_Right),
                     "Expression not within type (O7).");
               end if;

            elsif The_Right /= null and then Is_Constant (The_Right) then
               case Binary_Expression_Node (The_Expression.all).The_Operator is
                  when Plus_Symbol =>
                     if Constant_Operand (The_Right.all).The_Value = 0 then
                        Assign_Left (The_Expression);
                        The_Operand := Copy (The_Left);

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when Minus_Symbol =>
                     if Constant_Operand (The_Right.all).The_Value = 0 then
                        Assign_Left (The_Expression);
                        The_Operand := Copy (The_Left);

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when Times_Symbol =>
                     if Constant_Operand (The_Right.all).The_Value = 0 then
                        if Is_Within (0, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type => The_Type, The_Value => 0);
                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));

                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O8).");
                        end if;

                     elsif Constant_Operand (The_Right.all).The_Value = 1 then
                        Assign_Left (The_Expression);
                        The_Operand := Copy (The_Left);

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when Divide_Symbol =>
                     if Constant_Operand (The_Right.all).The_Value = 0 then
                        Optimize_Error
                          (Position_Of
                             (Binary_Expression_Node (The_Expression.all)
                              .The_Right),
                           "Expression divided by zero (O1).");

                     elsif Constant_Operand (The_Right.all).The_Value = 1 then
                        Assign_Left (The_Expression);
                        The_Operand := Copy (The_Left);

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when others =>
                     raise Critical_Error;
               end case;

            elsif The_Left /= null and then Is_Constant (The_Left) then
               case Binary_Expression_Node (The_Expression.all).The_Operator is
                  when Plus_Symbol =>
                     if Constant_Operand (The_Left.all).The_Value = 0 then
                        Assign_Right (The_Expression);
                        The_Operand := Copy (The_Left);
                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when Minus_Symbol =>
                     if Constant_Operand (The_Left.all).The_Value = 0 then
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);

                        -- unary expression - (negate)

                        The_Pointer :=
                          new Unary_Expression_Node'
                            (The_Operator => Minus_Symbol,
                             The_Right    =>
                               Binary_Expression_Node (The_Expression.all)
                             .The_Right,
                             The_Result => Copy (The_Operand));

                        Binary_Expression_Node (The_Expression.all).The_Right :=
                          null;
                        Dispose (The_Expression);

                        The_Expression := The_Pointer;

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when Times_Symbol =>
                     if Constant_Operand (The_Left.all).The_Value = 0 then
                        if Is_Within (0, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type => The_Type, The_Value => 0);
                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));

                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O9).");
                        end if;

                     elsif Constant_Operand (The_Left.all).The_Value = 1 then
                        Assign_Right (The_Expression);
                        The_Operand := Copy (The_Right);

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when Divide_Symbol =>
                     if Constant_Operand (The_Left.all).The_Value = 0 then
                        if Is_Within (0, The_Type) then
                           The_Operand :=
                             new Constant_Operand'
                               (The_Type => The_Type, The_Value => 0);
                           Assign
                             (The_Expression,
                              Copy (The_Operand),
                              Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right));

                        else
                           Optimize_Error
                             (Position_Of
                                (Binary_Expression_Node (The_Expression.all)
                                 .The_Right),
                              "Expression not within type (O10).");
                        end if;

                     else
                        The_Operand :=
                          new Variable_Operand'(The_Type => The_Type);
                        The_Expression.The_Result := Copy (The_Operand);
                     end if;

                  when others =>
                     raise Critical_Error;
               end case;

            else
               The_Operand := new Variable_Operand'(The_Type => The_Type);
               The_Expression.The_Result := Copy (The_Operand);
            end if;

         when others =>
            raise Critical_Error;
      end case;

      -- Dispose the operands.

      Dispose (The_Left);
      Dispose (The_Right);

      Debug (Optimize_Debug, "end Binary_Expression");
   end Binary_Expression;

   procedure Variable_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer)
   is
      The_Right      : Operand_Pointer;
      The_Identifier : Identifier_Pointer;
   begin
      Debug (Optimize_Debug, "begin Variable_Expression");
      The_Operand := null;

      Variable
        (Variable_Expression_Node (The_Expression.all).The_Variable,
         The_Right);

      if The_Right /= null then
         The_Identifier := Identifier_Operand (The_Right.all).The_Identifier;

         if Is_Variable (The_Identifier) then
            The_Operand               := The_Right;
            The_Expression.The_Result := Copy (The_Operand);

         elsif Is_Index (The_Identifier) then
            The_Operand               := The_Right;
            The_Expression.The_Result := Copy (The_Operand);

         elsif Is_Parameter (The_Identifier) then
            The_Operand               := The_Right;
            The_Expression.The_Result := Copy (The_Operand);

         elsif Is_Constant (The_Identifier) then
            The_Operand :=
              new Constant_Operand'
                (The_Type  => The_Right.The_Type,
                 The_Value =>
                   Constant_Identifier (The_Identifier.all).The_Value);

            Assign
              (The_Expression,
               Copy (The_Operand),
               Position_Of (The_Expression));

            -- Dispose the operand.
            Dispose (The_Right);

         else
            raise Critical_Error;
         end if;
      end if;

      Debug (Optimize_Debug, "end Variable_Expression");
   end Variable_Expression;

   procedure Integer_Expression
     (The_Expression : in out Expression_Graph;
      The_Operand    :    out Operand_Pointer)
   is
      The_Value : Integer  := 0;
      The_Last  : Positive := 1;
   begin
      Debug (Optimize_Debug, "begin Integer_Expression");

      Get
        (To_String (Integer_Expression_Node (The_Expression.all).The_String),
         The_Value,
         The_Last);

      The_Operand :=
        new Constant_Operand'
          (The_Type => Universal_Integer, The_Value => The_Value);

      Assign (The_Expression, Copy (The_Operand), Position_Of (The_Expression));

      Debug (Optimize_Debug, "end Integer_Expression");
   end Integer_Expression;

   -- Optimize a Boolean Compiler (BC) graph and check semenatics.

   procedure Optimize (The_Unit : in out Compilation_Unit_Graph) is
   begin
      Debug (Optimize_Debug, "begin Optimize");
      Compilation_Unit (The_Unit);
      Debug (Optimize_Debug, "end Optimize");
   end Optimize;

begin
   Debug (Debug_Initialization, "Optimize_Package");
end Optimize_Package;

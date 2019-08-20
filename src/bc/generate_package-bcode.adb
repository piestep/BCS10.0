-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Unchecked_Deallocation;
--
with Ada.Text_IO; use Ada.Text_IO;
--
with System_Package;     use System_Package;
with Debug_Package;      use Debug_Package;
with Error_Package;      use Error_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Graph_Package;      use Graph_Package;
with Identifier_Package; use Identifier_Package;
with Type_Package;       use Type_Package;
with Operand_Package;    use Operand_Package;
with Block_Package;      use Block_Package;
with Boolean_Package;    use Boolean_Package;
with Variable_Package;   use Variable_Package;
with Equation_Package;   use Equation_Package;
with Word_Package;       use Word_Package;
--
--with BCode_Package;                           use BCode_Package;
with BCode_Package.IO_Package; use BCode_Package.IO_Package;
--

separate (Generate_Package)
procedure BCode
  (The_Unit      :     Compilation_Unit_Graph;
   The_Variables : out Natural;
   The_Words     : out BCode_Words)
is

   -- Store the word to the identifier.

   procedure Store
     (The_Identifier : Identifier_Pointer;
      The_Word       : Word_Type)
   is
      And_The_Word : Word_Type;
   begin
      if Is_Assigned (The_Identifier) then
         And_The_Word := Look_Up (The_Identifier);
         Dispose (And_The_Word);
         Assign (The_Identifier, The_Word);
      else
         Assign (The_Identifier, The_Word);
      end if;
   end Store;

   -- BC Language constructs.

   procedure Generate (The_Unit : Compilation_Unit_Graph);
   procedure Generate (The_Package : Package_Body_Graph);
   procedure Generate (The_Procedure : Procedure_Body_Graph);
   procedure Generate (The_Statements : Statement_Graph);
   procedure Generate (The_Statement : Assignment_Statement_Graph);

   procedure Generate
     (The_Variable  :     Variable_Graph;
      The_Word      : out Word_Type;
      With_The_Word : out Word_Type);

   procedure Generate (The_Statement : If_Statement_Graph);
   procedure Generate (The_Statement : For_Statement_Graph);

   procedure Generate
     (The_Expression :     Expression_Graph;
      The_Word       : out Word_Type);

   procedure Generate
     (The_Expression :     Unary_Expression_Graph;
      The_Word       : out Word_Type);

   procedure Generate
     (The_Expression :     Binary_Expression_Graph;
      The_Word       : out Word_Type);

   procedure Generate
     (The_Expression :     Variable_Expression_Graph;
      The_Word       : out Word_Type);

   procedure Generate (The_Unit : Compilation_Unit_Graph) is
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Compilation_Unit");

         Generate (The_Unit.The_Package);

         Debug (BCode_Debug, "end Compilation_Unit");
      end if;
   end Generate;

   procedure Generate (The_Package : Package_Body_Graph) is
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Package_Body");

         Generate (The_Package.The_Procedure);

         Debug (BCode_Debug, "end Package_Body");
      end if;
   end Generate;

   procedure Generate (The_Procedure : Procedure_Body_Graph) is
      The_Parameter  : Parameter_Graph;
      The_Identifier : Identifier_Pointer;
      The_Size       : Natural;
      The_Word       : Word_Type;
      The_Type       : Type_Pointer;
      The_Element    : Type_Pointer;

      procedure Array_To_BCode_Words
        (The_Word  :        Word_Type;
         The_Size  :        Natural;
         The_Words : in out BCode_Words)
      is
         A_Word      : Word_Type;
         To_The_Word : Word_Type;
         The_Index   : Natural := 0;

         procedure Equations_To_Word (The_Equation : Equation_Type) is
         begin
            Append (To_The_Word, The_Equation);
            The_Index := The_Index + 1;
            if The_Index = The_Size then
               The_Index := 0;
               Copy (To_The_Word, A_Word);
               Append (The_Words, A_Word);
               Dispose (To_The_Word);
            end if;
         end Equations_To_Word;

      begin
         Iterate (The_Word, Equations_To_Word'Access);
      end Array_To_BCode_Words;

   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Procedure_Body");
         Block_Package.Open;

         -- Create and assign parameters to block.

         The_Parameter := The_Procedure.The_Parameters;
         while The_Parameter /= null loop

            The_Identifier := The_Parameter.The_Identifier.The_Pointer;

            The_Size :=
              Size_Of (Typed_Identifier (The_Identifier.all).The_Type);

            if The_Parameter.Is_In then
               Word_Package.Create_Variable
                 (The_Word       => The_Word,
                  The_Length     => The_Size,
                  The_Variable   => Variable_Package.Variable_Type (The_Variables));
               The_Variables := The_Variables + The_Size;
            else
               Create_Constant (The_Word => The_Word, The_Value => 0, The_Length => The_Size);
            end if;

            Assign (The_Identifier, The_Word);
            The_Parameter := The_Parameter.The_Next;
         end loop;

         Generate (The_Procedure.The_Statements);

         -- Append out paramters to words.

         The_Parameter := The_Procedure.The_Parameters;
         while The_Parameter /= null loop
            if The_Parameter.Is_Out then
               The_Identifier := The_Parameter.The_Identifier.The_Pointer;
               The_Type       :=
                 Type_Identifier (The_Parameter.The_Definition.The_Pointer.all)
                 .The_Type;
               if Is_Scalar (The_Type) then
                  Copy (Block_Package.Look_Up (The_Identifier), The_Word);
                  Append (The_Words, The_Word);
               elsif Is_Array (The_Type) then
                  The_Element := Array_Type (The_Type.all).The_Element;
                  Array_To_BCode_Words
                    (Block_Package.Look_Up (The_Identifier),
                     Size_Of (The_Element),
                     The_Words);
               else
                  raise Critical_Error;
               end if;
            end if;

            The_Parameter := The_Parameter.The_Next;
         end loop;

         Block_Package.Close;

         Debug (BCode_Debug, "end Procedure_Body");
      end if;
   end Generate;

   procedure Generate (The_Statements : Statement_Graph) is
      The_Statement : Statement_Graph := The_Statements;
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Statements");

         while The_Statement /= null loop

            if The_Statement.all in Assignment_Statement_Node then
               Generate (Assignment_Statement_Graph (The_Statement));
            elsif The_Statement.all in Null_Statement_Node then
               null;
            elsif The_Statement.all in If_Statement_Node then
               Generate (If_Statement_Graph (The_Statement));
            elsif The_Statement.all in For_Statement_Node then
               Generate (For_Statement_Graph (The_Statement));
            else
               raise Critical_Error;
            end if;

            The_Statement := The_Statement.The_Next;
         end loop;
         Debug (BCode_Debug, "end Statements");
      end if;
   end Generate;

   procedure Generate (The_Statement : Assignment_Statement_Graph) is
      The_Identifier : Identifier_Pointer;
      The_Type       : Type_Pointer;
      The_Word       : Word_Type;
      With_The_Word  : Word_Type;
      The_Right      : Word_Type;
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Assignment_Statement");

         The_Identifier :=
           The_Statement.The_Variable.The_Identifier.The_Pointer;
         The_Type := Typed_Identifier (The_Identifier.all).The_Type;

         Generate (The_Statement.The_Variable, The_Word, With_The_Word);

         if Is_Constant (The_Statement.The_Expression.The_Result) then
            if Is_Array (The_Type) then

               Create_Constant
                 (The_Word    => The_Right,
                  The_Value   =>
                    BCModular
                      (Constant_Operand
                           (The_Statement.The_Expression.The_Result.all)
                       .The_Value),
                  The_Length  =>
                    Size_Of (Array_Type (The_Type.all).The_Element));
            else
               Create_Constant
                 (The_Word      => The_Right,
                  The_Value     =>
                    BCModular
                      (Constant_Operand
                           (The_Statement.The_Expression.The_Result.all)
                       .The_Value),
                  The_Length    => Size_Of (The_Type));
            end if;
         else
            Generate (The_Statement.The_Expression, The_Right);
         end if;

         if Is_Array (The_Type) then
            Assign_Element (The_Word, The_Right, With_The_Word);
            Store (The_Identifier, The_Word);
            Dispose (With_The_Word);
            Dispose (The_Right);
         else
            Store (The_Identifier, The_Right);
         end if;

         Debug (BCode_Debug, "end Assignment_Statement");
      end if;
   end Generate;

   procedure Generate
     (The_Variable  :     Variable_Graph;
      The_Word      : out Word_Type;
      With_The_Word : out Word_Type)
   is
      The_Identifier : Identifier_Pointer;
      The_Type       : Type_Pointer;
      The_Size       : Natural;
      The_First      : Word_Type;
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Variable");

         The_Identifier := The_Variable.The_Identifier.The_Pointer;
         The_Type       := Typed_Identifier (The_Identifier.all).The_Type;

         -- array index.

         if Is_Array (The_Type) then
            if Is_Constant (The_Variable.The_Expression.The_Result) then
               -- literal index-first
               Create_Constant
                 (The_Word    => With_The_Word,
                  The_Value   =>
                    BCModular
                      (Constant_Operand
                           (The_Variable.The_Expression.The_Result.all)
                       .The_Value),
                  The_Length  => Size_Of (Array_Type (The_Type.all).The_Index));
            else
               Generate
                 (Variable_Node (The_Variable.all).The_Expression,
                  With_The_Word);
            end if;

            if First_Of (The_Type) /= 0 then
               Create_Constant
                 (The_Word    => The_First,
                  The_Value   => BCModular (First_Of (The_Type)),
                  The_Length  => Size_Of (Array_Type (The_Type.all).The_Index));

               Subtract (With_The_Word, The_First);
               Dispose (The_First);
            end if;
         end if;

         -- look up identifier.

         if Block_Package.Is_Entry (The_Identifier) then
            Copy (Block_Package.Look_Up (The_Identifier), The_Word);
         else
            if Is_Array (The_Type) then
               The_Size :=
                 Size_Of (Typed_Identifier (The_Identifier.all).The_Type);

               Create_Constant (The_Word => The_Word, The_Value => (1 .. The_Size => False));

            else
               -- Identifier must be variable. Parameter and
               -- Index identifiers are initialized.

               Create_Constant
                 (The_Word    => The_Word,
                  The_Value   =>
                    BCModular
                      (Variable_Identifier (The_Identifier.all).The_Value),
                  The_Length  =>
                    Size_Of
                      (Typed_Identifier (The_Identifier.all).The_Type));
            end if;
         end if;

         Debug (BCode_Debug, "end Variable");
      end if;
   end Generate;

   procedure Generate (The_Statement : If_Statement_Graph) is
      The_Word : Word_Type;
      The_Then : Block;
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin If_Statement");


         Generate (The_Statement.The_Expression, The_Word);

         Block_Package.Open;

         Generate (The_Statement.The_Statements);

         if The_Statement.The_Alternates /= null then
            Block_Package.Branch (The_Then);
            Generate (The_Statement.The_Alternates);
            Block_Package.Close (The_Word, The_Then);
         else
            Block_Package.Close (The_Word);
         end if;

         Debug (BCode_Debug, "end If_Statement");
      end if;
   end Generate;

   procedure Generate (The_Statement : For_Statement_Graph) is
      The_Identifier : Identifier_Pointer;
      The_Type       : Type_Pointer;
      The_First      : Integer;
      The_Last       : Integer;
      The_Start      : Word_Type;
      The_End        : Word_Type;
      The_Value      : Word_Type;

      procedure Unroll_Reverse_Loop_To_Constant is
         The_Condition : Word_Type;
      begin
         for The_Index in reverse The_First .. The_Last loop
            Block_Package.Open;
            Create_Constant
              (The_Word    => The_Value,
               The_Value   => BCModular (The_Index),
               The_Length  => Size_Of (The_Type));
            Store (The_Identifier, The_Value);

            Generate (The_Statement.The_Statements);

            -- f <= 0 --> not f > 0
            Copy (The_Start, The_Condition);
            if Is_Signed (The_Type) then
               Signed_Greater_Than (The_Condition, The_Value);
            else
               Unsigned_Greater_Than (The_Condition, The_Value);
            end if;
            Not_Op (The_Condition);

            Block_Package.Close (The_Condition);
         end loop;
      end Unroll_Reverse_Loop_To_Constant;

      procedure Unroll_Reverse_Loop_From_Constant is
         The_Condition : Word_Type;
      begin
         for The_Index in reverse The_First .. The_Last loop
            Block_Package.Open;
            Create_Constant
              (The_Word    => The_Value,
               The_Value   => BCModular (The_Index),
               The_Length  => Size_Of (The_Type));
            Store (The_Identifier, The_Value);

            Generate (The_Statement.The_Statements);

            -- l >= 0 or not (l < 0)
            Copy (The_End, The_Condition);
            if Is_Signed (The_Type) then
               Signed_Less_Than (The_Condition, The_Value);
            else
               Unsigned_Less_Than (The_Condition, The_Value);
            end if;
            Not_Op (The_Condition);

            Block_Package.Close (The_Condition);
         end loop;
      end Unroll_Reverse_Loop_From_Constant;

      procedure Unroll_Reverse_Loop_For_All is
         The_Condition1 : Word_Type;
         The_Condition2 : Word_Type;
      begin
         for The_Index in reverse The_First .. The_Last loop
            Block_Package.Open;
            Create_Constant
              (The_Word    => The_Value,
               The_Value   => BCModular (The_Index),
               The_Length  => Size_Of (The_Type));
            Store (The_Identifier, The_Value);

            Generate (The_Statement.The_Statements);

            -- f <= 0 --> not f > 0
            Copy (The_Start, The_Condition1);
            if Is_Signed (The_Type) then
               Signed_Greater_Than (The_Condition1, The_Value);
            else
               Unsigned_Greater_Than (The_Condition1, The_Value);
            end if;
            Not_Op (The_Condition1);

            -- l >= 0 or not (l < 0)
            Copy (The_End, The_Condition2);
            if Is_Signed (The_Type) then
               Signed_Less_Than (The_Condition2, The_Value);
            else
               Unsigned_Less_Than (The_Condition2, The_Value);
            end if;
            Not_Op (The_Condition2);

            And_Op (The_Condition1, The_Condition2);
            Dispose (The_Condition2);

            Block_Package.Close (The_Condition1);
         end loop;
      end Unroll_Reverse_Loop_For_All;

      procedure Unroll_Loop_To_Constant is
         The_Condition : Word_Type;
      begin
         for The_Index in The_First .. The_Last loop
            Block_Package.Open;
            Create_Constant
              (The_Word    => The_Value,
               The_Value   => BCModular (The_Index),
               The_Length  => Size_Of (The_Type));
            Store (The_Identifier, The_Value);

            Generate (The_Statement.The_Statements);

            -- f <= 0 --> not f > 0
            Copy (The_Start, The_Condition);
            if Is_Signed (The_Type) then
               Signed_Greater_Than (The_Condition, The_Value);
            else
               Unsigned_Greater_Than (The_Condition, The_Value);
            end if;
            Not_Op (The_Condition);

            Block_Package.Close (The_Condition);
         end loop;
      end Unroll_Loop_To_Constant;

      procedure Unroll_Loop_From_Constant is
         The_Condition : Word_Type;
      begin
         for The_Index in The_First .. The_Last loop
            Block_Package.Open;
            Create_Constant
              (The_Word    => The_Value,
               The_Value   => BCModular (The_Index),
               The_Length  => Size_Of (The_Type));
            Store (The_Identifier, The_Value);

            Generate (The_Statement.The_Statements);

            -- l >= 0 or not (l < 0)
            Copy (The_End, The_Condition);
            if Is_Signed (The_Type) then
               Signed_Less_Than (The_Condition, The_Value);
            else
               Unsigned_Less_Than (The_Condition, The_Value);
            end if;
            Not_Op (The_Condition);

            Block_Package.Close (The_Condition);
         end loop;
      end Unroll_Loop_From_Constant;

      procedure Unroll_Loop_For_All is
         The_Condition1 : Word_Type;
         The_Condition2 : Word_Type;
      begin
         for The_Index in The_First .. The_Last loop
            Block_Package.Open;
            Create_Constant
              (The_Word    => The_Value,
               The_Value   => BCModular (The_Index),
               The_Length  => Size_Of (The_Type));
            Store (The_Identifier, The_Value);

            Generate (The_Statement.The_Statements);

            -- f <= 0 --> not f > 0
            Copy (The_Start, The_Condition1);
            if Is_Signed (The_Type) then
               Signed_Greater_Than (The_Condition1, The_Value);
            else
               Unsigned_Greater_Than (The_Condition1, The_Value);
            end if;
            Not_Op (The_Condition1);

            -- l >= 0 or not (l < 0)
            Copy (The_End, The_Condition2);
            if Is_Signed (The_Type) then
               Signed_Less_Than (The_Condition2, The_Value);
            else
               Unsigned_Less_Than (The_Condition2, The_Value);
            end if;
            Not_Op (The_Condition2);

            And_Op (The_Condition1, The_Condition2);
            Dispose (The_Condition2);

            Block_Package.Close (The_Condition1);
         end loop;
      end Unroll_Loop_For_All;

   begin

      if Generate_BCode then
         Debug (BCode_Debug, "begin For_Statement");

         The_Identifier := The_Statement.The_Index.The_Pointer;

         if The_Statement.The_Definition /= null then
            The_Type :=
              Type_Identifier
                (For_Statement_Node (The_Statement.all).The_Definition
                 .The_Pointer.all)
              .The_Type;
         else
            The_Type := Type_Package.Integer_Type;
         end if;

         -- first and last operands.

         if Is_Constant (The_Statement.The_First.The_Result) then
            The_First :=
              Constant_Operand (The_Statement.The_First.The_Result.all)
              .The_Value;
            Create_Constant
              (The_Word    => The_Start,
               The_Value   => BCModular (The_First),
               The_Length  => Size_Of (The_Type));
         else
            Generate (The_Statement.The_First, The_Start);
            The_First := First_Of (The_Type);
         end if;

         if Is_Constant (The_Statement.The_Last.The_Result) then
            The_Last :=
              Constant_Operand (The_Statement.The_Last.The_Result.all)
              .The_Value;
            Create_Constant
              (The_Word    => The_End,
               The_Value   => BCModular (The_Last),
               The_Length  => Size_Of (The_Type));
         else
            Generate (The_Statement.The_Last, The_End);
            The_Last := Last_Of (The_Type);
         end if;

         if The_Statement.Is_Reverse then
            if Is_Constant (The_Start) and Is_Constant (The_End) then
               Block_Package.Open;
               for The_Index in reverse The_First .. The_Last loop
                  Create_Constant
                    (The_Word    => The_Value,
                     The_Value   => BCModular (The_Index),
                     The_Length  => Size_Of (The_Type));
                  Store (The_Identifier, The_Value);
                  Generate (The_Statement.The_Statements);
               end loop;
               Block_Package.Close;

            elsif Is_Constant (The_Start) then
               The_First :=
                 Constant_Operand (The_Statement.The_First.The_Result.all)
                 .The_Value;
               Unroll_Reverse_Loop_From_Constant;
            elsif Is_Constant (The_End) then
               The_Last :=
                 Constant_Operand (The_Statement.The_Last.The_Result.all)
                 .The_Value;
               Unroll_Reverse_Loop_To_Constant;
            else
               Unroll_Reverse_Loop_For_All;
            end if;

         else
            if Is_Constant (The_Start) and Is_Constant (The_End) then
               Block_Package.Open;
               for The_Index in The_First .. The_Last loop
                  Create_Constant
                    (The_Word    => The_Value,
                     The_Value   => BCModular (The_Index),
                     The_Length  => Size_Of (The_Type));
                  Store (The_Identifier, The_Value);
                  Generate (The_Statement.The_Statements);
               end loop;
               Block_Package.Close;
            elsif Is_Constant (The_Start) then
               The_First :=
                 Constant_Operand (The_Statement.The_First.The_Result.all)
                 .The_Value;
               Unroll_Loop_From_Constant;
            elsif Is_Constant (The_End) then
               The_Last :=
                 Constant_Operand (The_Statement.The_Last.The_Result.all)
                 .The_Value;
               Unroll_Loop_To_Constant;
            else
               Debug (BCode_Debug, "Unroll_Loop_For_All");
               Unroll_Loop_For_All;
            end if;
         end if;

         Debug (BCode_Debug, "end For_Statement");
      end if;
   end Generate;

   procedure Generate
     (The_Expression :     Expression_Graph;
      The_Word       : out Word_Type)
   is
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Expression");

         if The_Expression /= null then
            if The_Expression.all in Unary_Expression_Node then
               Generate (Unary_Expression_Graph (The_Expression), The_Word);
            elsif The_Expression.all in Binary_Expression_Node then
               Generate (Binary_Expression_Graph (The_Expression), The_Word);
            elsif The_Expression.all in Variable_Expression_Node then
               Generate (Variable_Expression_Graph (The_Expression), The_Word);
            else
               raise Critical_Error;
            end if;
         end if;

         Debug (BCode_Debug, "end Expression");
      end if;
   end Generate;

   procedure Generate
     (The_Expression :     Unary_Expression_Graph;
      The_Word       : out Word_Type)
   is
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Unary_Expression");

         Generate (The_Expression.The_Right, The_Word);

         case The_Expression.The_Operator is
            when Not_Symbol =>
               Not_Op (The_Word);
            when Minus_Symbol =>
               Negate (The_Word);
            when others =>
               raise Critical_Error;
         end case;

         Debug (BCode_Debug, "end Unary_Expression");
      end if;
   end Generate;

   procedure Generate
     (The_Expression :     Binary_Expression_Graph;
      The_Word       : out Word_Type)
   is

      The_Type  : Type_Pointer;
      The_Right : Word_Type;

      procedure Binary_Operation
        (The_Operator :        Symbol;
         The_Type     :        Type_Pointer;
         The_Left     : in out Word_Type;
         The_Right    :        Word_Type)
      is
      begin
         case The_Operator is
            when Equal_Symbol =>
               Not_Equal (The_Left, The_Right);
               Not_Op (The_Left);
            when Not_Equal_Symbol =>
               Not_Equal (The_Left, The_Right);
            when Less_Than_Symbol =>
               if Is_Signed (The_Type) then
                  Signed_Less_Than (The_Left, The_Right);
               else
                  Unsigned_Less_Than (The_Left, The_Right);
               end if;
            when Less_Than_Equal_Symbol =>
               if Is_Signed (The_Type) then
                  Signed_Greater_Than (The_Left, The_Right);
               else
                  Unsigned_Greater_Than (The_Left, The_Right);
               end if;
               Not_Op (The_Left);
            when Greater_Than_Symbol =>
               if Is_Signed (The_Type) then
                  Signed_Greater_Than (The_Left, The_Right);
               else
                  Unsigned_Greater_Than (The_Left, The_Right);
               end if;
            when Greater_Than_Equal_Symbol =>
               if Is_Signed (The_Type) then
                  Signed_Less_Than (The_Left, The_Right);
               else
                  Unsigned_Less_Than (The_Left, The_Right);
               end if;
               Not_Op (The_Left);
            when And_Symbol =>
               And_Op (The_Left, The_Right);
            when Or_Symbol =>
               Or_Op (The_Left, The_Right);
            when Xor_Symbol =>
               Xor_Op (The_Left, The_Right);
            when Plus_Symbol =>
               Add (The_Left, The_Right);
            when Minus_Symbol =>
               Subtract (The_Left, The_Right);
            when Times_Symbol =>
               Multiply (The_Left, The_Right);
            when Divide_Symbol =>
               if Is_Signed (The_Type) then
                  Signed_Divide (The_Left, The_Right);
               else
                  Unsigned_Divide (The_Left, The_Right);
               end if;
            when Rem_Symbol =>
               if Is_Signed (The_Type) then
                  Signed_Remainder (The_Left, The_Right);
               else
                  Unsigned_Remainder (The_Left, The_Right);
               end if;
            when others =>
               raise Critical_Error;
         end case;
      end Binary_Operation;

   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Binary_Expression");

         case The_Expression.The_Operator is
            when Equal_Symbol           |
                 Not_Equal_Symbol          |
                 Less_Than_Symbol          |
                 Less_Than_Equal_Symbol    |
                 Greater_Than_Symbol       |
                 Greater_Than_Equal_Symbol =>
               The_Type :=
                 Best_Of
                   (The_Expression.The_Left.The_Result.The_Type,
                    The_Expression.The_Right.The_Result.The_Type);
            when And_Symbol |
                 Or_Symbol     |
                 Xor_Symbol    |
                 Plus_Symbol   |
                 Minus_Symbol  |
                 Times_Symbol  |
                 Divide_Symbol  |
                 Rem_Symbol =>
               The_Type := The_Expression.The_Result.The_Type;
            when others =>
               raise Critical_Error;
         end case;

         if Is_Constant (The_Expression.The_Left.The_Result) then
            Create_Constant
              (The_Word    => The_Word,
               The_Value   =>
                 BCModular
                   (Constant_Operand (The_Expression.The_Left.The_Result.all)
                    .The_Value),
               The_Length  => Size_Of (The_Type));
         else
            Generate (The_Expression.The_Left, The_Word);
         end if;

         if Is_Constant
           (Binary_Expression_Node (The_Expression.all).The_Right.The_Result)
         then
            Create_Constant
              (The_Word    => The_Right,
               The_Value   =>
                 BCModular
                   (Constant_Operand
                        (The_Expression.The_Right.The_Result.all)
                    .The_Value),
               The_Length  => Size_Of (The_Type));
         else
            Generate (The_Expression.The_Right, The_Right);
         end if;

         Binary_Operation
           (The_Operator => The_Expression.The_Operator,
            The_Type     => The_Type,
            The_Left     => The_Word,
            The_Right    => The_Right);

         Dispose (The_Right);

         Debug (BCode_Debug, "end Binary_Expression");
      end if;
   end Generate;

   procedure Generate
     (The_Expression :     Variable_Expression_Graph;
      The_Word       : out Word_Type)
   is
      With_The_Word : Word_Type;
      The_Type      : Type_Pointer;
   begin
      if Generate_BCode then
         Debug (BCode_Debug, "begin Variable_Expression");

         The_Type :=
           Typed_Identifier
             (The_Expression.The_Variable.The_Identifier.The_Pointer.all)
           .The_Type;

         Generate (The_Expression.The_Variable, The_Word, With_The_Word);

         if Is_Array (The_Type) then
            Word_Package.Access_Element
              (The_Word,
               With_The_Word,
               Size_Of (The_Expression.The_Result.The_Type));
            Dispose (With_The_Word);
         end if;

         Debug (BCode_Debug, "end Variable_Expression");
      end if;
   end Generate;

begin
   Debug (BCode_Debug, "begin BCode");

   The_Variables := 0;
   Generate (The_Unit);

   Debug (BCode_Debug, "end BCode");
end BCode;

-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Tags; use Ada.Tags;
--
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
--
with Debug_Package;      use Debug_Package;
with Error_Package;      use Error_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Graph_Package;      use Graph_Package;
with Scope_Package;      use Scope_Package;
with Identifier_Package; use Identifier_Package;
with Type_Package;       use Type_Package;
with Operand_Package;    use Operand_Package;
--
with PCode_Package.IO_Package; use PCode_Package.IO_Package;
--
with Operand_Package.Image_Package; use Operand_Package.Image_Package;
--

separate (Generate_Package)
procedure PCode
  (The_Unit   :        Compilation_Unit_Graph;
   The_Code   : in out Code;
   The_Length :    out Address)
is

   package Address_IO is new Ada.Text_IO.Modular_IO (Address);

   The_Data : Address := 0; -- code data counter.

   -- Return pcode storage of a type.

   function Storage_Of (The_Type : Type_Pointer) return Natural is
   begin
      if The_Type = null then
         return 0;
      elsif Is_Scalar (The_Type) then
         return 1;
      elsif Is_Array (The_Type) then
         return Last_Of (The_Type) - First_Of (The_Type) + 1;
      else
         raise Critical_Error;
      end if;
   end Storage_Of;

   -- Append op code instruction to code.

   procedure Code (The_Operation : Op_Code; The_Comment : String := "") is
   begin
      The_Code (The_Length) := (The_Operation, 0, 0);
      if PCode_Dump then
         Put (Integer (The_Length), 4);
         Put (" : " & String_Of (The_Code (The_Length)));
         if The_Comment = "" then
            New_Line;
         else
            Put_Line (" ;     " & The_Comment);
         end if;
      end if;
      The_Length := The_Length + 1;
   end Code;

   -- Append op code instruction to code.

   procedure Code
     (The_Operation : Op_Code;
      The_Size      : Natural;
      The_Comment   : String := "")
   is
   begin
      The_Code (The_Length) := (The_Operation, Word (The_Size), 0);
      if PCode_Dump then
         Put (Integer (The_Length), 4);
         Put (" : " & String_Of (The_Code (The_Length)));
         if The_Comment = "" then
            New_Line;
         else
            Put_Line (" ;     " & The_Comment);
         end if;
      end if;
      The_Length := The_Length + 1;
   end Code;

   -- Append op code instruction to code.

   procedure Code
     (The_Operation : Op_Code;
      The_Size      : Natural;
      The_Operand   : Integer;
      The_Comment   : String := "")
   is
   begin
      The_Code (The_Length) :=
        (The_Operation, Word (The_Size), To_Word (The_Operand));
      if PCode_Dump then
         Put (Integer (The_Length), 4);
         Put (" : " & String_Of (The_Code (The_Length)));
         if The_Comment = "" then
            New_Line;
         else
            Put_Line (" ;     " & The_Comment);
         end if;
      end if;
      The_Length := The_Length + 1;
   end Code;

   procedure Comment (The_Comment : String := "") is
   begin
      if PCode_Dump then
         if The_Comment = "" then
            New_Line;
         else
            Put_Line ("   ; " & The_Comment);
         end if;
      end if;
   end Comment;

   -- BC Language constructs for pcode generation.

   procedure Generate (The_Unit : Compilation_Unit_Graph);
   procedure Generate (The_Package : Package_Body_Graph);
   procedure Generate (The_Procedure : Procedure_Body_Graph);
   procedure Generate (The_Parameters : Parameter_Graph);
   procedure Generate (The_Declarations : Declaration_Graph);
   procedure Generate (The_Declaration : Type_Declaration_Graph);
   procedure Generate (The_Declaration : Identifier_Declaration_Graph);
   procedure Generate (The_Statements : Statement_Graph);
   procedure Generate (The_Statement : Assignment_Statement_Graph);
   procedure Generate (The_Variable : Variable_Graph);
   procedure Generate (The_Statement : Null_Statement_Graph);
   procedure Generate (The_Statement : If_Statement_Graph);
   procedure Generate (The_Statement : For_Statement_Graph);
   procedure Generate (The_Expression : Expression_Graph);
   procedure Generate (The_Expression : Variable_Expression_Graph);
   procedure Generate (The_Expression : Unary_Expression_Graph);
   procedure Generate (The_Expression : Binary_Expression_Graph);

   procedure Generate (The_Unit : Compilation_Unit_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Compilation_Unit");
         Generate (The_Unit.The_Package);
         Debug (PCode_Debug, "end Compilation_Unit");
      end if;
   end Generate;

   procedure Generate (The_Package : Package_Body_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Package_Body");

         Code (incs_op, ADDRESS_SIZE, 0);
         Comment;

         Generate (The_Package.The_Declarations);
         Generate (The_Package.The_Procedure);

         Debug (PCode_Debug, "end Package_Body");
      end if;
   end Generate;

   procedure Generate (The_Procedure : Procedure_Body_Graph) is

      procedure Generate_Reads (The_Parameters : Parameter_Graph) is
         The_Parameter : Parameter_Graph := The_Parameters;
         The_Type      : Type_Pointer;
         The_Element   : Type_Pointer;
         The_Address   : Address;
      begin
         while The_Parameter /= null loop
            if Parameter_Node (The_Parameter.all).Is_In then

               The_Type :=
                 Type_Identifier
                   (Parameter_Node (The_Parameter.all).The_Definition
                      .The_Pointer.all)
                   .The_Type;
               The_Address :=
                 Address
                   (Parameter_Identifier
                      (Parameter_Node (The_Parameter.all).The_Identifier
                         .The_Pointer.all)
                      .The_Address);

               if Is_Array (The_Type) then

                  The_Element := Array_Type (The_Type.all).The_Element;

                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand => Natural (The_Address));

                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand =>
                       Array_Type (The_Type.all).The_Last -
                       Array_Type (The_Type.all).The_First +
                       1);
                  Code (rdb_op, The_Size => Size_Of (The_Element));

                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand =>
                       Array_Type (The_Type.all).The_Last -
                       Array_Type (The_Type.all).The_First +
                       1);
                  Code (stob_op, The_Size => Size_Of (The_Element));

               else
                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand => Natural (The_Address));
                  Code (rd_op, The_Size => Size_Of (The_Type));
                  Code (sto_op, The_Size => Size_Of (The_Type));
               end if;
            end if;
            The_Parameter := The_Parameter.The_Next;
         end loop;
      end Generate_Reads;

      procedure Generate_Writes (The_Parameters : Parameter_Graph) is
         The_Parameter : Parameter_Graph := The_Parameters;
         The_Type      : Type_Pointer;
         The_Element   : Type_Pointer;
         The_Address   : Address;
      begin
         while The_Parameter /= null loop
            if Parameter_Node (The_Parameter.all).Is_Out then

               The_Type :=
                 Type_Identifier
                   (Parameter_Node (The_Parameter.all).The_Definition
                      .The_Pointer.all)
                   .The_Type;
               The_Address :=
                 Address
                   (Parameter_Identifier
                      (Parameter_Node (The_Parameter.all).The_Identifier
                         .The_Pointer.all)
                      .The_Address);

               if Is_Array (The_Type) then

                  The_Element := Array_Type (The_Type.all).The_Element;

                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand => Natural (The_Address));

                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand =>
                       Array_Type (The_Type.all).The_Last -
                       Array_Type (The_Type.all).The_First +
                       1);

                  Code
                    (lodb_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand => Size_Of (The_Element));

                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand =>
                       Array_Type (The_Type.all).The_Last -
                       Array_Type (The_Type.all).The_First +
                       1);
                  Code (wrtb_op, The_Size => Size_Of (The_Element));

               else
                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand => Natural (The_Address));
                  Code (lod_op, The_Size => Size_Of (The_Type));
                  Code (wrt_op, The_Size => Size_Of (The_Type));
               end if;
            end if;
            The_Parameter := The_Parameter.The_Next;
         end loop;
      end Generate_Writes;

   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Procedure_Body");

         Generate (The_Procedure.The_Parameters);

         -- Update incs instruction to allocate variables.
         The_Code (0).The_Operand := Word (The_Data);
         Comment ("Total Data Size: " & Address'Image (The_Data));
         Comment;

         Generate_Reads (The_Procedure.The_Parameters);
         Generate (The_Procedure.The_Statements);
         Generate_Writes (The_Procedure.The_Parameters);

         Code (hlt_op);

         Debug (PCode_Debug, "end Procedure_Body");
      end if;
   end Generate;

   procedure Generate (The_Parameters : Parameter_Graph) is
      The_Parameter : Parameter_Graph := The_Parameters;

      procedure Generate_Parameter (The_Parameter : Parameter_Graph) is
         The_Type    : Type_Pointer;
         The_Address : Address;
         The_Storage : Natural;
      begin
         if Generate_PCode then
            Debug (PCode_Debug, "begin In_Parameter");

            The_Type :=
              Type_Identifier
                (Parameter_Node (The_Parameter.all).The_Definition
                   .The_Pointer.all)
                .The_Type;
            The_Storage := Storage_Of (The_Type);

            if PCode_Dump then
               Put (Integer (The_Data), 4);
               Put
                 (" : " &
                  To_Lower
                    (To_String
                       (Parameter_Node (The_Parameter.all).The_Identifier
                          .The_String)));
               if The_Storage /= 1 then
                  New_Line;
               else
                  Put_Line (" " & Natural'Image (The_Storage));
               end if;
            end if;

            -- allocate variable.
            The_Address := The_Data;
            The_Data    := The_Data + Address (The_Storage);

            Parameter_Identifier
              (Parameter_Node (The_Parameter.all).The_Identifier
                 .The_Pointer.all)
              .The_Address :=
              Natural (The_Address);

            Debug (PCode_Debug, "end In_Parameter");
         end if;
      end Generate_Parameter;

   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Parameters");

         while The_Parameter /= null loop
            Generate_Parameter (The_Parameter);
            The_Parameter := The_Parameter.The_Next;
         end loop;

         Debug (PCode_Debug, "end Parameters");
      end if;
   end Generate;

   procedure Generate (The_Declarations : Declaration_Graph) is
      The_Declaration : Declaration_Graph := The_Declarations;
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Declarations");

         while The_Declaration /= null loop

            if The_Declaration.all in Type_Declaration_Node then
               Generate (Type_Declaration_Graph (The_Declaration));
            elsif The_Declaration.all in Identifier_Declaration_Node then
               Generate (Identifier_Declaration_Graph (The_Declaration));
            else
               raise Critical_Error;
            end if;

            The_Declaration := The_Declaration.The_Next;
         end loop;

         Debug (PCode_Debug, "end Declarations");
      end if;
   end Generate;

   procedure Generate (The_Declaration : Type_Declaration_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Identifier_Declaration");
         -- no pcode generation needed for types declarations.
         Debug (PCode_Debug, "end Identifier_Declaration");
      end if;
   end Generate;

   procedure Generate (The_Declaration : Identifier_Declaration_Graph) is
      The_Type    : Type_Pointer;
      The_Address : Address;
      The_Storage : Natural;
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Identifier_Declaration");

         The_Type :=
           Type_Identifier (The_Declaration.The_Definition.The_Pointer.all)
             .The_Type;
         The_Storage := Storage_Of (The_Type);

         -- identifier declaration expression.

         if The_Declaration.The_Expression /= null then

            if Is_Variable (The_Declaration.The_Identifier.The_Pointer) then

               if PCode_Dump then
                  Put (Integer (The_Data), 4);
                  Put
                    (" : " &
                     To_Lower
                       (To_String
                          (The_Declaration.The_Identifier.The_String)));
                  if The_Storage /= 1 then
                     New_Line;
                  else
                     Put_Line (" " & Natural'Image (The_Storage));
                  end if;
               end if;

               -- allocate variable.
               The_Address := The_Data;
               The_Data    := The_Data + Address (The_Storage);

               Variable_Identifier
                 (The_Declaration.The_Identifier.The_Pointer.all)
                 .The_Address :=
                 Natural (The_Address);

               -- variable address.
               Code (lit_op, ADDRESS_SIZE, Natural (The_Address));

               if Is_Constant (The_Declaration.The_Expression.The_Result) then
                  Code
                    (lit_op,
                     The_Size    => Size_Of (The_Type),
                     The_Operand =>
                       Constant_Operand
                         (The_Declaration.The_Expression.The_Result.all)
                         .The_Value);
                    else
--  Don't think this is possiable.
                  Generate (The_Declaration.The_Expression);
               end if;

               -- store variable value.
               Code (sto_op, Size_Of (The_Type));
            end if;
         else

            if PCode_Dump then
               Put (Integer (The_Data), 4);
               Put
                 (" : " &
                  To_Lower
                    (To_String (The_Declaration.The_Identifier.The_String)));
               if The_Storage /= 1 then
                  New_Line;
               else
                  Put_Line (" " & Natural'Image (The_Storage));
               end if;
            end if;

            -- allocate variable.
            The_Address := The_Data;
            The_Data    := The_Data + Address (Storage_Of (The_Type));

            Variable_Identifier
              (The_Declaration.The_Identifier.The_Pointer.all)
              .The_Address :=
              Natural (The_Address);
         end if;

         Debug (PCode_Debug, "end Identifier_Declaration");
      end if;
   end Generate;

   procedure Generate (The_Statements : Statement_Graph) is
      The_Statement : Statement_Graph := The_Statements;
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Statements");
         while The_Statement /= null loop

            if The_Statement.all in Assignment_Statement_Node then
               Generate (Assignment_Statement_Graph (The_Statement));
            elsif The_Statement.all in Null_Statement_Node then
               Generate (Null_Statement_Graph (The_Statement));
            elsif The_Statement.all in If_Statement_Node then
               Generate (If_Statement_Graph (The_Statement));
            elsif The_Statement.all in For_Statement_Node then
               Generate (For_Statement_Graph (The_Statement));
            else
               raise Critical_Error;
            end if;

            The_Statement := The_Statement.The_Next;
         end loop;
         Debug (PCode_Debug, "end Statements");
      end if;
   end Generate;

   procedure Generate (The_Statement : Assignment_Statement_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Assignment_Statement");

         Generate (The_Statement.The_Variable);

         if Is_Constant (The_Statement.The_Expression.The_Result) then
            Code
              (lit_op,
               The_Size =>
                 Size_Of (The_Statement.The_Variable.The_Result.The_Type),
               The_Operand =>
                 Constant_Operand (The_Statement.The_Expression.The_Result.all)
                   .The_Value);
         else
            Generate (The_Statement.The_Expression);
         end if;
         Code
           (sto_op,
            Size_Of (The_Statement.The_Variable.The_Result.The_Type));

         Debug (PCode_Debug, "end Assignment_Statement");
      end if;
   end Generate;

   procedure Generate (The_Variable : Variable_Graph) is
      The_Type    : Type_Pointer;
      The_Address : Address;
      The_Value   : Integer;
      The_Size    : Natural;
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Variable");

         -- identfier array value.

         if The_Variable.The_Expression /= null then

            The_Type :=
              Typed_Identifier (The_Variable.The_Identifier.The_Pointer.all)
                .The_Type;

            The_Address :=
              Address
                (Addressable_Identifier
                   (The_Variable.The_Identifier.The_Pointer.all)
                   .The_Address);

            if Is_Constant (The_Variable.The_Expression.The_Result) then

               -- literal (index-first)*size+address
               The_Value :=
                 Constant_Operand (The_Variable.The_Expression.The_Result.all)
                   .The_Value;
               The_Size := Storage_Of (Array_Type (The_Type.all).The_Element);

               Code
                 (lit_op,
                  The_Size    => ADDRESS_SIZE,
                  The_Operand =>
                    (The_Value - First_Of (The_Type)) * The_Size +
                    Natural (The_Address));
            else
               Generate (The_Variable.The_Expression);

               The_Size := Storage_Of (Array_Type (The_Type.all).The_Element);

               if First_Of (The_Type) /= 0 then
                  -- first -
                  Code
                    (lit_op,
                     The_Size => Size_Of (Array_Type (The_Type.all).The_Index),
                     The_Operand => First_Of (The_Type));
                  Code (sub_op, ADDRESS_SIZE);
               end if;

               if The_Size /= 1 then
                  -- size *
                  Code
                    (lit_op,
                     The_Size    => ADDRESS_SIZE,
                     The_Operand =>
                       Storage_Of (Array_Type (The_Type.all).The_Element));
                  Code (umul_op, ADDRESS_SIZE);
               end if;

               -- address +
               Code
                 (lit_op,
                  The_Size    => ADDRESS_SIZE,
                  The_Operand => Natural (The_Address));
               Code (add_op, ADDRESS_SIZE);
            end if;

         elsif Is_Addressable (The_Variable.The_Identifier.The_Pointer) then
            Code
              (lit_op,
               ADDRESS_SIZE,
               Natural
                 (Addressable_Identifier
                    (Variable_Node (The_Variable.all).The_Identifier
                       .The_Pointer.all)
                    .The_Address));

         else
            raise Critical_Error;
         end if;

         Debug (PCode_Debug, "end Variable");
      end if;
   end Generate;

   procedure Generate (The_Statement : Null_Statement_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Null_Statement");
         -- no pcode needed for null statement.
         Debug (PCode_Debug, "end Null_Statement");
      end if;
   end Generate;

   procedure Generate (The_Statement : If_Statement_Graph) is
      The_Jump    : Address;
      The_Address : Address;
   begin
        if Generate_PCode then
            Debug (PCode_Debug, "begin If_Statement");

            Generate (The_Statement.The_Expression);

            The_Jump := The_Length;
            Code (jz_op, 0, 0);

            Generate (The_Statement.The_Statements);

            if The_Statement.The_Alternates /= null then

                The_Address := The_Length;
                Code (jmp_op, 0, 0);
                The_Code (The_Jump).The_Operand := Word (The_Length);
                The_Jump                        := The_Address;

                Generate (The_Statement.The_Alternates);
            end if;

            The_Code (The_Jump).The_Operand := Word (The_Length);

            Debug (PCode_Debug, "end If_Statement");
        end if;
   end Generate;

   procedure Generate (The_Statement : For_Statement_Graph) is
      The_Type : Type_Pointer;

      The_Index : Address;      -- loop index address.
      The_Limit : Address;      -- loop limit address;

      The_Loop : Address;
      The_Exit : Address;
      The_For  : Address;
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin For_Statement");

         if The_Statement.Is_Reverse then
            Comment ("for reverse loop");
         else
            Comment ("for loop");
         end if;

         if The_Statement.The_Definition /= null then
            The_Type :=
              Type_Identifier (The_Statement.The_Definition.The_Pointer.all)
                .The_Type;
         else
            The_Type := Type_Package.Integer_Type;
         end if;

         -- first and last operands.

         if The_Statement.Is_Reverse then
            Comment ("initial limit");
         else
            Comment ("initial index");
         end if;

         if Is_Constant (The_Statement.The_First.The_Result) then
            Code
              (lit_op,
               The_Size    => Size_Of (The_Type),
               The_Operand =>
                 Constant_Operand (The_Statement.The_First.The_Result.all)
                   .The_Value);
         else
            Generate (The_Statement.The_First);
         end if;

         if The_Statement.Is_Reverse then
            The_Limit := The_Data;
         else
            The_Index := The_Data;
         end if;

         The_Data := The_Data + Address (Storage_Of (The_Type));

         if The_Statement.Is_Reverse then
            Comment ("initial index");
         else
            Comment ("initial limit");
         end if;

         if Is_Constant (The_Statement.The_Last.The_Result) then
            Code
              (lit_op,
               The_Size    => Size_Of (The_Type),
               The_Operand =>
                 Constant_Operand (The_Statement.The_Last.The_Result.all)
                   .The_Value);
         else
            Generate (The_Statement.The_Last);
         end if;

         if The_Statement.Is_Reverse then
            The_Index := The_Data;
         else
            The_Limit := The_Data;
         end if;

         The_Data := The_Data + Address (Storage_Of (The_Type));

         Index_Identifier (The_Statement.The_Index.The_Pointer.all)
           .The_Address :=
           Natural (The_Index);

         Comment;

         --Load index.
         Code (lit_op, ADDRESS_SIZE, Natural (The_Index));
         Code (lod_op, Size_Of (The_Type), "load index");

         -- Load limit.
         Code (lit_op, ADDRESS_SIZE, Natural (The_Limit));
         Code (lod_op, Size_Of (The_Type), "load limit");

         -- Compare index and limit; index <= limit.

         if The_Statement.Is_Reverse then
            if Is_Signed (The_Type) then
               Code
                 (geq_op,
                  Size_Of (The_Type),
                  "index greater than equal limit");
            else
               Code
                 (ugeq_op,
                  Size_Of (The_Type),
                  "index greater than equal limit");
            end if;
         else
            if Is_Signed (The_Type) then
               Code
                 (leq_op,
                  Size_Of (The_Type),
                  "index less than equal limit");
            else
               Code
                 (uleq_op,
                  Size_Of (The_Type),
                  "index less than equal limit");
            end if;
         end if;

         The_For := The_Length;

         -- Jump to exit.
         Code (jz_op, 0, 0, "jump to exit");

         Comment;
         Comment (": loop");
         The_Loop := The_Length;

         Generate (The_Statement.The_Statements);
         Comment;

         -- Load index.
         Code (lit_op, ADDRESS_SIZE, Natural (The_Index));
         Code (lod_op, Size_Of (The_Type), "load index");

         -- Load limit.
         Code (lit_op, ADDRESS_SIZE, Natural (The_Limit));
         Code (lod_op, Size_Of (The_Type), "load limit");

         -- Compare index and limit; index /= limit.
         Code (neq_op, Size_Of (The_Type), "index /= limit");

         The_Exit := The_Length;

         -- Exit loop if index >= limit.
         Code (jz_op, 0, 0, "jump to exit");
         Comment;

         Comment ("increment/decrement index");
         -- Index address for store.
         Code (lit_op, ADDRESS_SIZE, Natural (The_Index), "index address");

         -- Load index.
         Code (lit_op, ADDRESS_SIZE, Natural (The_Index));
         Code (lod_op, Size_Of (The_Type), "load index");

         -- Add/Subtract 1.
         Code (lit_op, Size_Of (The_Type), 1);

         if The_Statement.Is_Reverse then
            Code (sub_op, Size_Of (The_Type), "index-1");
         else
            Code (add_op, Size_Of (The_Type), "index+1");
         end if;

         -- Store Index.
         Code (sto_op, Size_Of (The_Type), "store result to index");

         -- Jump to statements.

         Code (jmp_op, 0, Natural (The_Loop), "jump to loop");
         Comment;

         The_Code (The_For).The_Operand  := Word (The_Length);
         The_Code (The_Exit).The_Operand := Word (The_Length);

         Comment (": exit loop");
         Code (decs_op, ADDRESS_SIZE, 2);

         Comment ("end loop");
         Comment;

         Debug (PCode_Debug, "end For_Statement");
      end if;
   end Generate;

   procedure Generate (The_Expression : Expression_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Expression");

         if The_Expression /= null then
            if The_Expression.all in Unary_Expression_Node then
               Generate (Unary_Expression_Graph (The_Expression));
            elsif The_Expression.all in Binary_Expression_Node then
               Generate (Binary_Expression_Graph (The_Expression));
            elsif The_Expression.all in Variable_Expression_Node then
               Generate (Variable_Expression_Graph (The_Expression));
            else
               raise Critical_Error;
            end if;
         end if;

         Debug (PCode_Debug, "end Expression");
      end if;
   end Generate;

   procedure Generate (The_Expression : Unary_Expression_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Unary_Expression");

         Generate (The_Expression.The_Right);

         case The_Expression.The_Operator is
            when Not_Symbol =>
                Code
                 (not_op,
                  The_Size => Size_Of (The_Expression.The_Result.The_Type));
            when Minus_Symbol =>
               Code
                 (neg_op,
                  The_Size => Size_Of (The_Expression.The_Result.The_Type));
            when others =>
               raise Critical_Error;
         end case;

         Debug (PCode_Debug, "end Unary_Expression");
      end if;
   end Generate;

   procedure Generate (The_Expression : Binary_Expression_Graph) is

      The_Type : Type_Pointer;

      procedure Binary_Operation
        (The_Operator : Symbol;
         The_Type     : Type_Pointer)
      is
        begin
         case The_Operator is
            when Equal_Symbol =>
               Code (equ_op, The_Size => Size_Of (The_Type));
            when Not_Equal_Symbol =>
               Code (neq_op, The_Size => Size_Of (The_Type));
            when Less_Than_Symbol =>
               if Is_Signed (The_Type) then
                  Code (ls_op, The_Size => Size_Of (The_Type));
               else
                  Code (uls_op, The_Size => Size_Of (The_Type));
               end if;
            when Less_Than_Equal_Symbol =>
               if Is_Signed (The_Type) then
                  Code (leq_op, The_Size => Size_Of (The_Type));
               else
                  Code (uleq_op, The_Size => Size_Of (The_Type));
               end if;
            when Greater_Than_Symbol =>
               if Is_Signed (The_Type) then
                  Code (gtr_op, The_Size => Size_Of (The_Type));
               else
                  Code (ugtr_op, The_Size => Size_Of (The_Type));
               end if;
            when Greater_Than_Equal_Symbol =>
               if Is_Signed (The_Type) then
                  Code (geq_op, The_Size => Size_Of (The_Type));
               else
                  Code (ugeq_op, The_Size => Size_Of (The_Type));
               end if;
            when And_Symbol =>
               Code (and_op, The_Size => Size_Of (The_Type));
            when Or_Symbol =>
               Code (or_op, The_Size => Size_Of (The_Type));
            when Xor_Symbol =>
               Code (xor_op, The_Size => Size_Of (The_Type));
            when Plus_Symbol =>
               Code (add_op, The_Size => Size_Of (The_Type));
            when Minus_Symbol =>
               Code (sub_op, The_Size => Size_Of (The_Type));
            when Times_Symbol =>
               if Is_Signed (The_Type) then
                  Code (mul_op, The_Size => Size_Of (The_Type));
               else
                  Code (umul_op, The_Size => Size_Of (The_Type));
               end if;
            when Divide_Symbol =>
               if Is_Signed (The_Type) then
                  Code (div_op, The_Size => Size_Of (The_Type));
               else
                  Code (udiv_op, The_Size => Size_Of (The_Type));
               end if;
            when Rem_Symbol =>
               if Is_Signed (The_Type) then
                  Code (rem_op, The_Size => Size_Of (The_Type));
               else
                  Code (urem_op, The_Size => Size_Of (The_Type));
               end if;
            when others =>
               raise Critical_Error;
         end case;
      end Binary_Operation;

   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Binary_Expression");

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
              Divide_Symbol |
              Rem_Symbol =>
               The_Type := The_Expression.The_Result.The_Type;
            when others =>
               raise Critical_Error;
         end case;

         if Is_Constant (The_Expression.The_Left.The_Result) then
            Code
              (lit_op,
               The_Size    => Size_Of (The_Type),
               The_Operand =>
                 Constant_Operand (The_Expression.The_Left.The_Result.all)
                   .The_Value);
         else
            Generate (The_Expression.The_Left);
         end if;

         if Is_Constant (The_Expression.The_Right.The_Result) then
            Code
              (lit_op,
               The_Size    => Size_Of (The_Type),
               The_Operand =>
                 Constant_Operand (The_Expression.The_Right.The_Result.all)
                   .The_Value);
         else
            Generate (The_Expression.The_Right);
         end if;

         Binary_Operation
           (The_Operator => The_Expression.The_Operator,
            The_Type     => The_Type);

         Debug (PCode_Debug, "end Binary_Expression");
      end if;
   end Generate;

   procedure Generate (The_Expression : Variable_Expression_Graph) is
   begin
      if Generate_PCode then
         Debug (PCode_Debug, "begin Variable_Expression");

         Generate (The_Expression.The_Variable);

         Code
           (lod_op,
            The_Size => Size_Of (The_Expression.The_Result.The_Type));

         Debug (PCode_Debug, "end Variable_Expression");
      end if;
   end Generate;

begin
   Debug (PCode_Debug, "begin PCode");

   The_Length := 0;
   The_Code   := (The_Code'Range => (Op_Code'First, 0, 0));

   Generate (The_Unit);

   Debug (PCode_Debug, "end PCode");
end PCode;

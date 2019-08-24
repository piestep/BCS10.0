-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO;
--
--
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with System_Package;     use System_Package;
with Debug_Package;      use Debug_Package;
with Error_Package;      use Error_Package;
with Source_Package;     use Source_Package;
with Scanner_Package;    use Scanner_Package;
with Scope_Package;      use Scope_Package;
with Identifier_Package; use Identifier_Package;
with Type_Package;       use Type_Package;
with Operand_Package;    use Operand_Package;
--
with Type_Package.Image_Package;       use Type_Package.Image_Package;
with Identifier_Package.Image_Package; use Identifier_Package.Image_Package;
with Operand_Package.Image_Package;    use Operand_Package.Image_Package;
--

-- Error messages:
--
-- Base type not compatiable with expression       --
-- Expected scalar type \()([[:digit:]]+)(\).")                            --
-- Expected type identifier \()([[:digit:]]+)(\).") 		             --
-- Expression must be constant \()([[:digit:]]+)(\).") 	             --
-- Expression not compatiable with index \()([[:digit:]]+)(\).")           --
-- Expression not compatiable with type \()([[:digit:]]+)(\).")            --
-- Expression not within type \()([[:digit:]]+)(\).") 	             --
-- ("Expression value not compatiable with base type \()([[:digit:]]+)(\).") --
-- ("Expression value not within index constraint \()([[:digit:]]+)(\).")    --
-- ("Expression value not within type constraint \()([[:digit:]]+)(\).")     --
-- ("Identifier already defined \()([[:digit:]]+)(\).") 		     --
-- ("Identifier not compatiable with expression \()([[:digit:]]+)(\).")      --
-- ("Undefined type identifier \()([[:digit:]]+)(\).")
-- ("Length attribute requires array type \()([[:digit:]]+)(\).")
-- ("Expected attribute \()([[:digit:]]+)(\).")
--
-- Warning messages that require error codes:
--
-- ("Integer type may cause excessive BCode compilation \()([[:digit:]]+)(\).") --

package body Semantics_Package is

   -- The parameters' output length.

   The_Output : SYSNatural := 0;

   -- Handle a Semenatic error.

   procedure Semenatics_Error
     (The_Position : Source_Position;
      The_Message  : String)
   is
   begin
      Error_Package.Source_Error (The_Position, The_Message);
   end Semenatics_Error;

   -- Handle a Semenatic warning.

   procedure Semenatics_Warning
     (The_Position : Source_Position;
      The_Message  : String)
   is
   begin
      Error_Package.Source_Warning (The_Position, The_Message);
   end Semenatics_Warning;

   -- BC Language constructs.

   procedure Parse (The_Package : Package_Body_Graph);
   procedure Parse (The_Procedure : Procedure_Body_Graph);
   procedure Parse (The_Parameters : Parameter_Graph);
   procedure Parse (The_Declarations : Declaration_Graph);
   procedure Parse (The_Declaration : Type_Declaration_Graph);
   procedure Parse (The_Declaration : Identifier_Declaration_Graph);

   procedure Parse
     (The_Definition :     Definition_Graph;
      The_Type       : out Type_Pointer);

   procedure Parse
     (The_Definition :     Range_Definition_Graph;
      The_Type       : out Type_Pointer);

   procedure Parse
     (The_Definition :     Mod_Definition_Graph;
      The_Type       : out Type_Pointer);

   procedure Parse
     (The_Definition :     Array_Definition_Graph;
      The_Type       : out Type_Pointer);

   procedure Parse (The_Statements : Statement_Graph);
   procedure Parse (The_Statement : Assignment_Statement_Graph);

   procedure Parse (The_Variable : Variable_Graph);

   procedure Parse (The_Statement : Null_Statement_Graph);
   procedure Parse (The_Statement : If_Statement_Graph);
   procedure Parse (The_Statement : For_Statement_Graph);

   procedure Parse (The_Expression : Expression_Graph);

   procedure Parse (The_Expression : Unary_Expression_Graph);

   procedure Parse (The_Expression : Binary_Expression_Graph);

   procedure Parse (The_Expression : Variable_Expression_Graph);

   procedure Parse (The_Expression : Attribute_Expression_Graph);

   procedure Parse (The_Expression : Integer_Expression_Graph);

   procedure Parse (The_Package : Package_Body_Graph) is
      The_Identifier : Identifier_Pointer;
   begin
      Debug (Semenatics_Debug, "begin Package_Body");

      Scope_Package.Open;

      if Scope_Package.Is_Identifier (The_Package.The_Name.The_String) then
         Semenatics_Error
           (The_Package.The_Name.The_Position,
            "Identifier already defined (1).");
      else
         The_Identifier :=
           new Package_Identifier'
             (The_String => The_Package.The_Name.The_String);
         The_Package.The_Name.The_Pointer := The_Identifier;
         Scope_Package.Enter (The_Identifier);
      end if;

      Parse (The_Package.The_Declarations);
      Parse (The_Package.The_Procedure);

      if The_Package.The_Name.The_String =
        The_Package.The_Identifier.The_String
      then
         The_Package.The_Identifier.The_Pointer :=
           The_Package.The_Name.The_Pointer;
      else
         Semenatics_Error
           (The_Package.The_Identifier.The_Position,
            "Expected package identifier (1).");
      end if;

      Scope_Package.Close;

      Debug (Semenatics_Debug, "end Package_Body");
   end Parse;

   procedure Parse (The_Procedure : Procedure_Body_Graph) is
      The_Identifier : Identifier_Pointer;
   begin
      Debug (Semenatics_Debug, "begin Procedure_Body");

      Scope_Package.Open;

      if Scope_Package.Is_Identifier (The_Procedure.The_Name.The_String) then
         Semenatics_Error
           (The_Procedure.The_Name.The_Position,
            "Identifier already defined (2).");
      else
         The_Identifier :=
           new Procedure_Identifier'
             (The_String => The_Procedure.The_Name.The_String);
         The_Procedure.The_Name.The_Pointer := The_Identifier;
         Scope_Package.Enter (The_Identifier);
      end if;

      Parse (The_Procedure.The_Parameters);
      Parse (The_Procedure.The_Statements);

      if The_Procedure.The_Name.The_String =
        The_Procedure.The_Identifier.The_String
      then
         The_Procedure.The_Identifier.The_Pointer :=
           The_Procedure.The_Name.The_Pointer;
      else
         Semenatics_Error
           (The_Procedure.The_Identifier.The_Position,
            "Expected procedure identifier (1).");
      end if;

      Scope_Package.Close;

      Debug (Semenatics_Debug, "end Procedure_Body");
   end Parse;

   procedure Parse (The_Parameters : Parameter_Graph) is
      The_Parameter : Parameter_Graph := The_Parameters;

      procedure Parse_Parameter (The_Parameter : Parameter_Graph) is
         The_Identifier       : Identifier_Pointer;
         The_Type             : Type_Pointer;
         Duplicate_Identifier : Boolean := False;
      begin
         Debug (Semenatics_Debug, "begin Parameter");

         -- look up the identifier.

         if Scope_Package.Is_Identifier
           (The_Parameter.The_Identifier.The_String)
         then
            Semenatics_Error
              (The_Parameter.The_Identifier.The_Position,
               "Identifier already defined (3).");
            Duplicate_Identifier := True;
         end if;

         -- look up the type.

         if Scope_Package.Is_Identifier
           (The_Parameter.The_Definition.The_String)
         then
            The_Identifier :=
              Scope_Package.Look_Up (The_Parameter.The_Definition.The_String);

            The_Parameter.The_Definition.The_Pointer := The_Identifier;

            if Is_Type (The_Identifier) then
               The_Type := Type_Identifier (The_Identifier.all).The_Type;

               if The_Type = Integer_Type then
                  Semenatics_Warning
                    (The_Parameter.The_Definition.The_Position,
                     "Integer type may cause excessive BCode compilation (1).");
               end if;
            else
               Semenatics_Error
                 (The_Parameter.The_Definition.The_Position,
                  "Expected type identifier (1).");
            end if;
         else
            Scope_Package.Enter
              (new Type_Identifier'
                 (The_String => The_Parameter.The_Definition.The_String,
                  The_Type   => null));

            Semenatics_Error
              (The_Parameter.The_Definition.The_Position,
               "Undefined type identifier (1).");
         end if;

         -- enter identifier.

         if not Duplicate_Identifier then

            The_Identifier :=
              new Parameter_Identifier'
                (The_String  => The_Parameter.The_Identifier.The_String,
                 The_Type    => The_Type,
                 Is_In       => The_Parameter.Is_In,
                 Is_Out      => The_Parameter.Is_Out,
                 The_Address => 0);

            The_Parameter.The_Identifier.The_Pointer := The_Identifier;

            Scope_Package.Enter (The_Identifier);

            Debug
              (Semenatics_Debug,
               "Parameter_Identifier: " &
                 To_String (The_Parameter.The_Identifier.The_String));
            Debug (Semenatics_Debug, Image_Of (The_Identifier));
         end if;

         Debug (Semenatics_Debug, "end Parameter");
      end Parse_Parameter;

   begin
      Debug (Semenatics_Debug, "begin Parameters");
      while The_Parameter /= null loop
         Parse_Parameter (The_Parameter);
         The_Parameter := The_Parameter.The_Next;
      end loop;
      Debug (Semenatics_Debug, "end Parameters");
   end Parse;

   procedure Parse (The_Declarations : Declaration_Graph) is
      The_Declaration : Declaration_Graph := The_Declarations;
   begin
      Debug (Semenatics_Debug, "begin Declarations");

      while The_Declaration /= null loop
         if The_Declaration.all in Type_Declaration_Node then
            Parse (Type_Declaration_Graph (The_Declaration));
         elsif The_Declaration.all in Identifier_Declaration_Node then
            Parse (Identifier_Declaration_Graph (The_Declaration));
         else
            raise Critical_Error;
         end if;

         The_Declaration := The_Declaration.The_Next;
      end loop;

      Debug (Semenatics_Debug, "end Declarations");
   end Parse;

   procedure Parse (The_Declaration : Type_Declaration_Graph) is
      The_Type             : Type_Pointer;
      The_Identifier       : Identifier_Pointer;
      Duplicate_Identifier : Boolean := False;
   begin
      Debug (Semenatics_Debug, "begin Type_Declaration");

      -- look up the identifier.

      if Scope_Package.Is_Identifier
        (The_Declaration.The_Identifier.The_String)
      then
         Semenatics_Error
           (The_Declaration.The_Identifier.The_Position,
            "Identifier already defined (4).");
         Duplicate_Identifier := True;
      end if;

      -- type definition.

      Parse (The_Declaration.The_Definition, The_Type);

      -- enter identifier.

      if not Duplicate_Identifier then
         The_Identifier :=
           new Type_Identifier'
             (The_String => The_Declaration.The_Identifier.The_String,
              The_Type   => The_Type);

         The_Declaration.The_Identifier.The_Pointer := The_Identifier;

         Scope_Package.Enter (The_Identifier);

         Debug
           (Semenatics_Debug,
            "Type_Identifier " &
              To_String (The_Declaration.The_Identifier.The_String));
         Debug (Semenatics_Debug, Image_Of (The_Type));
      end if;

      Debug (Semenatics_Debug, "end Type_Declaration");
   end Parse;

   procedure Parse (The_Declaration : Identifier_Declaration_Graph) is
      The_Identifier       : Identifier_Pointer;
      The_Type             : Type_Pointer := null;
      The_Value            : SYSInteger   := 0;
      Duplicate_Identifier : Boolean      := False;
   begin
      Debug (Semenatics_Debug, "begin Identifier_Declaration");

      -- look up the identifier.

      if Scope_Package.Is_Identifier
        (The_Declaration.The_Identifier.The_String)
      then
         Semenatics_Error
           (The_Declaration.The_Identifier.The_Position,
            "Identifier already defined (5).");
         Duplicate_Identifier := True;
      end if;

      -- look up the type.

      if Scope_Package.Is_Identifier
        (The_Declaration.The_Definition.The_String)
      then
         The_Identifier :=
           Scope_Package.Look_Up (The_Declaration.The_Definition.The_String);

         The_Declaration.The_Definition.The_Pointer := The_Identifier;

         if Is_Type (The_Identifier) then
            The_Type :=
              Identifier_Package.Type_Identifier (The_Identifier.all).The_Type;

            if The_Type = Integer_Type then
               Semenatics_Warning
                 (The_Declaration.The_Definition.The_Position,
                  "Integer type may cause excessive BCode compilation (2).");
            end if;
         else
            Semenatics_Error
              (The_Declaration.The_Definition.The_Position,
               "Expected type identifier (2).");
         end if;
      else
         Scope_Package.Enter
           (new Type_Identifier'
              (The_String => The_Declaration.The_Definition.The_String,
               The_Type   => null));

         Semenatics_Error
           (The_Declaration.The_Definition.The_Position,
            "Undefined type identifier (2).");
      end if;

      -- identifier declaration expression.

      if The_Declaration.The_Expression /= null then

         Parse (The_Declaration.The_Expression);

         if The_Declaration.The_Expression.The_Result /= null then
            if The_Type = null or else Is_Scalar (The_Type) then
               if Operand_Package.Is_Constant
                 (The_Declaration.The_Expression.The_Result)
               then
                  if Is_Compatiable
                    (The_Type,
                     The_Declaration.The_Expression.The_Result.The_Type)
                  then
                     if Is_Within
                       (Constant_Operand
                          (The_Declaration.The_Expression.The_Result.all)
                        .The_Value,
                        The_Type)
                     then
                        The_Value :=
                          Constant_Operand
                            (The_Declaration.The_Expression.The_Result.all)
                          .The_Value;
                     else
                        Semenatics_Error
                          (Position_Of (The_Declaration.The_Expression),
                           "Expression value not compatiable with identifier type (1).");
                     end if;
                  else
                     Semenatics_Error
                       (Position_Of (The_Declaration.The_Expression),
                        "Identifier not compatiable with expression (1).");
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Declaration.The_Expression),
                     "Expression must be constant (1).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Declaration.The_Expression),
                  "Arrays can not be initialized (1).");
            end if;
         end if;

      else
         if The_Declaration.Is_Constant then
            Semenatics_Error
              (Position_Of (The_Declaration),
               "Expected expression for constant variable (1).");
         end if;
      end if;

      -- enter identifier.

      if not Duplicate_Identifier then
         if The_Declaration.Is_Constant then
            The_Identifier :=
              new Constant_Identifier'
                (The_String => The_Declaration.The_Identifier.The_String,
                 The_Type   => The_Type,
                 The_Value  => The_Value);

            The_Declaration.The_Identifier.The_Pointer := The_Identifier;

            Scope_Package.Enter (The_Identifier);

            Debug
              (Semenatics_Debug,
               "Constant_Identifier: " &
                 To_String (The_Declaration.The_Identifier.The_String));

         else
            The_Identifier :=
              new Variable_Identifier'
                (The_String  => The_Declaration.The_Identifier.The_String,
                 The_Type    => The_Type,
                 The_Value   => The_Value,
                 The_Address => 0);

            The_Declaration.The_Identifier.The_Pointer := The_Identifier;

            Scope_Package.Enter (The_Identifier);

            Debug
              (Semenatics_Debug,
               "Variable_Identifier: " &
                 To_String (The_Declaration.The_Identifier.The_String));

         end if;
      end if;

      Debug (Semenatics_Debug, "end Identifier_Declaration");
   end Parse;

   procedure Parse
     (The_Definition :     Definition_Graph;
      The_Type       : out Type_Pointer)
   is
   begin
      Debug (Semenatics_Debug, "begin Definitions");

      if The_Definition.all in Array_Definition_Node then
         Parse (Array_Definition_Graph (The_Definition), The_Type);

      elsif The_Definition.all in Range_Definition_Node then
         Parse (Range_Definition_Graph (The_Definition), The_Type);

      elsif The_Definition.all in Mod_Definition_Node then
         Parse (Mod_Definition_Graph (The_Definition), The_Type);

      else
         raise Critical_Error;
      end if;

      Debug (Semenatics_Debug, "end Definitions");
   end Parse;

   procedure Parse
     (The_Definition :     Range_Definition_Graph;
      The_Type       : out Type_Pointer)
   is
      The_Identifier : Identifier_Pointer;
      The_Base       : Type_Pointer := null;
      The_Modulas    : SYSInteger;

   begin
      Debug (Semenatics_Debug, "begin Range_Definition");

      The_Type := null;

      -- look up range type (identifier).

      if Scope_Package.Is_Identifier
        (The_Definition.The_Identifier.The_String)
      then
         The_Identifier :=
           Scope_Package.Look_Up (The_Definition.The_Identifier.The_String);

         if Is_Type (The_Identifier) then
            The_Base :=
              Identifier_Package.Type_Identifier (The_Identifier.all).The_Type;

            The_Definition.The_Identifier.The_Pointer := The_Identifier;
         else
            Semenatics_Error
              (The_Definition.The_Identifier.The_Position,
               "Expected type identifier (3).");
         end if;
      else
         Scope_Package.Enter
           (new Type_Identifier'
              (The_String => The_Definition.The_Identifier.The_String,
               The_Type   => null));

         Semenatics_Error
           (The_Definition.The_Identifier.The_Position,
            "Undefined type identifier (3).");
      end if;

      -- range definition expressions.

      if The_Definition.The_First /= null then

         Parse (The_Definition.The_First);

         if The_Definition.The_First.The_Result /= null then
            if Operand_Package.Is_Constant
              (The_Definition.The_First.The_Result)
            then
               if Is_Compatiable
                 (The_Base,
                  The_Definition.The_First.The_Result.The_Type)
               then
                  if not Is_Within
                    (Constant_Operand
                       (The_Definition.The_First.The_Result.all)
                     .The_Value,
                     The_Base)
                  then
                     Semenatics_Error
                       (Position_Of (The_Definition.The_First),
                        "Expression value not compatiable with base type (1).");
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Definition.The_First),
                     "Base type not compatiable with expression (1).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Definition.The_First),
                  "Expression must be constant (2).");
            end if;
         end if;

         Parse (The_Definition.The_Last);

         if The_Definition.The_Last.The_Result /= null then
            if Operand_Package.Is_Constant
              (The_Definition.The_Last.The_Result)
            then
               if Is_Compatiable
                 (The_Base,
                  The_Definition.The_Last.The_Result.The_Type)
               then
                  if not Is_Within
                    (Constant_Operand (The_Definition.The_Last.The_Result.all)
                     .The_Value,
                     The_Base)
                  then
                     Semenatics_Error
                       (Position_Of (The_Definition.The_Last),
                        "Expression value not compatiable with base type (2).");
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Definition.The_Last),
                     "Base type not compatiable with expression (2).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Definition.The_Last),
                  "Expression must be constant (3).");
            end if;
         end if;

         if
           (The_Definition.The_First.The_Result /= null
            and then Operand_Package.Is_Constant
              (The_Definition.The_First.The_Result)) and
           (The_Definition.The_Last.The_Result /= null
            and then Operand_Package.Is_Constant
              (The_Definition.The_Last.The_Result))
         then
            if The_Base /= null and then Is_Modular (The_Base) then
               The_Modulas := Modular_Type (The_Base.all).The_Modulas;
               The_Type    :=
                 new Modular_Type'
                   (The_Base  => The_Base,
                    The_First =>
                      Constant_Operand (The_Definition.The_First.The_Result.all)
                    .The_Value mod
                      The_Modulas,
                    The_Last =>
                      Constant_Operand (The_Definition.The_Last.The_Result.all)
                    .The_Value mod
                      The_Modulas,
                    The_Size =>
                      Size_Of
                        (Constant_Operand
                           (The_Definition.The_Last.The_Result.all)
                         .The_Value mod
                           The_Modulas),
                    The_Modulas => The_Modulas);
            elsif The_Base /= null and then Is_Signed (The_Base) then
               The_Type :=
                 new Signed_Type'
                   (The_Base  => The_Base,
                    The_First =>
                      Constant_Operand (The_Definition.The_First.The_Result.all)
                    .The_Value,
                    The_Last =>
                      Constant_Operand (The_Definition.The_Last.The_Result.all)
                    .The_Value,
                    The_Size =>
                      Size_Of
                        (Constant_Operand
                           (The_Definition.The_First.The_Result.all)
                         .The_Value,
                         Constant_Operand
                           (The_Definition.The_Last.The_Result.all)
                         .The_Value));
            else
               The_Type :=
                 new Discrete_Type'
                   (The_Base  => The_Base,
                    The_First =>
                      Constant_Operand (The_Definition.The_First.The_Result.all)
                    .The_Value,
                    The_Last =>
                      Constant_Operand (The_Definition.The_Last.The_Result.all)
                    .The_Value,
                    The_Size =>
                      Size_Of
                        (Constant_Operand
                           (The_Definition.The_Last.The_Result.all)
                         .The_Value));
            end if;
         end if;

      else
         if The_Base /= null and then Is_Modular (The_Base) then
            The_Modulas := Modular_Type (The_Base.all).The_Modulas;
            The_Type    :=
              new Modular_Type'
                (The_Base    => The_Base,
                 The_First   => First_Of (The_Base),
                 The_Last    => Last_Of (The_Base),
                 The_Size    => Size_Of (The_Base),
                 The_Modulas => The_Modulas);
         elsif The_Base /= null and then Is_Signed (The_Base) then
            The_Type :=
              new Signed_Type'
                (The_Base  => The_Base,
                 The_First => First_Of (The_Base),
                 The_Last  => Last_Of (The_Base),
                 The_Size  => Size_Of (The_Base));
         else
            The_Type :=
              new Discrete_Type'
                (The_Base  => The_Base,
                 The_First => First_Of (The_Base),
                 The_Last  => Last_Of (The_Base),
                 The_Size  => Size_Of (The_Base));
         end if;
      end if;

      Debug (Semenatics_Debug, Image_Of (The_Type));
      Debug (Semenatics_Debug, "end Range_Definition");
   end Parse;

   procedure Parse
     (The_Definition :     Mod_Definition_Graph;
      The_Type       : out Type_Pointer)
   is
      The_Size : SYSInteger := 0;
   begin
      Debug (Semenatics_Debug, "begin Mod_Definition");

      The_Type := null;

      Parse (The_Definition.The_Expression);

      if The_Definition.The_Expression.The_Result /= null then
         if Operand_Package.Is_Constant
           (The_Definition.The_Expression.The_Result)
         then
            if Is_Compatiable
              (Universal_Integer,
               The_Definition.The_Expression.The_Result.The_Type)
            then
               if Constant_Operand
                 (The_Definition.The_Expression.The_Result.all)
                 .The_Value >=
                   0
               then
                  The_Size :=
                    Size_Of
                      (Constant_Operand
                         (The_Definition.The_Expression.The_Result.all)
                       .The_Value -
                         1);
                  The_Type :=
                    new Modular_Type'
                      (The_Base  => Universal_Integer,
                       The_First => 0,
                       The_Last  =>
                         Constant_Operand
                           (The_Definition.The_Expression.The_Result.all)
                       .The_Value -
                         1,
                       The_Size    => The_Size,
                       The_Modulas =>
                         Constant_Operand
                           (The_Definition.The_Expression.The_Result.all)
                       .The_Value);
               else
                  Semenatics_Error
                    (Position_Of
                       (Mod_Definition_Node (The_Definition.all)
                        .The_Expression),
                     "Expression value not compatiable with base type (3).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Definition.The_Expression),
                  "Base type not compatiable with expression (3).");
            end if;
         else
            Semenatics_Error
              (Position_Of (The_Definition.The_Expression),
               "Expression must be constant (4).");
         end if;
      end if;

      Debug (Semenatics_Debug, Image_Of (The_Type));
      Debug (Semenatics_Debug, "end Mod_Definition");
   end Parse;

   procedure Parse
     (The_Definition :     Array_Definition_Graph;
      The_Type       : out Type_Pointer)
   is
      The_Index      : Type_Pointer := null;
      The_Identifier : Identifier_Pointer;
   begin
      Debug (Semenatics_Debug, "begin Array_Definition");
      The_Type := null;

      -- look up index (identifier) type.

      if Scope_Package.Is_Identifier (The_Definition.The_Index.The_String) then
         The_Identifier :=
           Scope_Package.Look_Up (The_Definition.The_Index.The_String);

         if Is_Type (The_Identifier) then
            if Type_Identifier (The_Identifier.all).The_Type /= null
              and then Is_Scalar (Type_Identifier (The_Identifier.all).The_Type)
            then
               The_Index := Type_Identifier (The_Identifier.all).The_Type;

               The_Definition.The_Index.The_Pointer := The_Identifier;

               if The_Index = Integer_Type then
                  Semenatics_Warning
                    (The_Definition.The_Index.The_Position,
                     "Integer type may cause excessive BCode compilation (3).");
               end if;
            else
               Semenatics_Error
                 (The_Definition.The_Index.The_Position,
                  "Expected scalar type (1).");
            end if;
         else
            Semenatics_Error
              (The_Definition.The_Index.The_Position,
               "Expected type identifier (4).");
         end if;
      else
         Scope_Package.Enter
           (new Type_Identifier'
              (The_String => The_Definition.The_Index.The_String,
               The_Type   => null));

         Semenatics_Error
           (The_Definition.The_Index.The_Position,
            "Undefined type identifier (4).");
      end if;

      -- first and last operands.

      if The_Definition.The_First /= null then

         Parse (The_Definition.The_First);
         Parse (The_Definition.The_Last);

         if The_Definition.The_First.The_Result /= null then
            if Operand_Package.Is_Constant
              (The_Definition.The_First.The_Result)
            then
               if Is_Compatiable
                 (The_Index,
                  The_Definition.The_First.The_Result.The_Type)
               then
                  if not Is_Within
                    (Constant_Operand
                       (The_Definition.The_First.The_Result.all)
                     .The_Value,
                     The_Index)
                  then
                     Semenatics_Error
                       (Position_Of (The_Definition.The_First),
                        "Expression value not within type constraint (1).");
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Definition.The_First),
                     "Expression not compatiable with type (1).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Definition.The_First),
                  "Expression must be constant (5).");
            end if;
         end if;

         if The_Definition.The_Last.The_Result /= null then
            if Operand_Package.Is_Constant
              (The_Definition.The_Last.The_Result)
            then
               if Is_Compatiable
                 (The_Index,
                  The_Definition.The_Last.The_Result.The_Type)
               then
                  if not Is_Within
                    (Constant_Operand (The_Definition.The_Last.The_Result.all)
                     .The_Value,
                     The_Index)
                  then
                     Semenatics_Error
                       (Position_Of (The_Definition.The_Last),
                        "Expression value not within type constraint (2).");
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Definition.The_Last),
                     "Expression not compatiable with type (2).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Definition.The_Last),
                  "Expression must be constant (6).");
            end if;
         end if;
      end if;

      -- look up element (element) type.

      if Scope_Package.Is_Identifier
        (The_Definition.The_Element.The_String)
      then
         The_Identifier :=
           Scope_Package.Look_Up (The_Definition.The_Element.The_String);

         The_Definition.The_Element.The_Pointer := The_Identifier;

         if Is_Type (The_Identifier) then
            if Type_Identifier (The_Identifier.all).The_Type /= null
              and then Is_Scalar (Type_Identifier (The_Identifier.all).The_Type)
            then
               The_Type := Type_Identifier (The_Identifier.all).The_Type;

               if The_Type = Integer_Type then
                  Semenatics_Warning
                    (The_Definition.The_Element.The_Position,
                     "Integer type may cause excessive BCode compilation (4).");
               end if;
            else
               Semenatics_Error
                 (The_Definition.The_Element.The_Position,
                  "Expected scalar type (2).");
            end if;
         else
            Semenatics_Error
              (The_Definition.The_Element.The_Position,
               "Expected type identifier (5).");
         end if;
      else
         Scope_Package.Enter
           (new Type_Identifier'
              (The_String => The_Definition.The_Element.The_String,
               The_Type   => null));

         Semenatics_Error
           (The_Definition.The_Element.The_Position,
            "Undefined type identifier (5).");
      end if;

      if
        (The_Definition.The_First.The_Result /= null
         and then Operand_Package.Is_Constant
           (The_Definition.The_First.The_Result)) and
        (The_Definition.The_Last.The_Result /= null
         and then Operand_Package.Is_Constant
           (The_Definition.The_Last.The_Result))
      then
         The_Type :=
           new Array_Type'
             (The_Base  => null,
              The_Index => The_Index,
              The_First =>
                Constant_Operand (The_Definition.The_First.The_Result.all)
              .The_Value,
              The_Last =>
                Constant_Operand (The_Definition.The_Last.The_Result.all)
              .The_Value,
              The_Element => The_Type);
      end if;

      Debug (Semenatics_Debug, "end Array_Definition");
   end Parse;

   procedure Parse (The_Statements : Statement_Graph) is
      The_Statement : Statement_Graph := The_Statements;
   begin
      Debug (Semenatics_Debug, "begin Statements");
      while The_Statement /= null loop

         if The_Statement.all in Assignment_Statement_Node then
            Parse (Assignment_Statement_Graph (The_Statement));
         elsif The_Statement.all in Null_Statement_Node then
            Parse (Null_Statement_Graph (The_Statement));
         elsif The_Statement.all in If_Statement_Node then
            Parse (If_Statement_Graph (The_Statement));
         elsif The_Statement.all in For_Statement_Node then
            Parse (For_Statement_Graph (The_Statement));

         else
            raise Critical_Error;
         end if;

         The_Statement := The_Statement.The_Next;
      end loop;
      Debug (Semenatics_Debug, "end Statements");
   end Parse;

   procedure Parse (The_Statement : Assignment_Statement_Graph) is
      The_Identifier : Identifier_Pointer := null;
      The_Type       : Type_Pointer       := null;
   begin
      Debug (Semenatics_Debug, "begin Assignment_Statement");

      Parse (The_Statement.The_Variable);

      if The_Statement.The_Variable.The_Result /= null then
         if Is_Identifier (The_Statement.The_Variable.The_Result) then
            The_Identifier :=
              Identifier_Operand (The_Statement.The_Variable.The_Result.all)
              .The_Identifier;
            The_Type := The_Statement.The_Variable.The_Result.The_Type;

            if Is_Parameter (The_Identifier) then
               if not Parameter_Identifier (The_Identifier.all).Is_Out then
                  Semenatics_Error
                    (Position_Of (The_Statement.The_Variable),
                     "Assignment to in mode parameter not allowed (1).");
               end if;
            elsif not Is_Variable (The_Identifier) then
               Semenatics_Error
                 (Position_Of (The_Statement.The_Variable),
                  "Expected variable (1).");
            end if;
         end if;
      end if;

      Parse (The_Statement.The_Expression);

      if The_Statement.The_Variable.The_Result /= null and
        The_Statement.The_Expression.The_Result /= null
      then
         if Is_Compatiable
           (The_Statement.The_Variable.The_Result.The_Type,
            The_Statement.The_Expression.The_Result.The_Type)
         then
            if Is_Constant (The_Statement.The_Expression.The_Result) then
               if not Is_Within
                 (Constant_Operand
                    (The_Statement.The_Expression.The_Result.all)
                  .The_Value,
                  The_Statement.The_Variable.The_Result.The_Type)
               then
                  Semenatics_Error
                    (Position_Of (The_Statement.The_Expression),
                     "Expression not within type (1).");
               end if;
            end if;
         else
            Semenatics_Error
              (Position_Of (The_Statement.The_Expression),
               "Identifier not compatiable with expression (2).");
         end if;
      end if;

      Debug (Semenatics_Debug, "end Assignment_Statement");
   end Parse;

   procedure Parse (The_Variable : Variable_Graph) is
      The_Identifier       : Identifier_Pointer := null;
      The_Type             : Type_Pointer;
      Undefined_Identifier : Boolean            := False;
   begin
      Debug (Semenatics_Debug, "begin Variable");
      The_Type                := null;
      The_Variable.The_Result := null;

      -- look up identifier (identifier).

      if Scope_Package.Is_Identifier
        (The_Variable.The_Identifier.The_String)
      then
         The_Identifier :=
           Scope_Package.Look_Up (The_Variable.The_Identifier.The_String);

         The_Variable.The_Identifier.The_Pointer := The_Identifier;
      else
         Scope_Package.Enter
           (new Variable_Identifier'
              (The_String  => The_Variable.The_Identifier.The_String,
               The_Type    => null,
               The_Value   => 0,
               The_Address => 0));
         Semenatics_Error (Position_Of (The_Variable), "Undefined identifier (1).");
         Undefined_Identifier := True;
      end if;

      if The_Variable.The_Expression = null then
         if not Undefined_Identifier then
            if Is_Typed (The_Identifier) then
               if Typed_Identifier (The_Identifier.all).The_Type = null
                 or else not Is_Array
                   (Typed_Identifier (The_Identifier.all).The_Type)
               then
                  The_Type := Typed_Identifier (The_Identifier.all).The_Type;

                  The_Variable.The_Result :=
                    new Identifier_Operand'
                      (The_Type => The_Type, The_Identifier => The_Identifier);

                  Debug (Semenatics_Debug, "Identifier_Operand");
               else
                  Semenatics_Error
                    (Position_Of (The_Variable),
                     "Expected scalar variable (1).");
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Variable),
                  "Expected variable or constant variable (1).");
            end if;
         end if;
      else
         Parse (The_Variable.The_Expression);

         if The_Variable.The_Expression.The_Result /= null and
           not Undefined_Identifier
         then

            if Is_Variable (The_Identifier) or
              Is_Parameter (The_Identifier)
            then

               The_Type := Typed_Identifier (The_Identifier.all).The_Type;

               if The_Type /= null then
                  if Is_Array (The_Type) then
                     if Is_Compatiable
                       (The_Variable.The_Expression.The_Result.The_Type,
                        Array_Type (The_Type.all).The_Index)
                     then
                        if Is_Constant
                          (The_Variable.The_Expression.The_Result)
                        then
                           if Is_Within
                             (Constant_Operand
                                (The_Variable.The_Expression.The_Result.all)
                              .The_Value,
                              The_Type)
                           then

                              The_Variable.The_Result :=
                                new Array_Operand'
                                  (The_Type =>
                                     Array_Type (The_Type.all).The_Element,
                                   The_Identifier => The_Identifier,
                                   The_Index      =>
                                     Copy
                                       (The_Variable.The_Expression
                                        .The_Result));
                              Debug (Semenatics_Debug, "Array_Operand");
                           else
                              Semenatics_Error
                                (Position_Of (The_Variable.The_Expression),
                                 "Expression not within array index type (1).");
                           end if;
                        else
                           The_Variable.The_Result :=
                             new Array_Operand'
                               (The_Type =>
                                  Array_Type (The_Type.all).The_Element,
                                The_Identifier => The_Identifier,
                                The_Index      =>
                                  Copy
                                    (The_Variable.The_Expression.The_Result));
                           Debug (Semenatics_Debug, "Array_Operand");
                        end if;
                     else
                        Semenatics_Error
                          (Position_Of (The_Variable.The_Expression),
                           "Expression not compatiable with array index (1).");
                     end if;
                  else
                     Semenatics_Error
                       (Position_Of (The_Variable.The_Expression),
                        "Expected array type variable or parameter (1).");
                  end if;
               end if;
            else
               Semenatics_Error
                 (Position_Of (The_Variable.The_Expression),
                  "Expected variable or parameter (1).");
            end if;
         end if;
      end if;

      -- returns
      --    Array_Operand
      --    Identifier_Operand
      --       Variable_Identifier
      --       Index_Identifier
      --       Parameter_Identifier
      --       Constant_Identifier

      Check
        (The_Variable.The_Result = null
         or else Is_Array (The_Variable.The_Result)
         or else Is_Variable (The_Variable.The_Result)
         or else
           (Is_Identifier (The_Variable.The_Result)
            and then
              (Is_Variable
                   (Identifier_Operand (The_Variable.The_Result.all).The_Identifier)
               or else Is_Index
                 (Identifier_Operand (The_Variable.The_Result.all).The_Identifier)
               or else Is_Parameter
                 (Identifier_Operand (The_Variable.The_Result.all).The_Identifier)
               or else Is_Constant
                 (Identifier_Operand (The_Variable.The_Result.all)
                  .The_Identifier))));

      Debug (Semenatics_Debug, Image_Of (The_Variable.The_Result));

      Debug (Semenatics_Debug, "end Variable");
   end Parse;

   procedure Parse (The_Statement : Null_Statement_Graph) is
   begin
      Debug (Semenatics_Debug, "begin Null_Statement");
      Debug (Semenatics_Debug, "end Null_Statement");
   end Parse;

   procedure Parse (The_Statement : If_Statement_Graph) is
      Is_Jump : Boolean := True;
   begin
      Debug (Semenatics_Debug, "begin If_Statement");

      Parse (The_Statement.The_Expression);

      if The_Statement.The_Expression.The_Result /= null then
         if not Is_Boolean
           (The_Statement.The_Expression.The_Result.The_Type)
         then
            Semenatics_Error
              (Position_Of (The_Statement.The_Expression),
               "Expected boolean condition (1).");
         end if;
      end if;

      Parse (The_Statement.The_Statements);

      if The_Statement.The_Alternates /= null then
         Parse (The_Statement.The_Alternates);
      end if;

      Debug (Semenatics_Debug, "end If_Statement");
   end Parse;

   procedure Parse (The_Statement : For_Statement_Graph) is
      The_Index      : Type_Pointer := null;
      The_Identifier : Identifier_Pointer;
   begin
      Debug (Semenatics_Debug, "begin For_Statement");

      -- look up index type.

      if Scope_Package.Is_Identifier
        (The_Statement.The_Definition.The_String)
      then
         The_Identifier :=
           Scope_Package.Look_Up (The_Statement.The_Definition.The_String);

         if Is_Type (The_Identifier) then
            The_Index := Type_Identifier (The_Identifier.all).The_Type;

            The_Statement.The_Definition.The_Pointer := The_Identifier;
         else
            Semenatics_Error
              (The_Statement.The_Definition.The_Position,
               "Expected type identifier (6).");
         end if;
      else
         Semenatics_Error
           (The_Statement.The_Definition.The_Position,
            "Undefined type identifier (6).");
      end if;

      -- first and last operands.

      Parse (The_Statement.The_First);
      Parse (The_Statement.The_Last);

      if The_Statement.The_First.The_Result /= null then
         if Is_Compatiable
           (The_Index,
            The_Statement.The_First.The_Result.The_Type)
         then
            if Operand_Package.Is_Constant
              (The_Statement.The_First.The_Result)
            then
               if not Is_Within
                 (Constant_Operand (The_Statement.The_First.The_Result.all)
                  .The_Value,
                  The_Index)
               then
                  Semenatics_Error
                    (Position_Of (The_Statement.The_First),
                     "Expression value not within index constraint (1).");
               end if;
            end if;
         else
            Semenatics_Error
              (Position_Of (The_Statement.The_First),
               "Expression not compatiable with index (1).");
         end if;
      end if;

      if The_Statement.The_Last.The_Result /= null then
         if Is_Compatiable
           (The_Index,
            The_Statement.The_Last.The_Result.The_Type)
         then
            if Operand_Package.Is_Constant
              (The_Statement.The_Last.The_Result)
            then
               if not Is_Within
                 (Constant_Operand (The_Statement.The_Last.The_Result.all)
                  .The_Value,
                  The_Index)
               then
                  Semenatics_Error
                    (Position_Of (The_Statement.The_Last),
                     "Expression value not within index constraint (2).");
               end if;
            end if;
         else
            Semenatics_Error
              (Position_Of (The_Statement.The_Last),
               "Expression not compatiable with index (2).");
         end if;
      end if;

      Scope_Package.Open;

      The_Identifier :=
        new Index_Identifier'
          (The_String  => The_Statement.The_Index.The_String,
           The_Type    => The_Index,
           The_Address => 0);

      The_Statement.The_Index.The_Pointer := The_Identifier;

      Scope_Package.Enter (The_Identifier);

      Parse (The_Statement.The_Statements);

      Scope_Package.Close;

      Debug (Semenatics_Debug, "end For_Statement");
   end Parse;

   procedure Parse (The_Expression : Expression_Graph) is
   begin
      Debug (Semenatics_Debug, "begin Expression");

      if The_Expression /= null then
         if The_Expression.all in Unary_Expression_Node then
            Parse (Unary_Expression_Graph (The_Expression));

         elsif The_Expression.all in Binary_Expression_Node then
            Parse (Binary_Expression_Graph (The_Expression));

         elsif The_Expression.all in Variable_Expression_Node then
            Parse (Variable_Expression_Graph (The_Expression));

         elsif The_Expression.all in Integer_Expression_Node then
            Parse (Integer_Expression_Graph (The_Expression));

         elsif The_Expression.all in Attribute_Expression_Node then
            Parse (Attribute_Expression_Graph (The_Expression));

         else
            raise Critical_Error;
         end if;
      end if;

      Debug (Semenatics_Debug, "end Expression");
   end Parse;

   procedure Parse (The_Expression : Unary_Expression_Graph) is
      The_Value : SYSInteger;
      The_Type  : Type_Pointer;
   begin
      Debug (Semenatics_Debug, "begin Unary_Expression");
      The_Expression.The_Result := null;

      Parse (The_Expression.The_Right);

      if The_Expression.The_Right.The_Result /= null then
         case The_Expression.The_Operator is
            when Not_Symbol =>
               if Is_Boolean (The_Expression.The_Right.The_Result.The_Type) then

                  if Is_Constant (The_Expression.The_Right.The_Result) then
                     The_Value :=
                       Constant_Operation
                         (The_Expression.The_Operator,
                          Constant_Operand
                            (The_Expression.The_Right.The_Result.all)
                          .The_Value);

                     if Is_Within
                       (The_Value,
                        The_Expression.The_Right.The_Result.The_Type)
                     then
                        The_Expression.The_Result :=
                          new Constant_Operand'
                            (The_Type =>
                               The_Expression.The_Right.The_Result.The_Type,
                             The_Value => The_Value);
                     else
                        Semenatics_Error
                          (Position_Of (The_Expression.The_Right),
                           "Expression not within type (S2).");
                     end if;

                  else
                     The_Expression.The_Result :=
                       new Variable_Operand'
                         (The_Type =>
                            The_Expression.The_Right.The_Result.The_Type);
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Expression.The_Right),
                     "Expression not compatiable with boolean operator (S1).");
               end if;

            when Minus_Symbol =>
               if Is_Integer (The_Expression.The_Right.The_Result.The_Type) then
                  if Is_Constant (The_Expression.The_Right.The_Result) then
                     The_Value :=
                       Constant_Operation
                         (The_Expression.The_Operator,
                          Constant_Operand
                            (The_Expression.The_Right.The_Result.all)
                          .The_Value);

                     The_Type := The_Expression.The_Right.The_Result.The_Type;
                     if The_Type /= null and then Is_Modular (The_Type) then
                        The_Value :=
                          The_Value mod Modular_Type (The_Type.all).The_Modulas;
                     end if;

                     if Is_Within
                       (The_Value,
                        The_Expression.The_Right.The_Result.The_Type)
                     then
                        The_Expression.The_Result :=
                          new Constant_Operand'
                            (The_Type =>
                               The_Expression.The_Right.The_Result.The_Type,
                             The_Value => The_Value);
                     else
                        Semenatics_Error
                          (Position_Of (The_Expression.The_Right),
                           "Expression not within type (S3).");
                     end if;

                  else
                     The_Expression.The_Result :=
                       new Variable_Operand'
                         (The_Type =>
                            The_Expression.The_Right.The_Result.The_Type);
                  end if;

               else
                  Semenatics_Error
                    (Position_Of (The_Expression.The_Right),
                     "Expression not compatiable with integer operator (S1).");
               end if;
            when others =>
               raise Critical_Error;
         end case;
      end if;

      -- returns
      --     Variable_Operand
      --     Constant_Operand

      Check
        (The_Expression.The_Result = null
         or else Is_Variable (The_Expression.The_Result)
         or else Is_Constant (The_Expression.The_Result));

      Debug (Semenatics_Debug, "end Unary_Expression");
   end Parse;

   procedure Parse (The_Expression : Binary_Expression_Graph) is
      The_Type  : Type_Pointer;
      The_Value : SYSInteger;
   begin
      Debug (Semenatics_Debug, "begin Binary_Expression");
      The_Expression.The_Result := null;

      Parse (The_Expression.The_Left);
      Parse (The_Expression.The_Right);

      if The_Expression.The_Right.The_Result /= null and
        The_Expression.The_Left.The_Result /= null
      then
         case The_Expression.The_Operator is

            when Equal_Symbol           |
                 Not_Equal_Symbol          |
                 Less_Than_Symbol          |
                 Less_Than_Equal_Symbol    |
                 Greater_Than_Symbol       |
                 Greater_Than_Equal_Symbol =>

               if Is_Compatiable
                 (The_Expression.The_Left.The_Result.The_Type,
                  The_Expression.The_Right.The_Result.The_Type)
               then

                  if Is_Constant (The_Expression.The_Left.The_Result) and
                    Is_Constant (The_Expression.The_Right.The_Result)
                  then

                     The_Value :=
                       Constant_Operation
                         (The_Expression.The_Operator,
                          Constant_Operand
                            (The_Expression.The_Left.The_Result.all)
                          .The_Value,
                          Constant_Operand
                            (The_Expression.The_Right.The_Result.all)
                          .The_Value);

                     The_Expression.The_Result :=
                       new Constant_Operand'
                         (The_Type => Boolean_Type, The_Value => The_Value);
                  else
                     The_Expression.The_Result :=
                       new Variable_Operand'(The_Type => Boolean_Type);
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Expression.The_Right),
                     "Expressions not compatiable (1).");
               end if;

            when And_Symbol | Or_Symbol | Xor_Symbol =>

               if Is_Boolean (The_Expression.The_Left.The_Result.The_Type) and
                 Is_Boolean (The_Expression.The_Right.The_Result.The_Type)
               then

                  The_Type :=
                    Best_Of
                      (The_Expression.The_Left.The_Result.The_Type,
                       The_Expression.The_Right.The_Result.The_Type);

                  if Is_Constant (The_Expression.The_Left.The_Result) and
                    Is_Constant (The_Expression.The_Right.The_Result)
                  then

                     The_Value :=
                       Constant_Operation
                         (The_Expression.The_Operator,
                          Constant_Operand
                            (The_Expression.The_Left.The_Result.all)
                          .The_Value,
                          Constant_Operand
                            (The_Expression.The_Right.The_Result.all)
                          .The_Value);

                     if Is_Within (The_Value, The_Type) then

                        The_Expression.The_Result :=
                          new Constant_Operand'
                            (The_Type => The_Type, The_Value => The_Value);
                     else
                        Semenatics_Error
                          (Position_Of (The_Expression.The_Right),
                           "Expression not within type (4).");
                     end if;
                  else
                     The_Expression.The_Result :=
                       new Variable_Operand'(The_Type => The_Type);
                  end if;

               else
                  Semenatics_Error
                    (Position_Of (The_Expression.The_Right),
                     "Operands not compatiable with boolean operator (1).");
               end if;

            when Plus_Symbol |
                 Minus_Symbol   |
                 Times_Symbol   |
                 Divide_Symbol  |
                 Rem_Symbol     |
                 Mod_Symbol     =>
               if Is_Compatiable
                 (The_Expression.The_Left.The_Result.The_Type,
                  The_Expression.The_Right.The_Result.The_Type)
               then

                  if Is_Integer
                    (The_Expression.The_Right.The_Result.The_Type)
                  then

                     The_Type :=
                       Best_Of
                         (The_Expression.The_Left.The_Result.The_Type,
                          The_Expression.The_Right.The_Result.The_Type);

                     if Is_Constant (The_Expression.The_Left.The_Result) and
                       Is_Constant (The_Expression.The_Right.The_Result)
                     then

                        The_Value :=
                          Constant_Operation
                            (The_Expression.The_Operator,
                             Constant_Operand
                               (The_Expression.The_Left.The_Result.all)
                             .The_Value,
                             Constant_Operand
                               (The_Expression.The_Right.The_Result.all)
                             .The_Value);

                        if The_Type /= null and then Is_Modular (The_Type) then
                           The_Value :=
                             The_Value mod
                               Modular_Type (The_Type.all).The_Modulas;
                        end if;

                        if Is_Within (The_Value, The_Type) then

                           The_Expression.The_Result :=
                             new Constant_Operand'
                               (The_Type => The_Type, The_Value => The_Value);
                        else
                           Semenatics_Error
                             (Position_Of (The_Expression.The_Right),
                              "Expression not within type (5).");
                        end if;

                     else
                        The_Expression.The_Result :=
                          new Variable_Operand'(The_Type => The_Type);
                     end if;

                  else
                     Semenatics_Error
                       (Position_Of (The_Expression.The_Right),
                        "Operand not compatiable with integer operator (1).");
                  end if;
               else
                  Semenatics_Error
                    (Position_Of (The_Expression.The_Right),
                     "Operands are not compatiable (1).");
               end if;
            when others =>
               raise Critical_Error;
         end case;
      end if;

      -- returns
      --    Variable_Operand
      --    Constant_Operand

      Check
        (The_Expression.The_Result = null
         or else Is_Variable (The_Expression.The_Result)
         or else Is_Constant (The_Expression.The_Result));

      Debug (Semenatics_Debug, Image_Of (The_Expression.The_Result));

      Debug (Semenatics_Debug, "end Binary_Expression");
   end Parse;

   procedure Parse (The_Expression : Variable_Expression_Graph) is
      The_Identifier : Identifier_Pointer;
   begin
      Debug (Semenatics_Debug, "begin Variable_Expression");
      The_Expression.The_Result := null;

      Parse (The_Expression.The_Variable);

      if The_Expression.The_Variable.The_Result /= null then
         The_Identifier :=
           Identifier_Operand (The_Expression.The_Variable.The_Result.all)
           .The_Identifier;

         if Is_Variable (The_Identifier) then
            The_Expression.The_Result :=
              Copy (The_Expression.The_Variable.The_Result);
            Debug (Semenatics_Debug, "Variable");

         elsif Is_Index (The_Identifier) then
            The_Expression.The_Result :=
              Copy (The_Expression.The_Variable.The_Result);
            Debug (Semenatics_Debug, "Index");

         elsif Is_Parameter (The_Identifier) then
            The_Expression.The_Result :=
              Copy (The_Expression.The_Variable.The_Result);
            Debug (Semenatics_Debug, "Parameter");

         elsif Is_Constant (The_Identifier) then
            The_Expression.The_Result :=
              new Constant_Operand'
                (The_Type  => The_Expression.The_Variable.The_Result.The_Type,
                 The_Value =>
                   Constant_Identifier (The_Identifier.all).The_Value);

            Debug (Semenatics_Debug, "Constant");
         else
            raise Critical_Error;
         end if;
      end if;

      -- returns
      --     Variable_Operand
      --     Identifier_Operand
      --         Variable_Identifier
      --         Index_Identifier
      --         Parameter_Identifier
      --     Constant_Operand

      Check
        (The_Expression.The_Result = null
         or else Is_Variable (The_Expression.The_Result)
         or else
           (Is_Identifier (The_Expression.The_Result)
            and then
              (Is_Variable
                   (Identifier_Operand (The_Expression.The_Result.all).The_Identifier)
               or else Is_Index
                 (Identifier_Operand (The_Expression.The_Result.all).The_Identifier)
               or else Is_Parameter
                 (Identifier_Operand (The_Expression.The_Result.all)
                  .The_Identifier)))
         or else Is_Constant (The_Expression.The_Result));

      Debug (Semenatics_Debug, "end Variable_Expression");
   end Parse;

   procedure Parse (The_Expression : Attribute_Expression_Graph) is
      The_Type             : Type_Pointer;
      The_Identifier       : Identifier_Pointer;
      Undefined_Identifier : Boolean := False;
   begin
      Debug (Semenatics_Debug, "begin Attribute_Expression");

      -- look up identifier (identifier).

      if Scope_Package.Is_Identifier
        (The_Expression.The_Identifier.The_String)
      then
         The_Identifier :=
           Scope_Package.Look_Up (The_Expression.The_Identifier.The_String);

         The_Expression.The_Identifier.The_Pointer := The_Identifier;
      else
         Scope_Package.Enter
           (new Variable_Identifier'
              (The_String  => The_Expression.The_Identifier.The_String,
               The_Type    => null,
               The_Value   => 0,
               The_Address => 0));
         Semenatics_Error
           (Position_Of (The_Expression),
            "Undefined identifier for attribute (1).");
         Undefined_Identifier := True;
      end if;

      if not Undefined_Identifier then
         if Is_Type (The_Expression.The_Identifier.The_Pointer) or else
           Is_Variable (The_Expression.The_Identifier.The_Pointer) or else
           Is_Parameter (The_Expression.The_Identifier.The_Pointer) or else
           Is_Index (The_Expression.The_Identifier.The_Pointer) then

            if Is_Type (The_Expression.The_Identifier.The_Pointer) then
               The_Type := Type_Identifier
                 (The_Expression.The_Identifier.The_Pointer.all).The_Type;
            else
               The_Type := Typed_Identifier
                 (The_Expression.The_Identifier.The_Pointer.all).The_Type;
            end if;

            if To_String (The_Expression.The_String) = "LENGTH" and then
              not Is_Index (The_Expression.The_Identifier.The_Pointer) then
               if Is_Array (The_Type) then
                  The_Expression.The_Result :=
                    new Constant_Operand'
                      (The_Type  => Universal_Integer,
                       The_Value => Last_Of (The_Type) - First_Of (The_Type) + 1);
               else
                  Semenatics_Error
                    (Position_Of (The_Expression),
                     "Length attribute requires array type (1).");
               end if;

            elsif To_String (The_Expression.The_String) = "FIRST" then
               The_Expression.The_Result :=
                 new Constant_Operand'
                   (The_Type  => Universal_Integer,
                    The_Value => First_Of (The_Type));

            elsif To_String (The_Expression.The_String) = "LAST" then
               The_Expression.The_Result :=
                 new Constant_Operand'
                   (The_Type  => Universal_Integer,
                    The_Value => Last_Of (The_Type));

            elsif To_String (The_Expression.The_String) = "SIZE" then
               The_Expression.The_Result :=
                 new Constant_Operand'
                   (The_Type  => Universal_Integer,
                    The_Value => Size_Of (The_Type));
            else
               Semenatics_Error
                 (Position_Of (The_Expression),
                  "Expected attribute (1).");
            end if;

         else
            raise Critical_Error;
         end if;
      end if;

      -- returns
      --       Constant_Operand

      Check
        (The_Expression.The_Result = null
         or else Is_Constant (The_Expression.The_Result));

      Debug (Semenatics_Debug, "Attribute ");
      Debug (Semenatics_Debug, Image_Of (The_Expression.The_Result));

      Debug (Semenatics_Debug, "end Attribute_Expression");
   end Parse;

   procedure Parse (The_Expression : Integer_Expression_Graph) is
      The_Value : SYSInteger  := 0;
      The_Last  : SYSPositive := 1;
   begin
      Debug (Semenatics_Debug, "begin Integer_Expression");

      Get (To_String (The_Expression.The_String), The_Value, The_Last);

      The_Expression.The_Result :=
        new Constant_Operand'
          (The_Type => Universal_Integer, The_Value => The_Value);

      Debug (Semenatics_Debug, "Integer ");
      Debug (Semenatics_Debug, Image_Of (The_Expression.The_Result));

      Debug (Semenatics_Debug, "end Integer_Expression");
   end Parse;

   -- Parse a Boolean Compiler (BC) graph and check semenatics.

   procedure Parse (The_Unit : Compilation_Unit_Graph) is
   begin
      Debug (Semenatics_Debug, "begin Compilation_Unit");
      -- open initial scope and enter default types and constants.

      Parse (The_Unit.The_Package);

      Debug (Semenatics_Debug, "end Compilation_Unit");
   end Parse;

begin
   Debug (Debug_Initialization, "Semenatics_Package");
end Semantics_Package;

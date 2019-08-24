-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Containers.Vectors;
--
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers;      use Ada.Containers;
--
with System_Package;     use System_Package;
with Type_Package;       use Type_Package;
with Identifier_Package; use Identifier_Package;
--
with Identifier_Package.Image_Package; use Identifier_Package.Image_Package;
with Operand_Package.Image_Package;    use Operand_Package.Image_Package;
--

-- Dump a Boolean Compiler (BC) graph to standard output.

procedure Graph_Package.Dump
  (The_Unit      : Compilation_Unit_Graph;
   Semantic_Dump : Boolean := False;
   Full_Dump     : Boolean := False;
   XML_Format    : Boolean := False)
is

   package Vector_Package is new Ada.Containers.Vectors
     (SYSPositive,
      Graph_Pointer,
      "=");
   use Vector_Package;

   The_Graphs : Vector;

   function Lookup (The_Graph : Graph_Pointer) return SYSNatural is
      The_Index : Extended_Index;
   begin
      if The_Graph = null then
         return 0;
      end if;

      The_Index := Find_Index (The_Graphs, The_Graph);
      if The_Index = No_Index then
         Append (The_Graphs, The_Graph);
         return SYSNatural (Last_Index (The_Graphs));
      else
         return SYSNatural (The_Index);
      end if;
   end Lookup;

   procedure Put_Header (The_Graph : Graph_Pointer; The_Node : String) is
   begin
      if XML_Format then
         Put ("<ln>");
      end if;

      Put ("[");
      Put (Lookup (The_Graph), 5);
      Put ("] : " & The_Node);

      if XML_Format then
         Put ("</ln>");
      end if;

      New_Line;
   end Put_Header;

   procedure Put_Footer (The_Node : String) is
   begin
      if XML_Format then
         Put ("<ln>");
      end if;

      Put ("          end " & The_Node);

      if XML_Format then
         Put ("</ln>");
      end if;
      New_Line;

      if XML_Format then
         Put ("<ln></ln>");
      end if;
      New_Line;
   end Put_Footer;

   procedure Put_Item (The_Item : String) is
   begin
      if XML_Format then
         Put ("<ln>");
      end if;

      Put ("            " & The_Item);

      if XML_Format then
         Put ("</ln>");
      end if;

      New_Line;
   end Put_Item;

   procedure Put_Item (The_Title : String; The_Index : SYSNatural) is
   begin
      if The_Index /= 0 then
         Put_Item (The_Title & " [" & SYSNatural'Image (The_Index) & " ]");
      end if;
   end Put_Item;

   -- BC Language constructs.

   procedure Dump (The_Package : Package_Body_Graph);
   procedure Dump (The_Procedure : Procedure_Body_Graph);
   procedure Dump (The_Parameters : Parameter_Graph);
   procedure Dump (The_Declarations : Declaration_Graph);
   procedure Dump (The_Declaration : Type_Declaration_Graph);
   procedure Dump (The_Declaration : Identifier_Declaration_Graph);
   procedure Dump (The_Definition : Definition_Graph);
   procedure Dump (The_Definition : Range_Definition_Graph);
   procedure Dump (The_Definition : Mod_Definition_Graph);
   procedure Dump (The_Definition : Array_Definition_Graph);
   procedure Dump (The_Statements : Statement_Graph);
   procedure Dump (The_Statement : Assignment_Statement_Graph);
   procedure Dump (The_Variable : Variable_Graph);
   procedure Dump (The_Statement : Null_Statement_Graph);
   procedure Dump (The_Statement : If_Statement_Graph);
   procedure Dump (The_Statement : For_Statement_Graph);
   procedure Dump (The_Expression : Expression_Graph);
   procedure Dump (The_Expression : Unary_Expression_Graph);
   procedure Dump (The_Expression : Binary_Expression_Graph);
   procedure Dump (The_Expression : Variable_Expression_Graph);
   procedure Dump (The_Expression : Attribute_Expression_Graph);
   procedure Dump (The_Expression : Integer_Expression_Graph);
   procedure Dump (The_Expression : Operand_Expression_Graph);

   procedure Dump (The_Package : Package_Body_Graph) is
   begin
      if The_Package /= null then
         Put_Header (Graph_Pointer (The_Package), "Package_Body");

         Put_Item
           ("The_Name         " & To_String (The_Package.The_Name.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Package.The_Name.The_Pointer));
         end if;

         Put_Item
           ("The_Declarations",
            Lookup (Graph_Pointer (The_Package.The_Declarations)));
         Put_Item
           ("The_Procedure   ",
            Lookup (Graph_Pointer (The_Package.The_Procedure)));

         if Semantic_Dump then
            Put_Item
              ("The_Identifier   " &
                 To_String (The_Package.The_Identifier.The_String));

            if Full_Dump then
               Put_Item (Image_Of (The_Package.The_Identifier.The_Pointer));
            end if;
         end if;

         Put_Footer ("Package_Body");

         Dump (The_Package.The_Declarations);
         Dump (The_Package.The_Procedure);
      end if;
   end Dump;

   procedure Dump (The_Procedure : Procedure_Body_Graph) is
   begin
      if The_Procedure /= null then
         Put_Header (Graph_Pointer (The_Procedure), "Procedure_Body");

         Put_Item
           ("The_Name         " &
              To_String (The_Procedure.The_Name.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Procedure.The_Name.The_Pointer));
         end if;

         Put_Item
           ("The_Parameters  ",
            Lookup (Graph_Pointer (The_Procedure.The_Parameters)));
         Put_Item
           ("The_Statements  ",
            Lookup (Graph_Pointer (The_Procedure.The_Statements)));

         if Semantic_Dump then
            Put_Item
              ("The_Identifier  " &
                 To_String (The_Procedure.The_Identifier.The_String));

            if Full_Dump then
               Put_Item (Image_Of (The_Procedure.The_Identifier.The_Pointer));
            end if;
         end if;

         Put_Footer ("Procedure");

         Dump (The_Procedure.The_Parameters);
         Dump (The_Procedure.The_Statements);
      end if;
   end Dump;

   procedure Dump (The_Parameters : Parameter_Graph) is
      The_Parameter : Parameter_Graph := The_Parameters;

      procedure Dump_Parameter (The_Parameter : Parameter_Graph) is
      begin
         Put_Header (Graph_Pointer (The_Parameter), "Parameter");

         Put_Item
           ("The_Name        " &
              To_String (The_Parameter.The_Identifier.The_String));
         if Full_Dump then
            Put_Item (Image_Of (The_Parameter.The_Identifier.The_Pointer));
         end if;

         Put_Item ("In              " & Boolean'Image (The_Parameter.Is_In));
         Put_Item ("Out             " & Boolean'Image (The_Parameter.Is_Out));

         Put_Item
           ("The_Definition  " &
              To_String (The_Parameter.The_Definition.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Parameter.The_Definition.The_Pointer));
         end if;

         Put_Footer ("Parameter");
      end Dump_Parameter;

   begin
      if The_Parameter /= null then
         Dump_Parameter (The_Parameter);
         The_Parameter := The_Parameter.The_Next;

         while The_Parameter /= null loop
            Dump_Parameter (The_Parameter);
            The_Parameter := The_Parameter.The_Next;
         end loop;
      end if;
   end Dump;

   procedure Dump (The_Declarations : Declaration_Graph) is
      The_Declaration : Declaration_Graph := The_Declarations;
   begin
      while The_Declaration /= null loop

         if The_Declaration.all in Type_Declaration_Node then
            Dump (Type_Declaration_Graph (The_Declaration));

         elsif The_Declaration.all in Identifier_Declaration_Node then
            Dump (Identifier_Declaration_Graph (The_Declaration));
         else
            raise Critical_Error;
         end if;

         The_Declaration := The_Declaration.The_Next;
      end loop;
   end Dump;

   procedure Dump (The_Declaration : Type_Declaration_Graph) is
   begin
      if The_Declaration /= null then
         Put_Header (Graph_Pointer (The_Declaration), "Type_Declaration");

         Put_Item
           ("The_Identifier  " &
              To_String (The_Declaration.The_Identifier.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Declaration.The_Identifier.The_Pointer));
         end if;

         Put_Item
           ("The_Definition ",
            Lookup (Graph_Pointer (The_Declaration.The_Definition)));
         Put_Item
           ("The_Next       ",
            Lookup (Graph_Pointer (The_Declaration.The_Next)));

         Put_Footer ("Type_Declaration");

         Dump (The_Declaration.The_Definition);
      end if;
   end Dump;

   procedure Dump (The_Declaration : Identifier_Declaration_Graph) is
   begin
      if The_Declaration /= null then
         Put_Header (Graph_Pointer (The_Declaration), "Identifier_Declaration");

         Put_Item
           ("The_Identifier " &
              To_String (The_Declaration.The_Identifier.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Declaration.The_Identifier.The_Pointer));
         end if;

         Put_Item
           ("Constant       " & Boolean'Image (The_Declaration.Is_Constant));
         Put_Item
           ("The_Definition",
            Lookup (Graph_Pointer (The_Declaration.The_Definition)));
         Put_Item
           ("The_Expresson ",
            Lookup (Graph_Pointer (The_Declaration.The_Expression)));
         Put_Item
           ("The_Next      ",
            Lookup (Graph_Pointer (The_Declaration.The_Next)));

         Put_Footer ("Identifier_Declaration");

         Dump (The_Declaration.The_Expression);
      end if;
   end Dump;

   procedure Dump (The_Definition : Definition_Graph) is
   begin
      if The_Definition /= null then

         if The_Definition.all in Array_Definition_Node'Class then
            Dump (Array_Definition_Graph (The_Definition));

         elsif The_Definition.all in Range_Definition_Node'Class then
            Dump (Range_Definition_Graph (The_Definition));

         elsif The_Definition.all in Mod_Definition_Node'Class then
            Dump (Mod_Definition_Graph (The_Definition));
         else
            raise Critical_Error;
         end if;
      end if;
   end Dump;

   procedure Dump (The_Definition : Range_Definition_Graph) is
   begin
      if The_Definition /= null then
         Put_Header (Graph_Pointer (The_Definition), "Range_Definition");

         Put_Item
           ("The_Identifier " &
              To_String (The_Definition.The_Identifier.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Definition.The_Identifier.The_Pointer));
         end if;

         Put_Item
           ("The_First     ",
            Lookup (Graph_Pointer (The_Definition.The_First)));
         Put_Item
           ("The_Last      ",
            Lookup (Graph_Pointer (The_Definition.The_Last)));

         Put_Footer ("Range_Definition");

         Dump (The_Definition.The_First);
         Dump (The_Definition.The_Last);
      end if;
   end Dump;

   procedure Dump (The_Definition : Mod_Definition_Graph) is
   begin
      if The_Definition /= null then
         Put_Header (Graph_Pointer (The_Definition), "Mod_Definition");

         Put_Item
           ("The_Expression ",
            Lookup (Graph_Pointer (The_Definition.The_Expression)));

         Put_Footer ("Mod_Definition");

         Dump (The_Definition.The_Expression);
      end if;
   end Dump;

   procedure Dump (The_Definition : Array_Definition_Graph) is
   begin
      if The_Definition /= null then
         Put_Header (Graph_Pointer (The_Definition), "Array_Definition");

         if The_Definition.The_Index /= null then
            Put_Item
              ("The_Index   " &
                 To_String (The_Definition.The_Index.The_String));

            if Full_Dump then
               Put_Item (Image_Of (The_Definition.The_Index.The_Pointer));
            end if;
         end if;

         Put_Item
           ("The_First  ",
            Lookup (Graph_Pointer (The_Definition.The_First)));
         Put_Item
           ("The_Last   ",
            Lookup (Graph_Pointer (The_Definition.The_Last)));

         Put_Item
           ("The_Element  " &
              To_String (The_Definition.The_Element.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Definition.The_Element.The_Pointer));
         end if;

         Put_Footer ("Array_Definition");

         Dump (The_Definition.The_First);
         Dump (The_Definition.The_Last);
      end if;
   end Dump;

   procedure Dump (The_Statements : Statement_Graph) is
      The_Statement : Statement_Graph := The_Statements;
   begin
      while The_Statement /= null loop

         if The_Statement.all in Assignment_Statement_Node'Class then
            Dump (Assignment_Statement_Graph (The_Statement));

         elsif The_Statement.all in Null_Statement_Node'Class then
            Dump (Null_Statement_Graph (The_Statement));

         elsif The_Statement.all in If_Statement_Node'Class then
            Dump (If_Statement_Graph (The_Statement));

         elsif The_Statement.all in For_Statement_Node'Class then
            Dump (For_Statement_Graph (The_Statement));

         else
            raise Critical_Error;
         end if;

         The_Statement := The_Statement.The_Next;
      end loop;
   end Dump;

   procedure Dump (The_Statement : Assignment_Statement_Graph) is
   begin
      if The_Statement /= null then
         Put_Header (Graph_Pointer (The_Statement), "Assignment_Statement");

         Put_Item
           ("The_Variable  ",
            Lookup (Graph_Pointer (The_Statement.The_Variable)));
         Put_Item
           ("The_Expression",
            Lookup (Graph_Pointer (The_Statement.The_Expression)));
         Put_Item
           ("The_Next      ",
            Lookup (Graph_Pointer (The_Statement.The_Next)));

         Put_Footer ("Assignment_Statement");

         Dump (The_Statement.The_Variable);
         Dump (The_Statement.The_Expression);
      end if;
   end Dump;

   procedure Dump (The_Variable : Variable_Graph) is
   begin
      if The_Variable /= null then
         Put_Header (Graph_Pointer (The_Variable), "Variable");

         Put_Item
           ("The_Identifier  " &
              To_String (The_Variable.The_Identifier.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Variable.The_Identifier.The_Pointer));
         end if;

         Put_Item
           ("The_Expression ",
            Lookup (Graph_Pointer (The_Variable.The_Expression)));

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Variable.The_Result));
         end if;

         Put_Footer ("Variable");

         Dump (The_Variable.The_Expression);
      end if;
   end Dump;

   procedure Dump (The_Statement : Null_Statement_Graph) is
   begin
      if The_Statement /= null then
         Put_Header (Graph_Pointer (The_Statement), "Null_Statement");
         Put_Item
           ("The_Next ",
            Lookup (Graph_Pointer (The_Statement.The_Next)));
         Put_Footer ("Null_Statement");
      end if;
   end Dump;

   procedure Dump (The_Statement : If_Statement_Graph) is
   begin
      if The_Statement /= null then
         Put_Header (Graph_Pointer (The_Statement), "If_Statement");

         Put_Item
           ("The_Expression ",
            Lookup (Graph_Pointer (The_Statement.The_Expression)));
         Put_Item
           ("The_Statements ",
            Lookup (Graph_Pointer (The_Statement.The_Statements)));
         Put_Item
           ("The_Alternates ",
            Lookup (Graph_Pointer (The_Statement.The_Alternates)));
         Put_Item
           ("The_Next       ",
            Lookup (Graph_Pointer (The_Statement.The_Next)));

         Put_Footer ("If_Statement");

         Dump (The_Statement.The_Expression);
         Dump (The_Statement.The_Statements);
         Dump (The_Statement.The_Alternates);
      end if;
   end Dump;

   procedure Dump (The_Statement : For_Statement_Graph) is
   begin
      if The_Statement /= null then
         Put_Header (Graph_Pointer (The_Statement), "For_Statement");

         Put_Item
           ("The_Index        " &
              To_String (The_Statement.The_Index.The_String));

         if Full_Dump then
            Put_Item (Image_Of (The_Statement.The_Index.The_Pointer));
         end if;

         if The_Statement.The_Definition /= null then
            Put_Item
              ("The_Definition   " &
                 To_String (The_Statement.The_Definition.The_String));

            if Full_Dump then
               Put_Item (Image_Of (The_Statement.The_Definition.The_Pointer));
            end if;
         end if;

         Put_Item
           ("Is_Reverse       " & Boolean'Image (The_Statement.Is_Reverse));
         Put_Item
           ("The_First       ",
            Lookup (Graph_Pointer (The_Statement.The_First)));
         Put_Item
           ("The_Last        ",
            Lookup (Graph_Pointer (The_Statement.The_Last)));
         Put_Item
           ("The_Statements  ",
            Lookup (Graph_Pointer (The_Statement.The_Statements)));
         Put_Item
           ("The_Next        ",
            Lookup (Graph_Pointer (The_Statement.The_Next)));

         Put_Footer ("For_Statement");

         Dump (The_Statement.The_First);
         Dump (The_Statement.The_Last);
         Dump (The_Statement.The_Statements);
      end if;
   end Dump;

   procedure Dump (The_Expression : Expression_Graph) is
   begin
      if The_Expression /= null then
         if The_Expression.all in Unary_Expression_Node'Class then
            Dump (Unary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Binary_Expression_Node'Class then
            Dump (Binary_Expression_Graph (The_Expression));
         elsif The_Expression.all in Variable_Expression_Node'Class then
            Dump (Variable_Expression_Graph (The_Expression));
         elsif The_Expression.all in Attribute_Expression_Node'Class then
            Dump (Attribute_Expression_Graph (The_Expression));
         elsif The_Expression.all in Integer_Expression_Node'Class then
            Dump (Integer_Expression_Graph (The_Expression));
         elsif The_Expression.all in Operand_Expression_Node'Class then
            Dump (Operand_Expression_Graph (The_Expression));
         else
            raise Critical_Error;
         end if;
      end if;
   end Dump;

   procedure Dump (The_Expression : Unary_Expression_Graph) is
   begin
      if The_Expression /= null then
         Put_Header (Graph_Pointer (The_Expression), "Unary_Expression");

         Put_Item
           ("The_Operator   " & Symbol'Image (The_Expression.The_Operator));
         Put_Item
           ("The_Right     ",
            Lookup (Graph_Pointer (The_Expression.The_Right)));

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Expression.The_Result));
         end if;

         Put_Footer ("Unary_Expression");

         Dump (The_Expression.The_Right);
      end if;
   end Dump;

   procedure Dump (The_Expression : Binary_Expression_Graph) is

   begin
      if The_Expression /= null then
         Put_Header (Graph_Pointer (The_Expression), "Binary_Expression");

         Put_Item
           ("The_Operator   " & Symbol'Image (The_Expression.The_Operator));

         Put_Item
           ("The_Left      ",
            Lookup (Graph_Pointer (The_Expression.The_Left)));
         Put_Item
           ("The_Right     ",
            Lookup (Graph_Pointer (The_Expression.The_Right)));

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Expression.The_Result));
         end if;

         Put_Footer ("Binary_Expression");

         Dump (The_Expression.The_Left);
         Dump (The_Expression.The_Right);
      end if;
   end Dump;

   procedure Dump (The_Expression : Variable_Expression_Graph) is
   begin
      if The_Expression /= null then
         Put_Header (Graph_Pointer (The_Expression), "Variable_Expression");

         Put_Item
           ("The_Variable  ",
            Lookup (Graph_Pointer (The_Expression.The_Variable)));

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Expression.The_Result));
         end if;

         Put_Footer ("Variable_Expression");

         Dump (The_Expression.The_Variable);
      end if;
   end Dump;

   procedure Dump (The_Expression : Attribute_Expression_Graph) is
   begin
      if The_Expression /= null then
         Put_Header (Graph_Pointer (The_Expression), "Attribute_Expression");

         Put_Item
           ("The_Identifier " &
              To_String (The_Expression.The_Identifier.The_String));
         Put_Item
           ("The_Attribute  " &
              To_String (The_Expression.The_String));

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Expression.The_Result));
         end if;

         Put_Footer ("Attribute_Expression");
      end if;
   end Dump;

   procedure Dump (The_Expression : Integer_Expression_Graph) is
   begin
      if The_Expression /= null then
         Put_Header (Graph_Pointer (The_Expression), "Integer_Expression");
         Put_Item ("The_String     " & To_String (The_Expression.The_String));

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Expression.The_Result));
         end if;

         Put_Footer ("Integer_Expression");
      end if;
   end Dump;

   procedure Dump (The_Expression : Operand_Expression_Graph) is
   begin
      if The_Expression /= null then
         Put_Header (Graph_Pointer (The_Expression), "Operand_Expression");

         if Full_Dump then
            Put_Item ("The_Result " & Image_Of (The_Expression.The_Result));
         end if;

         Put_Footer ("Operand_Expression");
      end if;
   end Dump;

begin
   if XML_Format then
      Put_Line ("<graph>");
   end if;

   if The_Unit /= null then
      Put_Header (Graph_Pointer (The_Unit), "Compilation_Unit");

      Put_Item (" The_Unit", Lookup (Graph_Pointer (The_Unit.The_Package)));

      Put_Footer ("Compilation_Unit");

      Dump (The_Unit.The_Package);
   end if;

   if XML_Format then
      Put_Line ("</graph>");
   end if;

end Graph_Package.Dump;

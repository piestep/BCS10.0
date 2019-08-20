-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

--      -- BC Boolean Compiler Ada Test Generator.
--      -- Generated from 'TEST.BC', 10/12/2007
--
--      with Test;      use Test;
--      --
--      package body ARun_Package is
--
--              -- Execute ada code.
--
--              procedure Run
--              (The_Inputs     : Boolean_Array;                -- The input boolean vaules.
--              The_Outputs     : out Unbounded_String; -- The output values.
--              Boolean_Output  : Boolean)                              -- Boolean values output switch.
--              is
--
--                      The_Input       : Test.Value;
--                      The_Output      : Test.Value;
--
--              begin
--                      The_Input := Test.Value(Integer_At(0, Test.Value'Size, The_Inputs));
--
--                      Test.Run(The_Input, The_Output);
--
--                      if Boolean_Output then
--
--                              Append(The_Outputs, " ("&Image_Of(To_Integer_Boolean
--                                      (Integer(The_Output))(0..Test.Value'Size-1))&")");
--
--                      else
--
--                              Append(The_Outputs, " ("&Test.Value'Image(The_Output)&")");
--
--                      end if;
--              end Run;
--
--      end ARun_Package;

separate (Generate_Package.ACode)
procedure ARun_File
  (The_Name    : String;
   The_Package : Unbounded_String;
   The_Date    : String)
is

   The_File       : File_Type;
   The_Parameters : Parameter_Graph;
   The_Parameter  : Parameter_Graph;
   The_Index      : Natural;
   The_Type       : Type_Pointer;

   -- Return prameter graph of compilation unit.

   function Parameters_Of
     (The_Unit : Compilation_Unit_Graph) return Parameter_Graph
   is
   begin
      if The_Unit /= null
        and then The_Unit.The_Package /= null
        and then The_Unit.The_Package.The_Procedure /= null
      then
         return The_Unit.The_Package.The_Procedure.The_Parameters;
      end if;
      return null;
   end Parameters_Of;

   procedure Assignment_To_BC
     (The_Offset  : Natural;
      The_Package : String;
      The_Id      : String;
      The_Type    : Type_Pointer)
   is
   begin
      Put (The_File, Type_Of (The_Package, The_Id) & "( ");

      if Is_Boolean (The_Type) then
         Put (The_File, "The_Inputs(");
         Put (The_File, Integer'Image (The_Offset));
         Put (The_File, ")");

      elsif Is_Signed (The_Type) then
         Put (The_File, "Integer_At(");
         Put (The_File, Integer'Image (The_Offset));
         Put (The_File, ", " & Type_Of (The_Package, The_Id));
         Put (The_File, "'Size, The_Inputs)");

      else
         Put (The_File, "Modular_At(");
         Put (The_File, Integer'Image (The_Offset));
         Put (The_File, ", " & Type_Of (The_Package, The_Id));
         Put (The_File, "'Size, The_Inputs)");
      end if;

      Put (The_File, " )");
   end Assignment_To_BC;

   procedure Generate_Assignment_To_BC
     (The_Identifier : String;
      The_Offset     : Natural;
      The_Package    : String;
      The_Id         : String;
      The_Type       : Type_Pointer)
   is
      The_First   : Integer;
      The_Last    : Integer;
      The_Element : Type_Pointer;
   begin
      Put (The_File, "            ");
      Put (The_File, The_Identifier);
      Put (The_File, " := ");

      if Is_Scalar (The_Type) then
         Assignment_To_BC (The_Offset, The_Package, The_Id, The_Type);
         Put (The_File, ";");
      elsif Is_Array (The_Type) then
         New_Line (The_File);
         Put (The_File, "                ");
         Put (The_File, "(");

         The_First   := First_Of (The_Type);
         The_Last    := Last_Of (The_Type);
         The_Element := Array_Type (The_Type.all).The_Element;

         Assignment_To_BC
           (The_Offset,
            The_Package,
            To_String (Element (The_Map, The_Element)),
            The_Element);

         for Index in 1 .. The_Last - The_First loop
            Put (The_File, ", ");
            New_Line (The_File);
            Put (The_File, "                ");
            Assignment_To_BC
              (The_Offset + Index * Size_Of (The_Element),
               The_Package,
               To_String (Element (The_Map, The_Element)),
               The_Element);
         end loop;
         Put (The_File, " );");
      else
         raise Critical_Error;
      end if;

      New_Line (The_File);
   end Generate_Assignment_To_BC;

   procedure Assignment_To_Boolean_String
     (The_Identifier : String;
      The_Package    : String;
      The_Id         : String)
   is
   begin
      Put (The_File, "                ");
      Put (The_File, "Append(The_Outputs, ");
      Put (The_File, '"' & " (" & '"');

      Put (The_File, "&Image_Of(To_Integer_Boolean");

      New_Line (The_File);
      Put (The_File, "                    ");
      if The_Id = "BOOLEAN" then
         Put (The_File, "(BOOLEAN'Pos(" & The_Identifier & "))");
      else
         Put (The_File, "(Integer(" & The_Identifier & "))");
      end if;
      Put (The_File, "( 0 ..");
      Put (The_File, Type_Of (The_Package, The_Id));

      Put (The_File, "'Size-1))&");
      Put (The_File, '"' & ")" & '"');
      Put (The_File, ");");
      New_Line (The_File);
   end Assignment_To_Boolean_String;

   procedure Generate_Assignment_To_Boolean_String
     (The_Identifier : String;
      The_Offset     : Natural;
      The_Package    : String;
      The_Id         : String;
      The_Type       : Type_Pointer)
   is
      The_First   : Integer;
      The_Last    : Integer;
      The_Index   : Type_Pointer;
      The_Element : Type_Pointer;
   begin
      if Is_Scalar (The_Type) then
         Assignment_To_Boolean_String (The_Identifier, The_Package, The_Id);
      elsif Is_Array (The_Type) then
         The_First   := First_Of (The_Type);
         The_Last    := Last_Of (The_Type);
         The_Index   := Array_Type (The_Type.all).The_Index;
         The_Element := Array_Type (The_Type.all).The_Element;

         for Index in 0 .. The_Last - The_First loop
            Assignment_To_Boolean_String
              (The_Identifier &
               "(" &
               Type_Of
                 (The_Package,
                  To_String (Element (The_Map, The_Index))) &
               "(" &
               Integer'Image (The_First + Index) &
               "))",
               The_Package,
               To_String (Element (The_Map, The_Element)));
         end loop;
      else
         raise Critical_Error;
      end if;
   end Generate_Assignment_To_Boolean_String;

   procedure Assignment_To_String
     (The_Identifier : String;
      The_Size       : Natural;
      The_Package    : String;
      The_Id         : String)
   is
   begin
      Put (The_File, "                ");
      Put (The_File, "Append(The_Outputs, ");
      Put (The_File, '"' & " (" & '"');
      Put (The_File, "&" & '"' & Natural'Image (The_Size) & "." & '"');
      Put (The_File, "&");

      if The_Id = "BOOLEAN" then
         Put (The_File, "Integer'Image(BOOLEAN'Pos(" & The_Identifier & "))");
      else
         Put
           (The_File,
            Type_Of (The_Package, The_Id) & "'Image(" & The_Identifier & ")");
      end if;

      Put (The_File, "&");
      Put (The_File, '"' & ")" & '"');
      Put (The_File, ");");
      New_Line (The_File);
   end Assignment_To_String;

   procedure Generate_Assignment_To_String
     (The_Identifier : String;
      The_Package    : String;
      The_Id         : String;
      The_Type       : Type_Pointer)
   is
      The_First   : Integer;
      The_Last    : Integer;
      The_Index   : Type_Pointer;
      The_Element : Type_Pointer;
   begin
      if Is_Scalar (The_Type) then
         Assignment_To_String
           (The_Identifier,
            Size_Of (The_Type),
            The_Package,
            The_Id);
      elsif Is_Array (The_Type) then
         The_First   := First_Of (The_Type);
         The_Last    := Last_Of (The_Type);
         The_Index   := Array_Type (The_Type.all).The_Index;
         The_Element := Array_Type (The_Type.all).The_Element;

         for Index in 0 .. The_Last - The_First loop
            Assignment_To_String
              (The_Identifier &
               "(" &
               Type_Of
                 (The_Package,
                  To_String (Element (The_Map, The_Index))) &
               "(" &
               Integer'Image (The_First + Index) &
               "))",
               Size_Of (The_Element),
               The_Package,
               To_String (Element (The_Map, The_Element)));
         end loop;
      else
         raise Critical_Error;
      end if;
   end Generate_Assignment_To_String;

begin
   The_Parameters := Parameters_Of (The_Unit);

   Create (The_File, Out_File, The_Name & "-arun_package" & ADA_EXTENSION);

   Put_Line (The_File, "-- BC Boolean Compiler Ada Test Generator.");
   Put_Line
     (The_File,
      "-- Generated from '" &
      The_Name &
      SOURCE_EXTENSION &
      "', " &
      The_Date &
      ".");
   New_Line (The_File);

--              Put_Line(The_File, "with Test;");
   Put (The_File, "with");
   Put (The_File, " " & To_String (The_Unit.The_Package.The_Name.The_String));
   Put (The_File, ";");
   New_Line (The_File);

   Put_Line (The_File, "--");
   Put_Line (The_File, "package body ARun_Package is");
   New_Line (The_File);
   Put_Line (The_File, "    -- Execute ada code.");
   New_Line (The_File);
   Put_Line (The_File, "    procedure Run");
   Put_Line
     (The_File,
      "    (The_Inputs     : Boolean_Array;        -- The input boolean vaules.");
   Put_Line
     (The_File,
      "    The_Outputs     : out Unbounded_String; -- The output values.");
   Put_Line
     (The_File,
      "    Boolean_Output  : Boolean)              -- Boolean values output switch.");
   Put_Line (The_File, "    is");
   New_Line (The_File);

--              The_Input       : Test.Value;
--              The_Output      : Test.Value;

   The_Parameter := The_Parameters;
   while The_Parameter /= null loop
      Put (The_File, "            ");
      Put (The_File, To_String (The_Parameter.The_Identifier.The_String));
      Put (The_File, " :");
      Put
        (The_File,
         " " &
         Type_Of
           (To_String (The_Package),
            To_String (The_Parameter.The_Definition.The_String)));
      Put (The_File, ";");
      New_Line (The_File);

      The_Parameter := The_Parameter.The_Next;
   end loop;

   New_Line (The_File);
   Put_Line (The_File, "        begin");
   New_Line (The_File);

--              The_Input := Test.Value(Integer_At(0, Test.Value'Size, The_Inputs));

   The_Index := 0;

   The_Parameter := The_Parameters;
   while The_Parameter /= null loop

      if The_Parameter.Is_In then
         The_Type :=
           Type_Identifier (The_Parameter.The_Definition.The_Pointer.all)
             .The_Type;

         Generate_Assignment_To_BC
           (To_String (The_Parameter.The_Identifier.The_String),
            The_Index,
            To_String (The_Package),
            To_String (The_Parameter.The_Definition.The_String),
            The_Type);

         The_Index := The_Index + Size_Of (The_Type);
      end if;

      The_Parameter := The_Parameter.The_Next;
   end loop;

   New_Line (The_File);
   Put (The_File, "            ");
   Put (The_File, To_String (The_Package) & ".");
   Put (The_File, "Run");

   The_Parameter := The_Parameters;

   New_Line (The_File);
   Put (The_File, "                (");
   Put (The_File, To_String (The_Parameter.The_Identifier.The_String));
   The_Parameter := The_Parameter.The_Next;

   while The_Parameter /= null loop
      Put (The_File, ",");
      New_Line (The_File);
      Put (The_File, "                ");
      Put (The_File, To_String (The_Parameter.The_Identifier.The_String));
      The_Parameter := The_Parameter.The_Next;
   end loop;

   Put (The_File, ");");

   New_Line (The_File);
   New_Line (The_File);
   Put_Line (The_File, "            if Boolean_Output then");
   New_Line (The_File);

--              Append(The_Outputs, " ("&Image_Of(To_Integer_Boolean
--                      (Integer(The_Output))(0..Test.Value'Size-1))&")");

   The_Index := 0;

   The_Parameter := The_Parameters;
   while The_Parameter /= null loop
      The_Type :=
        Type_Identifier (The_Parameter.The_Definition.The_Pointer.all)
          .The_Type;

      if The_Parameter.Is_Out then
         Generate_Assignment_To_Boolean_String
           (To_String (The_Parameter.The_Identifier.The_String),
            The_Index,
            To_String (The_Package),
            To_String (The_Parameter.The_Definition.The_String),
            The_Type);

         The_Index :=
           The_Index +
           Size_Of
             (Type_Identifier
                (Parameter_Node (The_Parameter.all).The_Definition
                   .The_Pointer.all)
                .The_Type);
      end if;

      The_Parameter := The_Parameter.The_Next;
   end loop;

   New_Line (The_File);
   Put_Line (The_File, "            else");
   New_Line (The_File);

--              Append(The_Outputs, " ("&Test.Value'Image(The_Output)&")");

   The_Parameter := The_Parameters;
   while The_Parameter /= null loop

      The_Type :=
        Type_Identifier (The_Parameter.The_Definition.The_Pointer.all)
          .The_Type;

      if The_Parameter.Is_Out then
         Generate_Assignment_To_String
           (To_String (The_Parameter.The_Identifier.The_String),
            To_String (The_Package),
            To_String (The_Parameter.The_Definition.The_String),
            The_Type);
      end if;

      The_Parameter := The_Parameter.The_Next;
   end loop;

   New_Line (The_File);
   Put_Line (The_File, "        end if;");
   Put_Line (The_File, "    end Run;");
   New_Line (The_File);
   Put_Line (The_File, "end ARun_Package;");

   Close (The_File);

end ARun_File;

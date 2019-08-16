-- BC Boolean Compiler
-- Copyright (c) 2007 Paul Estep

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with Sax.Readers;        use Sax.Readers;
with Sax.Attributes;     use Sax.Attributes;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;        use Unicode.CES;
--
with System_Package; use System_Package;
--

package body Parameter_Package.IO_Package is

   Not_Letters : Character_Set :=
     not To_Set (Character_Range'(Low => 'A', High => 'Z'));
   Not_Digits : Character_Set :=
     not To_Set (Character_Range'(Low => '0', High => '9'));

   The_Inputs  : Parameter_List_Type;
   The_Outputs : Parameter_List_Type;

   -- XML reader definitions

   type Reader is new Sax.Readers.Reader with record
      Is_Parameter   : Boolean := False;
      The_Parameters : Parameter_List_Type;
      The_Kind       : Parameter_Kind;
      The_Size       : SYSPositive;
      The_First      : SYSModular;
      The_Last       : SYSModular;
      Is_Signed      : Boolean;
      The_Length     : SYSPositive;
      Current_Value  : Unbounded_String;
   end record;

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI :        Unicode.CES.Byte_Sequence := "";
      Local_Name    :        Unicode.CES.Byte_Sequence := "";
      Qname         :        Unicode.CES.Byte_Sequence := "";
      Atts          :        Sax.Attributes.Attributes'Class);

   procedure Characters
     (Handler : in out Reader;
      Ch      :        Unicode.CES.Byte_Sequence);

   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI :        Unicode.CES.Byte_Sequence := "";
      Local_Name    :        Unicode.CES.Byte_Sequence := "";
      Qname         :        Unicode.CES.Byte_Sequence := "");

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI :        Unicode.CES.Byte_Sequence := "";
      Local_Name    :        Unicode.CES.Byte_Sequence := "";
      Qname         :        Unicode.CES.Byte_Sequence := "";
      Atts          :        Sax.Attributes.Attributes'Class)
   is
   begin
      Handler.Current_Value := Null_Unbounded_String;
      if Local_Name = "parameter" then
         Handler.Is_Parameter := True;
      elsif Local_Name = "inputs" then
         Handler.The_Parameters := The_Inputs;
      elsif Local_Name = "outputs" then
         Handler.The_Parameters := The_Outputs;
      end if;
   end Start_Element;

   procedure Characters
     (Handler : in out Reader;
      Ch      :        Unicode.CES.Byte_Sequence)
   is
      The_Character : Unbounded_String := Null_Unbounded_String;
   begin
      if Handler.Is_Parameter then
         Handler.Current_Value := Handler.Current_Value & Ch;
      end if;
   end Characters;

   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI :        Unicode.CES.Byte_Sequence := "";
      Local_Name    :        Unicode.CES.Byte_Sequence := "";
      Qname         :        Unicode.CES.Byte_Sequence := "")
   is
      The_Integer : SYSInteger;
      The_Modular : SYSModular;
      The_Last    : SYSPositive;
   begin
      if Local_Name = "parameter" then
         Handler.Is_Parameter := False;
         case Handler.The_Kind is
            when Scalar_Parameter =>
               Append
                 (The_Parameters => Handler.The_Parameters,
                  The_Size       => Handler.The_Size,
                  The_First      => Handler.The_First,
                  The_Last       => Handler.The_Last,
                  Is_Signed      => Handler.Is_Signed);

            when Array_Parameter =>
               Append
                 (The_Parameters => Handler.The_Parameters,
                  The_Size       => Handler.The_Size,
                  The_First      => Handler.The_First,
                  The_Last       => Handler.The_Last,
                  Is_Signed      => Handler.Is_Signed,
                  The_Length     => Handler.The_Length);
         end case;

      elsif Local_Name = "kind" then
         Handler.The_Kind :=
           Parameter_Kind'Value
             (Trim
                (To_String (Handler.Current_Value),
                 Not_Letters,
                 Not_Letters));

      elsif Local_Name = "size" then
         Ada.Integer_Text_IO.Get
           (Trim (To_String (Handler.Current_Value), Not_Digits, Not_Digits),
            The_Integer,
            The_Last);
         Handler.The_Size := The_Integer;

      elsif Local_Name = "first" then
         SYSModular_IO.Get
           (Trim (To_String (Handler.Current_Value), Not_Digits, Not_Digits),
            The_Modular,
            The_Last);
         Handler.The_First := The_Modular;

      elsif Local_Name = "last" then
         SYSModular_IO.Get
           (Trim (To_String (Handler.Current_Value), Not_Digits, Not_Digits),
            The_Modular,
            The_Last);
         Handler.The_Last := The_Modular;

      elsif Local_Name = "signed" then
         Handler.Is_Signed :=
           Boolean'Value
             (Trim
                (To_String (Handler.Current_Value),
                 Not_Letters,
                 Not_Letters));

      elsif Local_Name = "length" then
         Ada.Integer_Text_IO.Get
           (Trim (To_String (Handler.Current_Value), Not_Digits, Not_Digits),
            The_Integer,
            The_Last);
         Handler.The_Length := The_Integer;

      elsif Local_Name = "inputs" then
         IO_Package.The_Inputs := Handler.The_Parameters;

      elsif Local_Name = "outputs" then
         IO_Package.The_Outputs := Handler.The_Parameters;
      end if;
   end End_Element;

   -- Load a saved file of input operand lengths.

   procedure Load
     (The_Source_File_Name :     String;
      The_Inputs           : out Parameter_List_Type;
      The_Outputs          : out Parameter_List_Type)
   is
      XML_Reader : Reader;
      The_File   : File_Input;
   begin
      Set_Public_Id (The_File, "Parameters file");
      Set_System_Id (The_File, The_Source_File_Name);
      Open (The_Source_File_Name, The_File);

      Set_Feature (XML_Reader, Namespace_Prefixes_Feature, False);
      Set_Feature (XML_Reader, Namespace_Feature, False);
      Set_Feature (XML_Reader, Validation_Feature, False);

      Parse (XML_Reader, The_File);

      Close (The_File);

      The_Inputs  := IO_Package.The_Inputs;
      The_Outputs := IO_Package.The_Outputs;

   exception

      when Name_Error =>
         raise Parameter_Package.IO_Package.Name_Error;

      when Status_Error | Mode_Error | Use_Error | Device_Error | End_Error | Data_Error =>
         raise Parameter_Package.IO_Package.IO_Error;

      when XML_Fatal_Error =>
         raise Parameter_Package.IO_Package.IO_Error;
   end Load;

   -- Save input operand sizes to a file.

   procedure Save
     (The_Source_File_Name : String;
      The_Inputs           : Parameter_List_Type;
      The_Outputs          : Parameter_List_Type)
   is

      The_File : File_Type;

      procedure Write (The_Position : Cursor) is
         The_Parameter : Parameter_Type;
      begin
         The_Parameter := Element (The_Position);
         Put_Line (The_File, "<parameter>");
         Put_Line
           (The_File,
            "<kind>" &
            Parameter_Kind'Image (The_Parameter.The_Kind) &
            "</kind>");
         Put_Line
           (The_File,
            "<size>" & Integer'Image (The_Parameter.The_Size) & "</size>");

         Put_Line
           (The_File,
            "<first>" & SYSModular'Image (The_Parameter.The_First) & "</first>");

         Put_Line
           (The_File,
            "<last>" & SYSModular'Image (The_Parameter.The_Last) & "</last>");

         Put_Line
           (The_File,
            "<signed>" &
            Boolean'Image (The_Parameter.Is_Signed) &
            "</signed>");

         case The_Parameter.The_Kind is
            when Scalar_Parameter =>
               null;
            when Array_Parameter =>
               Put_Line
                 (The_File,
                  "<length>" &
                  Integer'Image (The_Parameter.The_Length) &
                  "</length>");
         end case;
         Put_Line (The_File, "</parameter>");
      end Write;

   begin
      Create (The_File, Out_File, The_Source_File_Name);

      Put_Line (The_File, "<?xml version=" & '"' & "1.0" & '"' & " ?>");
      Put_Line (The_File, "<parameters>");
      Put_Line (The_File, "<inputs>");
      Iterate (The_Inputs, Write'Access);
      Put_Line (The_File, "</inputs>");
      Put_Line (The_File, "<outputs>");
      Iterate (The_Outputs, Write'Access);
      Put_Line (The_File, "</outputs>");
      Put_Line (The_File, "</parameters>");

      Close (The_File);

   exception

      when Name_Error =>
         raise Parameter_Package.IO_Package.Name_Error;

      when Status_Error | Mode_Error | Use_Error | Device_Error | End_Error | Data_Error =>
         raise Parameter_Package.IO_Package.IO_Error;
   end Save;

end Parameter_Package.IO_Package;

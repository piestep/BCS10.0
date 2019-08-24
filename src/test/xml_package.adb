-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
--
with Sax.Readers;        use Sax.Readers;
with Sax.Attributes;     use Sax.Attributes;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;        use Unicode.CES;
--

package body XML_Package is

   The_Map : Map := Empty_Map;

   -- XML reader definitions

   type Reader is new Sax.Readers.Reader with record
      Current_Value : Unbounded_String := Null_Unbounded_String;
      The_Strings   : Vector;
      The_Record    : XML_Record      :=
        (Null_Unbounded_String, Empty_Vector, Empty_Vector);
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
      if Local_Name = "test" then
         Handler.The_Record :=
           (Null_Unbounded_String, Empty_Vector, Empty_Vector);
      elsif Local_Name = "name" then
         Handler.Current_Value := Null_Unbounded_String;
      elsif Local_Name = "code" then
         Handler.Current_Value := Null_Unbounded_String;
         Handler.The_Strings   := Empty_Vector;
      elsif Local_Name = "listing" or Local_Name = "graph" then
         Handler.Current_Value := Null_Unbounded_String;
         Handler.The_Strings   := Empty_Vector;
      elsif Local_Name = "ln" then
         Handler.Current_Value := Null_Unbounded_String;
      end if;
   end Start_Element;

   procedure Characters
     (Handler : in out Reader;
      Ch      :        Unicode.CES.Byte_Sequence)
   is
   begin
      Handler.Current_Value := Handler.Current_Value & Ch;
   end Characters;

   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI :        Unicode.CES.Byte_Sequence := "";
      Local_Name    :        Unicode.CES.Byte_Sequence := "";
      Qname         :        Unicode.CES.Byte_Sequence := "")
   is
   begin
      if Local_Name = "test" then
         Insert (The_Map, Handler.The_Record.The_Name, Handler.The_Record);
         Handler.The_Record :=
           (Null_Unbounded_String, Empty_Vector, Empty_Vector);
      elsif Local_Name = "name" then
         Handler.The_Record.The_Name := Trim (Handler.Current_Value, Both);
      elsif Local_Name = "code" then
         Handler.The_Record.The_Code := Handler.The_Strings;
         Handler.The_Strings         := Empty_Vector;
      elsif Local_Name = "listing" or Local_Name = "graph" then
         Handler.The_Record.The_Listing := Handler.The_Strings;
         Handler.The_Strings            := Empty_Vector;
      elsif Local_Name = "ln" then
         Append (Handler.The_Strings, Trim (Handler.Current_Value, Right));
      end if;
   end End_Element;

   -- Load a saved file of input operand lengths.

   procedure Load
     (The_Source_File_Name :     String;
      The_Tests            : out Tests_Map.Map)
   is
      XML_Reader : Reader;
      The_File   : File_Input;
   begin
      The_Map := Empty_Map;

      Set_Public_Id (The_File, "Test file");
      Set_System_Id (The_File, The_Source_File_Name);
      Open (The_Source_File_Name, The_File);

      Set_Feature (XML_Reader, Namespace_Prefixes_Feature, False);
      Set_Feature (XML_Reader, Namespace_Feature, False);
      Set_Feature (XML_Reader, Validation_Feature, False);

      Parse (XML_Reader, The_File);

      Close (The_File);

      The_Tests := The_Map;

   exception

      when Name_Error =>
         raise XML_Package.Name_Error;

      when Status_Error | Mode_Error | Use_Error | Device_Error | End_Error | Data_Error =>
         raise XML_Package.IO_Error;

      when XML_Fatal_Error =>
         raise XML_Package.IO_Error;
   end Load;

end XML_Package;

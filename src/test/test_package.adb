-- BC Boolean Compiler
-- Copyright (c) 2019 Paul Estep

with AUnit.Assertions;
--
with Ada.Sequential_IO;
--
with Ada.Text_IO;
--

package body Test_Package is

   -- Compare files and/or string types.

   type Byte is mod 256;

   package Byte_IO is new Ada.Sequential_IO (Element_Type => Byte);

   --AUnit assert for boolean values.

   procedure Integer_Type_Assert (The_Result   : Integer_Type;
                                  The_Expected : Integer_Type;
                                  The_Test     : String;
                                  The_Text     : String := "") is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test &
           " " &
           Integer_Type'Image (The_Result) &
           " -- " &
           Integer_Type'Image (The_Expected) &
           The_Text);
   end Integer_Type_Assert;

   procedure Modular_Type_Assert (The_Result   : Modular_Type;
                                  The_Expected : Modular_Type;
                                  The_Test     : String;
                                  The_Text     : String := "") is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test &
           " " &
           Modular_Type'Image (The_Result) &
           " -- " &
           Modular_Type'Image (The_Expected) &
           The_Text);
   end Modular_Type_Assert;

   --AUnit assert for boolean values.

   procedure Assert
     (The_Result   : Boolean;
      The_Expected : Boolean;
      The_Test     : String;
      The_Text     : String := "")
   is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test &
           " " &
           Boolean'Image (The_Result) &
           " -- " &
           Boolean'Image (The_Expected) &
           The_Text);
   end Assert;

   --AUnit assert for SYSInteger values.

   procedure Assert
     (The_Result   : Integer;
      The_Expected : Integer;
      The_Test     : String;
      The_Text     : String := "")
   is
   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test &
           " " &
           Integer'Image (The_Result) &
           " -- " &
           Integer'Image (The_Expected) &
           The_Text);
   end Assert;

   --AUnit assert for string values.

   procedure Assert
     (The_Result   : String;
      The_Expected : String;
      The_Test     : String;
      The_Text     : String := "")
   is

      function Truncate (The_String : String) return String is
      begin
         if The_String'Length > MAXIMUM_ASSERT_STRING then
            return The_String
              (The_String'First ..
                 The_String'First + MAXIMUM_ASSERT_STRING - 1) &
              " ...";
         else
            return The_String;
         end if;
      end Truncate;

   begin
      AUnit.Assertions.Assert
        (The_Result = The_Expected,
         The_Test &
           " " &
           Truncate (The_Result) &
           " -- " &
           Truncate (The_Expected) &
           Truncate (The_Text));
   end Assert;

   --AUnit assert for unbounded string values.

   procedure Assert
     (The_Result   : Unbounded_String;
      The_Expected : Unbounded_String;
      The_Test     : String;
      The_Text     : String := "")
   is
   begin
      Assert
        (To_String (The_Result),
         To_String (The_Expected),
         The_Test,
         The_Text);
   end Assert;

   -------------------
   -- Compare_Files --
   -------------------

   function Compare_Files (The_Left, The_Right : in String) return Boolean is
      use Byte_IO;

      The_File1 : File_Type;
      The_File2 : File_Type;
      The_Byte1 : Byte;
      The_Byte2 : Byte;
   begin
      Open (The_File1, In_File, The_Left);
      Open (The_File2, In_File, The_Right);

      while not End_Of_File (The_File1) loop
         Read (The_File1, The_Byte1);
         Read (The_File2, The_Byte2);
         if The_Byte1 /= The_Byte2 then
            Close (The_File1);
            Close (The_File2);
            return False;
         end if;
      end loop;

      if not End_Of_File (The_File2) then
         Close (The_File1);
         Close (The_File2);
         return False;
      end if;

      Close (The_File1);
      Close (The_File2);

      return True;
   end Compare_Files;

   ------------------------------------------
   -- Compare_First_Line_Of_File_To_String --
   ------------------------------------------

   function Compare_First_Line_Of_File_To_String
     (The_Filename : String;
      The_String   : String) return Boolean
   is
      use Ada.Text_IO;

      The_File      : File_Type;
      The_Character : Character;

   begin
      Open (The_File, In_File, The_Filename);

      while not End_Of_File (The_File) loop
         if End_Of_Line (The_File) then
            exit;
         else
            Get (The_File, The_Character);
            if Col (The_File) > Positive_Count (The_String'Length + 1)
              or else
                The_Character /=
                  The_String (Positive (Col (The_File)) - 1)
            then
               Close (The_File);
               return False;
            end if;
         end if;
      end loop;

      if Positive_Count (The_String'Length + 1) /= Col (The_File) then
         Close (The_File);
         return False;
      end if;

      Close (The_File);
      return True;
   end Compare_First_Line_Of_File_To_String;

   -----------------------------
   -- Compare_File_To_Strings --
   -----------------------------

   function Compare_File_To_Strings
     (The_Filename : in String;
      The_Strings  :    Array_Of_Unbounded_Strings) return Boolean
   is
      use Ada.Text_IO;

      The_File      : File_Type;
      The_Character : Character;
   begin
      Open (The_File, In_File, The_Filename);

      while not End_Of_File (The_File) loop
         if End_Of_Line (The_File) then
            if Line (The_File) > The_Strings'Length
              or else
                Positive_Count
                  (Length (The_Strings (Positive (Line (The_File)))) + 1) /=
              Col (The_File)
            then
               Close (The_File);
               return False;
            end if;
            Skip_Line (The_File);
         else
            Get (The_File, The_Character);
            if Line (The_File) > The_Strings'Length
              or else
                Col (The_File) >
              Positive_Count
                (Length (The_Strings (Positive (Line (The_File)))) + 1)
              or else
                The_Character /=
                  Element
                    (The_Strings (Positive (Line (The_File))),
                     Positive (Col (The_File)) - 1)
            then
               Close (The_File);
               return False;
            end if;
         end if;

      end loop;
      if The_Strings'Length /= Positive (Line (The_File)) then
         Close (The_File);
         return False;
      end if;

      Close (The_File);
      return True;
   end Compare_File_To_Strings;

   -- Return true if a file and an vector of unbounded strings are the same.

   function Compare_File_To_Strings_Vector
     (The_Filename : String;
      The_Strings  : Strings_Vector.Vector) return Boolean
   is
      use Ada.Text_IO;
      use Strings_Vector;

      The_File      : File_Type;
      The_String    : Unbounded_String;
      The_Character : Character;

      function Length_Of
        (The_Strings : Vector;
         The_Index   : Positive) return Natural is
        (Length (Element (The_Strings, The_Index)));

      function Length_Of
        (The_Strings : Vector) return Positive_Count is
        (Positive_Count (Length (The_Strings)));

      function String_Of
        (The_Strings : Vector;
         The_Index   : Positive) return Unbounded_String is
        (Element (The_Strings, The_Index));

   begin
      Open (The_File, In_File, The_Filename);

      while not End_Of_File (The_File) loop
         if End_Of_Line (The_File) then

            if Line (The_File) > Length_Of (The_Strings)
              or else
                Positive_Count
                  (Length_Of (The_Strings, Positive (Line (The_File))) + 1) /=
              Col (The_File)
            then
               Close (The_File);

               return False;
            end if;
            Skip_Line (The_File);

         else
            Get (The_File, The_Character);

            if Line (The_File) > Length_Of (The_Strings)
              or else
                Col (The_File) >
              Positive_Count
                (Length_Of (The_Strings, Positive (Line (The_File))) + 1)
              or else
                The_Character /=
                  Element
                    (String_Of (The_Strings, Positive (Line (The_File))),
                     Positive (Col (The_File)) - 1)
            then
               Close (The_File);

               return False;
            end if;
         end if;

      end loop;

      if Length_Of (The_Strings) /= Line (The_File) then
         Close (The_File);

         return False;
      end if;

      Close (The_File);

      return True;
   end Compare_File_To_Strings_Vector;

   -- Save code to file.

   procedure Save
     (The_Source_File_Name : String;
      The_Code             : Strings_Vector.Vector)
   is
      use Strings_Vector;

      The_File   : Ada.Text_IO.File_Type;
      The_Cursor : Strings_Vector.Cursor;
   begin
      Ada.Text_IO.Create (The_File, Ada.Text_IO.Out_File, The_Source_File_Name);
      The_Cursor := First (The_Code);

      while The_Cursor /= No_Element loop
         Ada.Text_IO.Put (The_File, To_String (Element (The_Cursor)));
         Ada.Text_IO.New_Line (The_File);

         The_Cursor := Next (The_Cursor);
      end loop;

      Ada.Text_IO.Close (The_File);
   end Save;

   -- List code to standard output.

   procedure List
     (The_Code             : Strings_Vector.Vector;
      XML_Format           : Boolean := False)
   is
      use Strings_Vector;

      The_Cursor : Strings_Vector.Cursor;
   begin
      The_Cursor := First (The_Code);

      if XML_Format then
         Ada.Text_IO.Put_Line ("<code>");
      end if;

      while The_Cursor /= No_Element loop
         if XML_Format then
            Ada.Text_IO.Put ("<ln>");
         end if;

         Ada.Text_IO.Put (To_String (Element (The_Cursor)));

         if XML_Format then
            Ada.Text_IO.Put ("</ln>");
         end if;

         Ada.Text_IO.New_Line;

         The_Cursor := Next (The_Cursor);
      end loop;

      if XML_Format then
         Ada.Text_IO.Put_Line ("</code>");
      end if;

   end List;

end Test_Package;

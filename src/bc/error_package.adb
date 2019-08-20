-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Text_IO; use Ada.Text_IO;
--
with Debug_Package; use Debug_Package;
--

package body Error_Package is

   -- Return true if the left source positions is less than equal the right;

   function Is_Less_Than_Equal
     (The_Left, The_Right : Source_Position) return Boolean
   is
   begin
      if The_Left.The_Line < The_Right.The_Line then
         return True;

      elsif The_Left.The_Line = The_Right.The_Line then
         return The_Left.The_Column <= The_Right.The_Column;

      else
         return False;

      end if;
   end Is_Less_Than_Equal;

   -- Add a message to the list.

   procedure Add_Message (The_Message : Message_Type) is
      The_Element : Message_Type;
      The_Cursor  : Cursor;
   begin
      The_Number_Of_Errors := The_Number_Of_Errors + 1;
      The_Cursor           := First (The_Messages);

      while The_Cursor /= No_Element loop
         The_Element := Element (The_Cursor);

         if not Is_Less_Than_Equal
             (The_Element.The_Position,
              The_Message.The_Position)
         then
            Insert (The_Messages, The_Cursor, The_Message);
            return;
         end if;

         Next (The_Cursor);
      end loop;

      Append (The_Messages, The_Message);
   end Add_Message;

   -- Report a source error.

   procedure Source_Error (The_Position : Source_Position; The_Text : String) is
   begin
      Add_Message ((The_Position, Error_Kind, To_Unbounded_String (The_Text)));
   end Source_Error;

   -- Report a warning.

   procedure Source_Warning
     (The_Position : Source_Position;
      The_Text     : String)
   is
   begin
      Add_Message
        ((The_Position, Warning_Kind, To_Unbounded_String (The_Text)));
   end Source_Warning;

   -- Report compiler error.

   procedure Compiler_Error (The_Message : String) is
   begin
      Put_Line ("*** " & The_Message);

      raise Compiler_Exception;
   end Compiler_Error;

   -- Report compiler error at a source position.

   procedure Compiler_Error
     (The_Position : Source_Position;
      The_Message  : String)
   is
   begin
      Put_Line ("*** " & Image_Of (The_Position) & ": " & The_Message);

      raise Compiler_Exception;
   end Compiler_Error;

   -- List messages.

   procedure List_Messages (XML_Format : Boolean := False) is

      procedure List_Message (The_Cursor : Cursor) is
         The_Message : Message_Type;
      begin
         The_Message := Element (The_Cursor);

         if XML_Format then
            Put ("<ln>");
         end if;

         if The_Message.The_Kind = Error_Kind then
            Put
              (Image_Of (The_Message.The_Position) &
               ": " &
               To_String (The_Message.The_Text));
         else
            Put
              (Image_Of (The_Message.The_Position) &
               ": Warning: " &
               To_String (The_Message.The_Text));
         end if;

         if XML_Format then
            Put ("</ln>");
         end if;

         New_Line;
      end List_Message;

   begin
      if XML_Format then
         Put_Line ("<listing>");
      end if;

      Iterate (The_Messages, List_Message'Access);

      if XML_Format then
         Put_Line ("</listing>");
      end if;
   end List_Messages;

   -- Iterate over errors and warnings in order.

   procedure Iterate
     (Process : not null access procedure
        (The_Position : in Source_Position;
         The_Kind     : in Message_Kind;
         The_Text     : in String))
   is

      procedure Process_Message (Position : in Cursor) is
         The_Message : Message_Type;
      begin
         The_Message := Element (Position);
         Process
           (The_Message.The_Position,
            The_Message.The_Kind,
            To_String (The_Message.The_Text));
      end Process_Message;

   begin
      Iterate (The_Messages, Process_Message'Access);
   end Iterate;

begin

   Debug (Debug_Initialization, "Error_Package");

end Error_Package;

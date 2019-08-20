-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Containers.Vectors;
--
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
with System_Package; use System_Package;
with BC_Package;     use BC_Package;
with Source_Package; use Source_Package;
--

-- A package to manage error reporting for the Boolean Compiler (BC).

package Error_Package is

   -- Exception raise for a compiler error.

   Compiler_Exception : exception;

   -- Message types.

   type Message_Kind is (Error_Kind, Warning_Kind);

   -- Number of errors reported.

   The_Number_Of_Errors : SYSNatural := 0;

   -- Number of warnings reported.

   The_Number_Of_Warnings : SYSNatural := 0;

   -- Report a source error.

   procedure Source_Error (The_Position : Source_Position; The_Text : String);

   -- Report a source warning.

   procedure Source_Warning (The_Position : Source_Position; The_Text : String);

   -- Report compiler error.

   procedure Compiler_Error (The_Message : String);

   -- Report compiler error at a source position.

   procedure Compiler_Error
     (The_Position : Source_Position;
      The_Message  : String);

   -- List errors.

   procedure List_Messages (XML_Format : Boolean := False);

   -- Iterate over errors and warnings in order.

   procedure Iterate
     (Process : not null access procedure
        (The_Position : in Source_Position;
         The_Kind     : in Message_Kind;
         The_Text     : in String));

private

   -- Message type.

   type Message_Type is record
      The_Position : Source_Position;
      The_Kind     : Message_Kind;
      The_Text     : Unbounded_String;
   end record;

   -- Return true if the left source positions is less than equal the right.

   function Is_Less_Than_Equal
     (The_Left, The_Right : Source_Position) return Boolean;

   -- Messages Map (List).

   package Message_Vector is new Ada.Containers.Vectors (SYSNatural, Message_Type);

   use Message_Vector;

   -- The messages ordered by source position.

   The_Messages : Vector;

end Error_Package;

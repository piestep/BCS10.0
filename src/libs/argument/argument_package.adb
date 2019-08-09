-- BC Boolean Compiler
-- Copyright (c) 2018 Paul Estep
pragma Ada_2012;

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
--
with System_Package;                use System_Package;
with Values_Package;                use Values_Package;
with Argument_Package.Command_Line; use Argument_Package.Command_Line;
--

package body Argument_Package is

   type Argument_Kind is (Scalar_Argument, Array_Argument);

   type Argument (The_Kind : Argument_Kind := Scalar_Argument) is record
      The_Size  : SYSPositive;
      Is_Signed : Boolean;
      case The_Kind is
         when Scalar_Argument =>
            null;
         when Array_Argument =>
            The_Length : SYSPositive;
      end case;
   end record;

   package Argument_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Argument);

   use Argument_Vector;

   package Natural_IO is new Ada.Text_IO.Integer_IO (SYSNatural);

   -- Maximum number of argument values.

   MAX_ARGUMENT : constant := 128;

   -- Maximum length of all boolean values.

   MAX_BOOLEAN : constant := 127;

   -- The argument values.

   The_Arguments : Argument_Vector.Vector := Argument_Vector.Empty_Vector;

   -- The values of the arguments.

   The_Values : Values_Type := EMPTY_VALUES;

   -- The length of all boolean argument values.

   The_Lengths : SYSNatural := 0;

   procedure Assert_Condition
     (The_Condition : Boolean;
      The_Argument  : SYSPositive;
      The_Message   : String)
   is
   begin
      if The_Condition then
         Put (The_Argument, 4);
         Put_Line (The_Message);
         raise Usage_Error;
      end if;
   end Assert_Condition;

   function Number_Of return SYSNatural is
   begin
      return SYSNatural (Argument_Vector.Length (The_Arguments));
   end Number_Of;

   -- The values of the arguments.

   function Boolean_Of return Boolean_Array_Type is
   begin
      return The_Values.The_Array (0 .. Length_Of (The_Values) - 1);
   end Boolean_Of;

   -- The total length of all boolean argument values.

   function Length_Of return SYSNatural is
   begin
      return The_Lengths;
   end Length_Of;

   -- List arguments to the standard output.

   function Image_Of return String is
      The_Image    : Unbounded_String;
      The_Offset   : SYSNatural;
      The_Cursor   : Argument_Vector.Cursor;
      The_Argument : Argument;
      The_First    : Boolean := True;

   begin
      The_Image  := Null_Unbounded_String;
      The_Offset := 0;
      The_Cursor := First (The_Arguments);

      while Has_Element (The_Cursor) loop
         The_Argument := Element (The_Cursor);

         case The_Argument.The_Kind is
            when Scalar_Argument =>

               if The_First then
                  The_First := False;
               else
                  Append (The_Image, " ");
               end if;

               if The_Argument.Is_Signed then
                  Append
                    (The_Image,
                     "!" &
                       Trim (Integer'Image (The_Argument.The_Size), Left) &
                       "." &
                       Trim
                       (BCInteger'Image
                            (BCInteger_At
                                 (The_Offset,
                                  The_Argument.The_Size,
                                  The_Values.The_Array
                                    (0 .. Length_Of (The_Values) - 1))),
                        Left));
               else
                  Append
                    (The_Image,
                     "!%" &
                       Trim (Integer'Image (The_Argument.The_Size), Left) &
                       "." &
                       Trim
                       (BCModular'Image
                            (BCModular_At
                                 (The_Offset,
                                  The_Argument.The_Size,
                                  The_Values.The_Array
                                    (0 .. Length_Of (The_Values) - 1))),
                        Left));
               end if;

               The_Offset := The_Offset + The_Argument.The_Size;

            when Array_Argument =>
               if The_First then
                  The_First := False;
               else
                  Append (The_Image, " ");
               end if;

               if The_Argument.Is_Signed then
                  Append
                    (The_Image,
                     "@" &
                       Trim (Integer'Image (The_Argument.The_Size), Left) &
                       "." &
                       Trim (Integer'Image (The_Argument.The_Length), Left));
                  for I in 1 .. The_Argument.The_Length loop
                     Append
                       (The_Image,
                        " " &
                          Trim
                          (BCInteger'Image
                               (BCInteger_At
                                    (The_Offset,
                                     The_Argument.The_Size,
                                     The_Values.The_Array
                                       (0 .. Length_Of (The_Values) - 1))),
                           Left));

                     The_Offset := The_Offset + The_Argument.The_Size;
                  end loop;

               else
                  Append
                    (The_Image,
                     "@%" &
                       Trim (Integer'Image (The_Argument.The_Size), Left) &
                       "." &
                       Trim (Integer'Image (The_Argument.The_Length), Left));
                  for I in 1 .. The_Argument.The_Length loop
                     Append
                       (The_Image,
                        " " &
                          Trim
                          (BCModular'Image
                               (BCModular_At
                                    (The_Offset,
                                     The_Argument.The_Size,
                                     The_Values.The_Array
                                       (0 .. Length_Of (The_Values) - 1))),
                           Left));

                     The_Offset := The_Offset + The_Argument.The_Size;
                  end loop;
               end if;

         end case;

         Next (The_Cursor);
      end loop;

      The_First := True;

      Append (The_Image, " * ");

      The_Offset := 1;
      The_Cursor := First (The_Arguments);

      while Has_Element (The_Cursor) loop
         The_Argument := Element (The_Cursor);

         case The_Argument.The_Kind is
            when Scalar_Argument =>
               if The_First then
                  The_First := False;
               else
                  Append (The_Image, " ");
               end if;

               Append
                 (The_Image,
                  Image_Of (The_Values)
                  (The_Offset .. The_Offset + The_Argument.The_Size - 1));

               The_Offset := The_Offset + The_Argument.The_Size;

            when Array_Argument =>
               if The_First then
                  The_First := False;
               else
                  Append (The_Image, " ");
               end if;

               for I in 1 .. The_Argument.The_Length loop
                  if I /= 1 then
                     Append (The_Image, ",");
                  end if;

                  Append
                    (The_Image,
                     Image_Of (The_Values)
                     (The_Offset .. The_Offset + The_Argument.The_Size - 1));

                  The_Offset := The_Offset + The_Argument.The_Size;
               end loop;
         end case;

         Next (The_Cursor);
      end loop;

      return To_String (The_Image);
   end Image_Of;

   -- Parse the command line starting at the start until the end.

   procedure Parse (The_Start : SYSNatural; The_End : SYSNatural) is
      The_Argument : SYSNatural;
      The_Position : Positive;
      The_Size     : SYSNatural;
      Is_Signed    : Boolean;
      The_Value    : SYSInteger;
      The_Length   : SYSNatural;
   begin
      The_Arguments := Argument_Vector.Empty_Vector;
      The_Values    := EMPTY_VALUES;
      The_Lengths   := 0;

      The_Argument := The_Start;

      while The_Argument <= The_End loop
         if Argument_At (The_Argument) (1) = '!' then
            Is_Signed    := True;
            The_Position := 2;
            if Argument_At (The_Argument) (The_Position) = '%' then
               Is_Signed    := False;
               The_Position := The_Position + 1;
            end if;

            -- scalar argument
            Natural_IO.Get
              (Argument_At (The_Argument)
               (The_Position .. Argument_At (The_Argument)'Length),
               The_Size,
               The_Position);
            Assert_Condition
              (Argument_At (The_Argument) (The_Position + 1) /= '.',
               The_Argument,
               "Expected period between size and value.");

            if Is_Signed then
               Get
                 (Argument_At (The_Argument)
                  (The_Position + 2 .. Argument_At (The_Argument)'Last),
                  The_Value,
                  The_Position);
               Assert_Condition
                 (The_Value < -2**(The_Size - 1) or
                      The_Value > 2**(The_Size - 1) - 1,
                  The_Argument,
                  "Value over size.");
            else
               Natural_IO.Get
                 (Argument_At (The_Argument)
                  (The_Position + 2 .. Argument_At (The_Argument)'Last),
                  The_Value,
                  The_Position);

               Assert_Condition
                 (The_Value > 2**The_Size - 1,
                  The_Argument,
                  "Value over size.");
            end if;

            Assert_Condition
              (The_Position /= Argument_At (The_Argument)'Length,
               The_Argument,
               "Expected numeric value.");

            Argument_Vector.Append
              (The_Arguments,
               Argument'
                 (The_Kind  => Scalar_Argument,
                  The_Size  => The_Size,
                  Is_Signed => Is_Signed));

            The_Values :=
              The_Values &
              Create
              (Boolean_Package.SYSInteger_To_Boolean_Array_Type (The_Value)
               (0 .. The_Size - 1));
            --              Append
            --                (The_Values,
            --                 BCInteger_To_Array (The_Value) (0 .. The_Size - 1));

            The_Lengths := The_Lengths + The_Size;

         elsif Argument_At (The_Argument) (1) = '@' then
            Is_Signed    := True;
            The_Position := 2;
            if Argument_At (The_Argument) (The_Position) = '%' then
               Is_Signed    := False;
               The_Position := The_Position + 1;
            end if;

            -- array argument
            Natural_IO.Get
              (Argument_At (The_Argument)
               (The_Position .. Argument_At (The_Argument)'Length),
               The_Size,
               The_Position);

            Assert_Condition
              (Argument_At (The_Argument) (The_Position + 1) /= '.',
               The_Argument,
               "Expected period between size and length.");

            Natural_IO.Get
              (Argument_At (The_Argument)
               (The_Position + 2 .. Argument_At (The_Argument)'Length),
               The_Length,
               The_Position);

            Assert_Condition
              (The_Position /= Argument_At (The_Argument)'Length,
               The_Argument,
               "Expected numeric value.");

            Assert_Condition
              (The_Argument + The_Length > Argument_Count,
               The_Argument,
               "Expected" &
                 SYSNatural'Image (The_Length) &
                 " values for array.");

            for I in The_Argument + 1 .. The_Argument + The_Length loop
               if Is_Signed then
                  Get (Argument_At (I), The_Value, The_Position);
                  Assert_Condition
                    (The_Value < -2**(The_Size - 1) or
                         The_Value > 2**(The_Size - 1) - 1,
                     The_Argument,
                     "Value over size.");
               else
                  Natural_IO.Get (Argument_At (I), The_Value, The_Position);

                  Assert_Condition
                    (The_Value > 2**The_Size - 1,
                     The_Argument,
                     "Value over size.");
               end if;

               Assert_Condition
                 (The_Position /= Argument_At (I)'Length,
                  I,
                  "Expected numeric value.");

               The_Values :=
                 The_Values &
                 Create
                 (Boolean_Package.SYSInteger_To_Boolean_Array_Type (The_Value)
                  (0 .. The_Size - 1));
               --                 Append
               --                   (The_Values,
               --                    BCInteger_To_Array (The_Value) (0 .. The_Size - 1));

               The_Lengths := The_Lengths + The_Size;
            end loop;

            Argument_Vector.Append
              (The_Arguments,
               Argument'
                 (The_Kind   => Array_Argument,
                  The_Size   => The_Size,
                  Is_Signed  => Is_Signed,
                  The_Length => The_Length));

            The_Argument := The_Argument + The_Length;
         else
            Assert_Condition
              (True,
               The_Argument,
               "Expected '!' for array or '@' for scalar argument.");
         end if;

         The_Argument := The_Argument + 1;

      end loop;

   end Parse;

end Argument_Package;

-- BC Boolean Compiler
-- Copyright (c) 2015 Paul Estep

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--

package body Parameter_Package is

   function Size_Of (The_Parameter : in Parameter_Type) return SYSInteger is
   begin
      case The_Parameter.The_Kind is
         when Scalar_Parameter =>
            return The_Parameter.The_Size;
         when Array_Parameter =>
            return The_Parameter.The_Size * The_Parameter.The_Length;
      end case;
   end Size_Of;

   function Size_Of (The_Parameters : in Parameter_List_Type) return SYSNatural is
      The_Position  : Cursor;
      The_Parameter : Parameter_Type;
      The_Length    : SYSNatural := 0;
   begin
      The_Position := First (The_Parameters);

      while Parameter_Vector.Has_Element (The_Position) loop

         The_Parameter := Parameter_Vector.Element (The_Position);
         The_Length    := The_Length + Size_Of (The_Parameter);
         Parameter_Vector.Next (The_Position);
      end loop;

      return The_Length;
   end Size_Of;

   function First_Of
     (The_Parameters : in Parameter_List_Type) return Boolean_Array_Type
   is
      The_Values    : Boolean_Array_Type (0 .. Size_Of (The_Parameters) - 1);
      The_Position  : Cursor;
      The_Parameter : Parameter_Type;
      The_Index     : SYSNatural := 0;
   begin
      The_Position := First (The_Parameters);

      while Parameter_Vector.Has_Element (The_Position) loop
         The_Parameter := Parameter_Vector.Element (The_Position);

         case The_Parameter.The_Kind is
            when Scalar_Parameter =>

               The_Values
                 (The_Index .. The_Index + The_Parameter.The_Size - 1) :=
                 BCModular_To_Array (The_Parameter.The_First)
                   (0 .. The_Parameter.The_Size - 1);

            when Array_Parameter =>

               for Index in 0 .. The_Parameter.The_Length - 1 loop
                  The_Values
                    (The_Index + Index * The_Parameter.The_Size ..
                         The_Index +
                         Index * The_Parameter.The_Size +
                         The_Parameter.The_Size -
                         1) :=
                    BCModular_To_Array (The_Parameter.The_First)
                      (0 .. The_Parameter.The_Size - 1);
               end loop;

         end case;

         The_Index := The_Index + Size_Of (The_Parameter);
         Parameter_Vector.Next (The_Position);
      end loop;

      return The_Values;
   end First_Of;

   function Last_Of
     (The_Parameters : in Parameter_List_Type) return Boolean_Array_Type
   is
      The_Values    : Boolean_Array_Type (0 .. Size_Of (The_Parameters) - 1);
      The_Position  : Cursor;
      The_Parameter : Parameter_Type;
      The_Index     : SYSNatural := 0;
   begin
      The_Position := First (The_Parameters);

      while Parameter_Vector.Has_Element (The_Position) loop
         The_Parameter := Parameter_Vector.Element (The_Position);

         case The_Parameter.The_Kind is
            when Scalar_Parameter =>

               The_Values
                 (The_Index .. The_Index + The_Parameter.The_Size - 1) :=
                 BCModular_To_Array (The_Parameter.The_Last)
                   (0 .. The_Parameter.The_Size - 1);

            when Array_Parameter =>

               for Index in 0 .. The_Parameter.The_Length - 1 loop
                  The_Values
                    (The_Index + Index * The_Parameter.The_Size ..
                         The_Index +
                         Index * The_Parameter.The_Size +
                         The_Parameter.The_Size -
                         1) :=
                    BCModular_To_Array (The_Parameter.The_Last)
                      (0 .. The_Parameter.The_Size - 1);
               end loop;
         end case;

         The_Index := The_Index + Size_Of (The_Parameter);
         Parameter_Vector.Next (The_Position);
      end loop;
      return The_Values;
   end Last_Of;

   procedure Increment
     (The_Values     : in out Boolean_Array_Type;
      The_Parameters :        Parameter_List_Type)
   is
      The_Position  : Cursor;
      The_Parameter : Parameter_Type;
      The_Index     : SYSNatural := 0;
      The_Value     : BCModular;
      The_Carry     : Boolean    := True;
      Index         : SYSNatural;
   begin
      The_Position := First (The_Parameters);

      while The_Carry = True and Parameter_Vector.Has_Element (The_Position)
      loop
         The_Parameter := Parameter_Vector.Element (The_Position);

         case The_Parameter.The_Kind is
            when Scalar_Parameter =>

               The_Value :=
                 BCModular_At (The_Index, The_Parameter.The_Size, The_Values);
               if The_Value = The_Parameter.The_Last then
                  -- reset parameter to first
                  The_Values
                    (The_Index .. The_Index + The_Parameter.The_Size - 1) :=
                    BCModular_To_Array (The_Parameter.The_First)
                      (0 .. The_Parameter.The_Size - 1);
               else
                  -- increment parameter
                  The_Values
                    (The_Index .. The_Index + The_Parameter.The_Size - 1) :=
                    BCModular_To_Array (The_Value + 1)
                      (0 .. The_Parameter.The_Size - 1);
                  The_Carry := False;
               end if;

            when Array_Parameter =>

               Index := 0;
               while The_Carry = True and Index < The_Parameter.The_Length loop

                  The_Value :=
                    BCModular_At
                      (The_Index + Index * The_Parameter.The_Size,
                       The_Parameter.The_Size,
                       The_Values);

                  if The_Value = The_Parameter.The_Last then
                     -- reset parameter to first
                     The_Values
                       (The_Index + Index * The_Parameter.The_Size ..
                            The_Index +
                            Index * The_Parameter.The_Size +
                            The_Parameter.The_Size -
                            1) :=
                       BCModular_To_Array (The_Parameter.The_First)
                         (0 .. The_Parameter.The_Size - 1);

                  else
                     -- increment parameter
                     The_Values
                       (The_Index + Index * The_Parameter.The_Size ..
                            The_Index +
                            Index * The_Parameter.The_Size +
                            The_Parameter.The_Size -
                            1) :=
                       BCModular_To_Array (The_Value + 1)
                         (0 .. The_Parameter.The_Size - 1);
                     The_Carry := False;
                  end if;

                  Index := Index + 1;
               end loop;
         end case;

         The_Index := The_Index + Size_Of (The_Parameter);
         Parameter_Vector.Next (The_Position);
      end loop;
   end Increment;

   procedure Append
     (The_Parameters : in out Parameter_List_Type;
      The_Size       : in     SYSNatural;
      The_First      : in     BCModular;
      The_Last       : in     BCModular;
      Is_Signed      : in     Boolean)
   is
   begin
      Append
        (The_Parameters,
         Parameter_Type'
           (The_Kind  => Scalar_Parameter,
            The_Size  => The_Size,
            The_First => The_First,
            The_Last  => The_Last,
            Is_Signed => Is_Signed));
   end Append;

   procedure Append
     (The_Parameters : in out Parameter_List_Type;
      The_Size       : in     SYSNatural;
      The_First      : in     BCModular;
      The_Last       : in     BCModular;
      Is_Signed      : in     Boolean;
      The_Length     : in     SYSNatural)
   is
   begin
      Append
        (The_Parameters,
         Parameter_Type'
           (The_Kind   => Array_Parameter,
            The_Size   => The_Size,
            The_First  => The_First,
            The_Last   => The_Last,
            Is_Signed  => Is_Signed,
            The_Length => The_Length));
   end Append;

   function Format
     (The_Values     : in Boolean_Array_Type;
      The_Parameters : in Parameter_List_Type) return String
   is
      The_Position  : Cursor;
      The_Parameter : Parameter_Type;
      The_Index     : SYSNatural       := 0;
      The_Value     : SYSInteger;
      The_String    : Unbounded_String := Null_Unbounded_String;
      The_First     : Boolean          := True;
   begin
      The_Position := First (The_Parameters);

      while Parameter_Vector.Has_Element (The_Position) loop
         The_Parameter := Parameter_Vector.Element (The_Position);

         if The_First then
            The_First := False;
         else
            The_String := The_String & " ";
         end if;

         case The_Parameter.The_Kind is
            when Scalar_Parameter =>

               if The_Parameter.Is_Signed then
                  The_Value :=
                    BCInteger_At
                      (The_Index,
                       The_Parameter.The_Size,
                       The_Values);
                  The_String :=
                    The_String &
                    Image_Of (The_Parameter) &
                    Trim (SYSInteger'Image (The_Value), Left);

               else
                  The_Value :=
                    SYSInteger
                      (BCModular_At
                         (The_Index,
                          The_Parameter.The_Size,
                          The_Values));
                  The_String :=
                    The_String &
                    Image_Of (The_Parameter) &
                    Trim (SYSInteger'Image (The_Value), Left);
               end if;

            when Array_Parameter =>

               if The_Parameter.Is_Signed then
                  The_String :=
                    The_String &
                    Image_Of (The_Parameter);

                  for Index in 0 .. The_Parameter.The_Length - 1 loop
                     The_Value :=
                       SYSInteger
                         (BCInteger_At
                            (The_Index + Index * The_Parameter.The_Size,
                             The_Parameter.The_Size,
                             The_Values));
                     The_String :=
                       The_String & " " & SYSInteger'Image (The_Value);
                  end loop;

               else
                  The_String :=
                    The_String &
                    Image_Of (The_Parameter);

                  for Index in 0 .. The_Parameter.The_Length - 1 loop
                     The_Value :=
                       SYSInteger
                         (BCModular_At
                            (The_Index + Index * The_Parameter.The_Size,
                             The_Parameter.The_Size,
                             The_Values));
                     The_String := The_String & SYSInteger'Image (The_Value);
                  end loop;
               end if;
         end case;

         The_Index := The_Index + Size_Of (The_Parameter);
         Parameter_Vector.Next (The_Position);
      end loop;
      return To_String (The_String);
   end Format;

   function Image_Of (The_Parameter : Parameter_Type) return String is
      The_String : Unbounded_String := Null_Unbounded_String;
   begin
      case The_Parameter.The_Kind is
         when Scalar_Parameter =>

            if The_Parameter.Is_Signed then
               The_String :=
                 The_String &
                 "!" &
                 Trim (SYSInteger'Image (The_Parameter.The_Size), Left) &
                 ".";

            else
               The_String :=
                 The_String &
                 "!%" &
                 Trim (SYSInteger'Image (The_Parameter.The_Size), Left) &
                 ".";
            end if;

         when Array_Parameter =>

            if The_Parameter.Is_Signed then
               The_String :=
                 The_String &
                 "@" &
                 Trim (SYSInteger'Image (The_Parameter.The_Size), Left) &
                 "." &
                 Trim (SYSInteger'Image (The_Parameter.The_Length), Left);

            else
               The_String :=
                 The_String &
                 "@%" &
                 Trim (SYSInteger'Image (The_Parameter.The_Size), Left) &
                 "." &
                 Trim (SYSInteger'Image (The_Parameter.The_Length), Left);
            end if;
      end case;

      return To_String (The_String);
   end Image_Of;

end Parameter_Package;

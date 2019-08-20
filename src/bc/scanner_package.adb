-- BC Boolean Compiler
-- Copyright (c) 2016 Paul Estep

with Ada.Characters.Latin_1;
--
with Ada.Text_IO; use Ada.Text_IO;
--
with System_Package; use System_Package;
with Debug_Package;  use Debug_Package;
with Source_Package; use Source_Package;
with Error_Package; use Error_Package;
--

package body Scanner_Package is

   -- Reserve word table entry.

   type Reserved_Word is record
      The_String : Unbounded_String;
      The_Symbol : Symbol;
   end record;

   -- Reserve word table.

   The_Reserved_Words : array
   (1 .. NUMBER_OF_RESERVED_WORDS) of Reserved_Word :=
     ((To_Unbounded_String ("PACKAGE"), Package_Symbol),
      (To_Unbounded_String ("BODY"), Body_Symbol),
      (To_Unbounded_String ("PROCEDURE"), Procedure_Symbol),
      (To_Unbounded_String ("IN"), In_Symbol),
      (To_Unbounded_String ("OUT"), Out_Symbol),
      (To_Unbounded_String ("IS"), Is_Symbol),
      (To_Unbounded_String ("TYPE"), Type_Symbol),
      (To_Unbounded_String ("NEW"), New_Symbol),
      (To_Unbounded_String ("CONSTANT"), Constant_Symbol),
      (To_Unbounded_String ("RANGE"), Range_Symbol),
      (To_Unbounded_String ("ARRAY"), Array_Symbol),
      (To_Unbounded_String ("OF"), Of_Symbol),
      (To_Unbounded_String ("BEGIN"), Begin_Symbol),
      (To_Unbounded_String ("NOT"), Not_Symbol),
      (To_Unbounded_String ("AND"), And_Symbol),
      (To_Unbounded_String ("OR"), Or_Symbol),
      (To_Unbounded_String ("XOR"), Xor_Symbol),
      (To_Unbounded_String ("MOD"), Mod_Symbol),
      (To_Unbounded_String ("REM"), Rem_Symbol),
      (To_Unbounded_String ("NULL"), Null_Symbol),
      (To_Unbounded_String ("IF"), If_Symbol),
      (To_Unbounded_String ("THEN"), Then_Symbol),
      (To_Unbounded_String ("ELSE"), Else_Symbol),
      (To_Unbounded_String ("FOR"), For_Symbol),
      (To_Unbounded_String ("REVERSE"), Reverse_Symbol),
      (To_Unbounded_String ("LOOP"), Loop_Symbol),
      (To_Unbounded_String ("END"), End_Symbol),

      (Boolean_String, Identifier_Symbol),
      (True_String, Identifier_Symbol),
      (False_String, Identifier_Symbol));

   -- Return true if string is a reserved word.

   function Is_Reserved (The_String : Unbounded_String) return Boolean is
   begin
      for The_Index in The_Reserved_Words'Range loop

         if The_Reserved_Words (The_Index).The_String = The_String then
            return True;
         end if;
      end loop;

      return False;
   end Is_Reserved;

   -- Return index location of the reserved word in the resevered word table.
   -- Raise a critical exception if not found.

   function Index_Of (The_String : Unbounded_String) return SYSPositive is
   begin
      for The_Index in The_Reserved_Words'Range loop

         if The_Reserved_Words (The_Index).The_String = The_String then
            return The_Index;
         end if;
      end loop;

      raise Critical_Error;
   end Index_Of;

   -- Parse next symbol in source program.

   procedure Next_Symbol is
      The_Word  : Unbounded_String;
      The_Index : SYSPositive;
   begin
      loop
         The_String := Null_Unbounded_String;

         -- skip whitespaces.

         while The_Character = ' ' or
           The_Character = Ada.Characters.Latin_1.HT or
           The_Character = Ada.Characters.Latin_1.LF or
           The_Character = Ada.Characters.Latin_1.CR
         loop
            Next_Character;
         end loop;

         The_Word := Null_Unbounded_String;

         case The_Character is
            -- reserved word or identifer symbol.
            when 'A' .. 'Z' | 'a' .. 'z' =>

               Append (The_Word, The_Character);
               Next_Character;

               while The_Character in 'A' .. 'Z' or
                 The_Character in 'a' .. 'z' or
                 The_Character in '0' .. '9' or
                 The_Character = '_'
               loop

                  Append (The_Word, The_Character);
                  Next_Character;
               end loop;

               if Is_Reserved (The_Word) then
                  The_Index  := Index_Of (The_Word);
                  The_Symbol := The_Reserved_Words (The_Index).The_Symbol;
                  The_String := The_Reserved_Words (The_Index).The_String;

               else
                  The_Symbol := Identifier_Symbol;
                  The_String := The_Word;
               end if;

               The_Position := Source_Package.The_Position;
               exit;

            -- integer symbol.
            when '0' .. '9' =>

               Append (The_Word, The_Character);
               Next_Character;

               while The_Character in '0' .. '9' or The_Character = '_' loop
                  Append (The_Word, The_Character);
                  Next_Character;
               end loop;

               The_Symbol := Integer_Symbol;
               The_String := The_Word;

               The_Position := Source_Package.The_Position;
               exit;

            -- combination 2 character symbols.
            when '.' =>
               Next_Character;

               if The_Character = '.' then
                  The_Symbol := Thru_Symbol;
                  Next_Character;

               else
                  The_Symbol := Period_Symbol;
               end if;

               The_Position := Source_Package.The_Position;
               exit;

            when '<' =>
               Next_Character;

               if The_Character = '=' then
                  The_Symbol := Less_Than_Equal_Symbol;
                  Next_Character;
               else

                  The_Symbol := Less_Than_Symbol;
               end if;

               The_Position := Source_Package.The_Position;
               exit;

            when '>' =>
               Next_Character;
               if The_Character = '=' then
                  The_Symbol := Greater_Than_Equal_Symbol;
                  Next_Character;
               else
                  The_Symbol := Greater_Than_Symbol;
               end if;

               The_Position := Source_Package.The_Position;
               exit;

            when '/' =>
               Next_Character;

               if The_Character = '=' then
                  The_Symbol := Not_Equal_Symbol;
                  Next_Character;

               else
                  The_Symbol := Divide_Symbol;
               end if;

               The_Position := Source_Package.The_Position;
               exit;

            when ':' =>
               Next_Character;

               if The_Character = '=' then
                  The_Symbol := Becomes_Symbol;
                  Next_Character;
               else

                  The_Symbol := Colon_Symbol;
               end if;

               The_Position := Source_Package.The_Position;
               exit;

            when '-' =>
               Next_Character;

               if The_Character = '-' then
                  while The_Character /= Ada.Characters.Latin_1.LF and
                    The_Character /= Ada.Characters.Latin_1.CR
                  loop
                     Next_Character;
                  end loop;

                  Next_Character;
               else

                  The_Symbol   := Minus_Symbol;
                  The_Position := Source_Package.The_Position;
                  exit;
               end if;

            -- single character symbols.

            when '=' =>
               Next_Character;
               The_Symbol   := Equal_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when '(' =>
               Next_Character;
               The_Symbol   := Left_Paren_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when ')' =>
               Next_Character;
               The_Symbol   := Right_Paren_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when '+' =>
               Next_Character;
               The_Symbol   := Plus_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when '*' =>
               Next_Character;
               The_Symbol   := Times_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when ',' =>
               Next_Character;
               The_Symbol   := Comma_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when ';' =>
               Next_Character;
               The_Symbol   := Semicolon_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            when ''' =>
               Next_Character;
               The_Symbol   := Tick_Symbol;
               The_Position := Source_Package.The_Position;
               exit;

            -- if unkown symbol report an error.

            when others =>
               Source_Error (The_Position, "Unknown symbol.");
               Next_Character;

         end case;
      end loop;

      Debug (Scanner_Debug, Symbol'Image (The_Symbol));

   end Next_Symbol;

begin

   Debug (Debug_Initialization, "Scanner_Package");

end Scanner_Package;

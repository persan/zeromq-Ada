-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                ZMQ.Utilities.Stream_Element_Array_Image                   --
--                                                                           --
--                                 B o d y                                   --
--                                                                           --
--            Copyright (C) 2013-2020, per.s.sandberg@bahnhof.se             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

function ZMQ.Utilities.Stream_Element_Array_Image
  (Item : Ada.Streams.Stream_Element_Array)
   return String
is
   use Ada.Streams;
   Cursor : Natural;
   type Map_String is array (Stream_Element (0) ..
                               Stream_Element (15)) of Character;
   Hex    : constant Map_String := "0123456789ABCDEF";
begin
   return Ret : String (1 .. Item'Length * 3 - 1) do
      Cursor := Ret'First;
      for I in Item'Range loop
         Ret (Cursor) := Hex (Item (I) / 16);
         Cursor := Cursor + 1;
         Ret (Cursor) := Hex (Item (I) mod 16);
         Cursor := Cursor + 1;
         if I < Item'Last then
            Ret (Cursor) := ' ';
            Cursor := Cursor + 1;
         end if;
      end loop;
   end return;
end ZMQ.Utilities.Stream_Element_Array_Image;


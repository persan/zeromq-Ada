-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2007 Per Sandberg <per.sandberg@bredband.net>                  --
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

--                                                                          --
------------------------------------------------------------------------------
with Ada.IO_Exceptions;
with GNAT.Memory_Dump;
package body ZMQ.Utilities.Memory_Streams is

   use Ada.Streams;
   overriding
   procedure Dump
     (This        : in Memory_Stream;
      Full_Buffer : in Boolean := False;
      Outf        : in Text_IO.File_Access := Text_IO.Standard_Output) is
      pragma Unreferenced (Outf);
      Buffer : Large_Buffer_Access renames This.Buffer.As_Pointer;
   begin
      if Full_Buffer then
         GNAT.Memory_Dump.Dump
           (Buffer.all (Buffer.all'First)'Address,
            Integer (This.Buffer_Length));
      else
         GNAT.Memory_Dump.Dump
           (Buffer.all (Buffer.all'First)'Address,
            Integer (This.Cursor) - 1);
      end if;
   end Dump;

   overriding function Eof (This : in Memory_Stream) return Boolean is
   begin
      return This.Cursor > This.Buffer_Length;
   end Eof;

   -----------------
   -- Get_Address --
   -----------------

   overriding
   function Get_Address (This : in Memory_Stream) return System.Address is
   begin
      return This.Buffer.As_Address;
   end Get_Address;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (This : in Memory_Stream)
                        return Ada.Streams.Stream_Element_Count is
   begin
      return This.Buffer_Length;
   end Get_Length;

   overriding procedure Seek (This : in out Memory_Stream;
                   Pos  : in Ada.Streams.Stream_Element_Offset) is
   begin
      This.Cursor := This.Cursor + Pos;
   end Seek;

   overriding function Pos (This : in Memory_Stream)
                 return  Ada.Streams.Stream_Element_Offset is
   begin
      return This.Cursor;
   end Pos;

   -----------------
   -- Set_Address --
   -----------------

   overriding
   procedure Set_Address
     (This : in out Memory_Stream; To :  in System.Address) is
   begin
      This.Buffer.As_Address := To;
   end Set_Address;

   ----------------
   -- Set_Length --
   ----------------

   overriding
   procedure Set_Length (This : in out Memory_Stream;
                         To   : in Ada.Streams.Stream_Element_Count)  is
   begin
      This.Buffer_Length := To;
      This.Reset;
   end Set_Length;

   ----------
   -- Read --
   ----------

   overriding
   procedure Read
     (Stream : in out Memory_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      First : Stream_Element_Offset;
      LLast  : Stream_Element_Offset;
   begin
      First :=  Stream.Cursor;
      LLast := Stream.Cursor + Item'Length - 1;
      if LLast > Stream.Buffer_Length then
         raise  Ada.IO_Exceptions.End_Error;
      end if;
      Item := Stream.Buffer.As_Pointer.all (First .. LLast);
      Stream.Cursor := LLast + 1;
      Last := Item'Last;
   end Read;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (Stream : in out Memory_Stream;
      Item   : in Stream_Element_Array)
   is
      First : Stream_Element_Offset;
      Last  : Stream_Element_Offset;
   begin
      First :=  Stream.Cursor;
      Last := Stream.Cursor + Item'Length - 1;
      if Last > Stream.Buffer_Length then
         raise  Ada.IO_Exceptions.Device_Error;
      end if;
      Stream.Cursor := Last + 1;
      Stream.Buffer.As_Pointer.all (First .. Last) := Item;
   end Write;

   overriding
   procedure Reset (This : in out Memory_Stream) is
   begin
      This.Cursor := This.Buffer.As_Pointer.all'First;
   end Reset;

end ZMQ.Utilities.Memory_Streams;

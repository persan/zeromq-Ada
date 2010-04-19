------------------------------------------------------------------------------
--                                                                          --
--  Copyright 2007 Per Sandberg <per.sandberg@bredband.net>                 --
--                                                                          --
--  This package is free software; you can redistribute it and/or           --
--  modify it under terms of the GNU General Public License as              --
--  published by the Free Software Foundation; either version 2, or         --
--  (at your option) any later version. This package is distributed in      --
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without      --
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A           --
--  PARTICULAR PURPOSE. See the GNU General Public License for more         --
--  details. You should have received a copy of the GNU General Public      --
--  License distributed with this package; see file COPYING.  If not,       --
--  write to the Free Software Foundation, 59 Temple Place - Suite          --
--  330, Boston, MA 02111-1307, USA.                                        --
--                                                                          --
--  As a special exception, if other files instantiate generics from        --
--  this unit, or you link this unit with other files to produce an         --
--  executable, this unit does not by itself cause the resulting            --
--  executable to be covered by the GNU General Public License.  This       --
--  exception does not however invalidate any other reasons why the         --
--  executable file might be covered by the GNU Public License.             --
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

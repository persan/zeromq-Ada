-------------------------------------------------------------------------------
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

--                                                                          --
------------------------------------------------------------------------------
with Ada.IO_Exceptions;
with GNAT.Memory_Dump;
with System.Memory;
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
     (This   : in out Memory_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      First  : Stream_Element_Offset;
      LLast  : Stream_Element_Offset;
   begin
      First :=  This.Cursor;
      LLast := This.Cursor + Item'Length - 1;
      if LLast > This.Buffer_Length then
         raise  Ada.IO_Exceptions.End_Error;
      end if;
      Item := This.Buffer.As_Pointer.all (First .. LLast);
      This.Cursor := LLast + 1;
      Last := Item'Last;
   end Read;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write
     (This   : in out Memory_Stream;
      Item   : in Stream_Element_Array)
   is
      First : Stream_Element_Offset;
      Last  : Stream_Element_Offset;
   begin
      First :=  This.Cursor;
      Last := This.Cursor + Item'Length - 1;
      if Last > This.Buffer_Length then
         raise  Ada.IO_Exceptions.Device_Error;
      end if;
      This.Cursor := Last + 1;
      This.Buffer.As_Pointer.all (First .. Last) := Item;
   end Write;

   overriding
   procedure Reset (This : in out Memory_Stream) is
   begin
      This.Cursor := This.Buffer.As_Pointer.all'First;
   end Reset;

   procedure Read
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Memory_Stream) is
   begin
      raise Program_Error with
        "Its not possible to read into a memory stream using 'read";
   end Read;

   procedure Write
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Memory_Stream) is
   begin
      Ada.Streams.Stream_Element_Array'Write
        (This,
         Item.Buffer.As_Pointer.all
           (Item.Buffer.As_Pointer.all'First .. Item.Cursor));
   end Write;



   procedure Read
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Dynamic_Memory_Stream) is
   begin
      Read (This, Memory_Stream (Item));
   end Read;

   procedure Write
     (This   : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Dynamic_Memory_Stream) is
   begin
      Write (This, Memory_Stream (Item));
   end Write;

   procedure Write
     (This   : in out Dynamic_Memory_Stream;
      Item   : in Ada.Streams.Stream_Element_Array) is
   begin
      if This.Cursor + Item'Length > This.Buffer_Length then
         This.Expand (This.Cursor + Item'Length);
      end if;
      Memory_Stream (This).Write (Item);
   end Write;

   procedure Expand
     (This    : in out Dynamic_Memory_Stream;
      to_Size : Ada.Streams.Stream_Element_Offset) is
      new_Size : System.Memory.size_t := 0;
      use System.Memory;
   begin
      while new_Size < size_t (to_Size) loop
         case This.Strategy is
         when As_Needed =>
            new_Size := size_t (to_Size);
         when Multiply_By_Two =>
            new_Size := size_t (2 * This.Buffer_Length);
         when Add_Initial_Size =>
            new_Size := size_t (This.Buffer_Length + This.Initial_Size);
         end case;
      end loop;
      This.Buffer.As_Address :=  System.Memory.Realloc
        (This.Buffer.As_Address, new_Size);
      This.Buffer_Length := Streams.Stream_Element_Count (new_Size);
   end Expand;

   procedure Initialize (This : in out Dynamic_Memory_Stream) is
      use System.Memory;
   begin
      This.Buffer.As_Address :=
        System.Memory.Alloc (size_t (This.Initial_Size));
      This.Buffer_Length := This.Initial_Size;
   end Initialize;

   procedure Finalize   (This : in out Dynamic_Memory_Stream) is
      use System.Memory;
   begin
      System.Memory.Free (This.Buffer.As_Address);
   end Finalize;


   procedure Initialize (This : in out controler) is
      use System.Memory;
   begin
      This.controled.Initialize;
   end Initialize;

   procedure Finalize   (This : in out controler) is
   begin
      This.controled.Finalize;
   end Finalize;

end ZMQ.Utilities.Memory_Streams;

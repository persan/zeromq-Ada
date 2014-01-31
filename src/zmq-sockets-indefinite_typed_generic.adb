-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--  Z M Q . S O C K E T S . I N D E F I N I T E _ T Y P E D _ G E N E R I C  --
--                                                                           --
--                                B o d y                                    --
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
with ZMQ.Utilities.Memory_Streams;
package body ZMQ.Sockets.Indefinite_Typed_Generic is

   ----------
   -- Send --
   ----------

   procedure Send
     (This  : in out Socket;
      Msg   : Element_Type)
   is
      S    : aliased ZMQ.Utilities.Memory_Streams.Dynamic_Memory_Stream
        (Initial_Size, ZMQ.Utilities.Memory_Streams.As_Needed);
   begin
      Element_Type'Write (S'Access, Msg);
      This.Send (S.Get_Address, Integer (S.Get_Length));
      if This.Acutal_Initial_Size < S.Get_Length then
         This.Acutal_Initial_Size := S.Get_Length;
      end if;
   end Send;


   ----------
   -- Recv --
   ----------

   procedure Recv
     (This       : in Socket;
      Msg        : out Element_Type)
   is
      Temp : Messages.Message;
      S    : aliased ZMQ.Utilities.Memory_Streams.Memory_Stream;
   begin
      This.Recv (Temp);
      S.Set_Address (Temp.GetData);
      S.Set_Length (Ada.Streams.Stream_Element_Offset (Temp.GetSize));
      Element_Type'Read (S'Access, Msg);
   end Recv;

end ZMQ.Sockets.Indefinite_Typed_Generic;

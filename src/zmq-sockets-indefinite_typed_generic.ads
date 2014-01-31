-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--  Z M Q . S O C K E T S . I N D E F I N I T E _ T Y P E D _ G E N E R I C  --
--                                                                           --
--                                  S p e c                                  --
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

with Ada.Streams;
generic
   type Element_Type (<>) is private;
   Initial_Size : Ada.Streams.Stream_Element_Offset := 1024;
package ZMQ.Sockets.Indefinite_Typed_Generic is
--  This package provides a wraper for first serializeing any object
--  then send the serialized data over the socket.

   type Socket is new ZMQ.Sockets.Socket with private;

   not overriding
   procedure Send
     (This  : in out Socket;
      Msg   : Element_Type);

   not overriding
   procedure Recv
     (This       : in Socket;
      Msg        : out Element_Type);
private
   type Socket is new ZMQ.Sockets.Socket with  record
      Acutal_Initial_Size : Ada.Streams.Stream_Element_Offset := Initial_Size;
   end record;
end ZMQ.Sockets.Indefinite_Typed_Generic;

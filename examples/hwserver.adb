-------------------------------------------------------------------------------
--                   Copyright (c) 2010 Per Sandberg                         --
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

--  Hello World server in Ada
--  Binds REP socket to tcp:--*:5555
--  Expects "Hello" from client, replies with "World"


with ZMQ.Sockets;
with ZMQ.Contexts;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure HWServer is
   Context  : ZMQ.Contexts.Context;
   Socket   : ZMQ.Sockets.Socket;
   inbuffer : Ada.Strings.Unbounded.Unbounded_String;
begin
   --  Prepare our context and socket
   Socket.Initialize (Context, ZMQ.Sockets.REP);
   Socket.Bind ("tcp://*:5555");

   loop
      --  Wait for next request from client
      inbuffer := Socket.Recv;
      Put_Line ("Received request:" & To_String (inbuffer));

      --  Do some 'work'
      delay 1.0;

      --  Send reply back to client
      Socket.Send ("World");
   end loop;
end HWServer;

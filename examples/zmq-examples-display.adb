-------------------------------------------------------------------------------
--                   Copyright (c) 2011 Per Sandberg                         --
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

with ZMQ.Sockets;
with ZMQ.Contexts;
with Ada.Text_IO;
procedure ZMQ.examples.Display is
   use Ada.Text_IO;
   Context  : aliased Contexts.Context;
   Socket   : Sockets.Socket;


begin

   Context.Set_Number_Of_IO_Threads (1);
   Socket.Initialize (Context, Sockets.SUB);
   Socket.Establish_Message_Filter ("");
   Socket.Bind ("tcp://lo:5555");
   Ada.Text_IO.Put_Line ("Connected");
   Read_Loop : loop
      declare
         Buffer : constant String := Socket.Recv;
      begin
         Ada.Text_IO.Put_Line (Buffer);
         exit Read_Loop when Buffer = END_MESSAGE;
      end;
   end loop Read_Loop;
end ZMQ.Examples.Display;

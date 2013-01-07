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
with GNAT.Sockets;
procedure ZMQ.examples.prompt is

   ctx : aliased Contexts.Context;
   s   : Sockets.Socket;

begin
   s.Initialize (ctx, Sockets.PUB);
   s.Connect ("tcp://localhost:5555");

   Read_Loop : loop
      Ada.Text_IO.Put (">");
      declare
         textbuf : constant String :=  Ada.Text_IO.Get_Line;
      begin
         exit Read_Loop when textbuf'Length = 0;
         s.Send ("hej" & ASCII.NUL & GNAT.Sockets.Host_Name & ":" & textbuf);
         delay 0.02;
      end;
   end loop Read_Loop;
   s.Send (END_MESSAGE);
end ZMQ.Examples.prompt;

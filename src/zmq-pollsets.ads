-------------------------------------------------------------------------------
--            Copyright (C) 2020-2030, per.s.sandberg@bahnhof.se             --
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
with ZMQ.Low_Level;
package ZMQ.Pollsets is
   pragma Elaborate_Body;
   type Pollitem is tagged record
      Socket  : access ZMQ.Sockets.Socket;
      --  fd      : aliased int;
      --  events  : aliased short;
      --  revents : aliased short;
   end record;

   type  Pollset (Max_Size : Natural := 32) is tagged limited private;
   procedure Append (This : in out Pollset; Item : Pollitem'Class);
   procedure Remove (This : in out Pollset; Item : Pollitem'Class);

   procedure Poll (This    : in out Pollset;
                   Timeout : Duration);
private
   type Ll_Polset is array (Natural range <>)
     of aliased ZMQ.Low_Level.zmq_pollitem_t;
   type  Pollset (Max_Size : Natural := 32) is tagged limited record
      Local_Data  : aliased Ll_Polset (1 .. Max_Size);
      Cursor      : Natural := 1;
   end record;
end ZMQ.Pollsets;

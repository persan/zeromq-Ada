-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . D E V I C E S                            --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net             --
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
------------------------------------------------------------------------------

with ZMQ.Low_Level;
with Interfaces.C; use Interfaces.C;

package body ZMQ.Devices is

   ----------------
   -- initialize --
   ----------------
   type Device_Kind_map is array (Device_Kind) of int;
   Map : constant Device_Kind_map :=
           (Streamer  => Low_Level.ZMQ_STREAMER,
            Forwarder => Low_Level.ZMQ_FORWARDER,
            Queue     => Low_Level.ZMQ_QUEUE);
   procedure Initialize
     (This      : in out Device;
                         Kind      : Device_Kind;
                         In_Socket  : ZMQ.Sockets.Socket;
      Out_Ocket  : ZMQ.Sockets.Socket)
   is
   begin
      This.Impl :=
        Low_Level.zmq_device (Map (Kind),
                              In_Socket.Get_Impl,
                              Out_Ocket.Get_Impl);
   end Initialize;

end ZMQ.Devices;

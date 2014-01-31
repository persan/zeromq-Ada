-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--             Z M Q . S O C K E T S . T Y P E D _ G E N E R I C             --
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

package body ZMQ.Sockets.Typed_Generic is

   ----------
   -- Send --
   ----------

   not overriding procedure Send
     (This  : in out Typed_Socket;
      Msg   : Element_Type;
      Flags : Socket_Flags := No_Flags)
   is
   begin
      This.Send (Msg'Address, Msg'Size / 8, Flags);
   end Send;

   ----------
   -- Recv --
   ----------

   not overriding function Recv
     (This       : in Typed_Socket;
      Flags      : Socket_Flags := No_Flags)
      return Element_Type
   is
   begin
      return Ret : Element_Type do
         This.Recv (Ret, Flags);
      end return;
   end Recv;

   ----------
   -- Recv --
   ----------

   not overriding procedure Recv
     (This       : in Typed_Socket;
      Msg        : out Element_Type;
      Flags      : Socket_Flags := No_Flags)
   is
   begin
      null; -- This.Recv (Msg'Address, Msg'Size, Flags);
   end Recv;

end ZMQ.Sockets.Typed_Generic;

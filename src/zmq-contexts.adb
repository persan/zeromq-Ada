-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . C O N T E X T S                          --
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

with Interfaces.C;
with ZMQ.Low_Level;
with GNAT.OS_Lib;
with GNAT.Source_Info;
package body ZMQ.Contexts is

   use Interfaces.C;
   use System;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This        : in out Context)
   is
   begin
      Validate_Library_Version;
      This.C := Low_Level.zmq_ctx_new;
      if This.C = Null_Address then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (This : in out Context)
   is
      Rc : int;
   begin
      Rc := Low_Level.zmq_term (This.C);
      This.C := Null_Address;
      if Rc  /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Finalize;


   function GetImpl (This : Context) return System.Address is
   begin
      return This.C;
   end GetImpl;

   procedure Set_Number_Of_IO_Threads
     (This : Context; Number_Of_IO_Threads : Positive := IO_THREADS_DFLT) is
      Rc : int;
   begin
      Rc := Low_Level.zmq_ctx_set
        (context => This.GetImpl,
         option  => Low_Level.ZMQ_IO_THREADS,
         optval  => int (Number_Of_IO_Threads));
      if Rc  /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Set_Number_Of_IO_Threads;

   procedure  Set_Maximum_Number_Of_Sockets
     (This : Context; Number_Of_Sockets : Positive := MAX_SOCKETS_DFLT) is
      Rc : int;
   begin
      Rc := Low_Level.zmq_ctx_set
        (context => This.GetImpl,
         option  => Low_Level.ZMQ_MAX_SOCKETS,
         optval  => int (Number_Of_Sockets));
      if Rc  /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Set_Maximum_Number_Of_Sockets;

   function Get_Number_Of_IO_Threads (This : Context)  return Positive is
   begin
      return Ret : Positive do
         Ret := Positive
           (Low_Level.zmq_ctx_get (context => This.GetImpl,
                                   option  => Low_Level.ZMQ_IO_THREADS));
      end return;
   end Get_Number_Of_IO_Threads;

   function  Get_Maximum_Number_Of_Sockets (This : Context) return Positive is
   begin
      return Ret : Positive do
         Ret := Positive
           (Low_Level.zmq_ctx_get (context => This.GetImpl,
                                   option  => Low_Level.ZMQ_MAX_SOCKETS));
      end return;
   end Get_Maximum_Number_Of_Sockets;


end ZMQ.Contexts;

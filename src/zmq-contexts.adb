-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . C O N T E X T S                          --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
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
      if This.C /= Null_Address then
         raise ZMQ_Error with "Already Initialized";
      end if;
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
      if This.Is_Connected then
         Rc := Low_Level.zmq_ctx_destroy (This.C);
         if Rc  /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
              GNAT.Source_Info.Enclosing_Entity;
         end if;
         This.C := Null_Address;
      end if;
   end Finalize;

   function Is_Connected (This : Context) return Boolean is
   begin
      return This.C /= Null_Address;
   end Is_Connected;

   function GetImpl (This : Context) return System.Address is
   begin
      return This.C;
   end GetImpl;


   not overriding
   procedure Set_Number_Of_IO_Threads
     (This    : in out Context;
      Threads : Natural := 1) is
      Status : int;
   begin
      Status :=  Low_Level.zmq_ctx_set
        (This.C, Low_Level.Defs.ZMQ_IO_THREADS, int (Threads));
      if Status /= 0 then
         raise Program_Error with Error_Message (Integer (Status));
      end if;
   end Set_Number_Of_IO_Threads;


   not overriding
   function Get_Number_Of_IO_Threads (This : in out Context) return Natural is
   begin
      return Natural
        (Low_Level.zmq_ctx_get (This.C, Low_Level.Defs.ZMQ_IO_THREADS));
   end Get_Number_Of_IO_Threads;

   not overriding
   procedure Set_Maximum_Number_Of_Sockets
     (This : in out Context; Count : Positive := 1024) is
      status : int;
   begin
      status := Low_Level.zmq_ctx_set
        (This.C, Low_Level.Defs.ZMQ_MAX_SOCKETS, int (Count));
      if status /= 0
      then
         raise Program_Error with Error_Message (Integer (status));
      end if;
   end Set_Maximum_Number_Of_Sockets;


   not overriding
   function Get_Maximum_Number_Of_Sockets
     (This : in out Context) return Natural is
   begin
      return Natural
        (Low_Level.zmq_ctx_get (This.C, Low_Level.Defs.ZMQ_MAX_SOCKETS));
   end Get_Maximum_Number_Of_Sockets;

   not overriding
   procedure Set_IPv6
     (This : in out Context; Enable : Boolean := False) is
      Status : int;
   begin
      Status := Low_Level.zmq_ctx_set
        (This.C, Low_Level.Defs.ZMQ_IPV6, Boolean'Pos (Enable));
      if Status /= 0
      then
         raise Program_Error with Error_Message (Integer (Status));
      end if;
   end Set_IPv6;

   not overriding
   function Get_IPv6 (This : in out Context) return Boolean is
   begin
      return Low_Level.zmq_ctx_get
        (This.C, Low_Level.Defs.ZMQ_MAX_SOCKETS) = 1;
   end Get_IPv6;


end ZMQ.Contexts;

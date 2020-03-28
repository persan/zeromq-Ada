-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . C O N T E X T S                          --
--                                                                           --
--                                  S p e c                                  --
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


with Ada.Finalization;
with System;
package ZMQ.Contexts is
   IO_THREADS_DFLT  : constant := 1;
   MAX_SOCKETS_DFLT : constant := 1024;

   type Context is tagged limited private;
   type Any_Context is access all Context'Class;



   not overriding
   procedure Set_Number_Of_IO_Threads
     (This : in out Context; Threads : Natural := 1);
   not overriding
   function Get_Number_Of_IO_Threads
     (This : in out Context) return Natural;
   --  Specifies the size of the ØMQ thread pool to handle I/O operations.
   --  If your application is using only the inproc transport for messaging
   --  you may set this to zero, otherwise set it to at least one.
   --  This option only applies before creating any sockets on the context.

   not overriding
   procedure Set_Maximum_Number_Of_Sockets
     (This : in out Context; Count : Positive := 1024);
   not overriding
   function Get_Maximum_Number_Of_Sockets
     (This : in out Context) return Natural;
   --  Sets the maximum number of sockets allowed on the context.

   not overriding
   procedure Set_IPv6
     (This : in out Context; Enable : Boolean := False);
   not overriding
   function Get_IPv6 (This : in out Context) return Boolean;
   --  When IPv6 is enabled, a socket will connect to,
   --  or accept connections from, both IPv4 and IPv6 hosts.

   function Is_Connected (This : Context) return Boolean;

   function GetImpl (This : Context) return System.Address;
   --  "Private" function to get lowlevel implementation handle.

private
   type Context is new Ada.Finalization.Limited_Controlled with record
      C : System.Address := System.Null_Address;
   end record;
   overriding
   procedure Initialize (This : in out Context);

   overriding
   procedure Finalize (This : in out Context);

end ZMQ.Contexts;

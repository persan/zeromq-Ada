------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                          Z M Q . C O N T E X T S                         --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net            --
--                                                                          --
-- 0MQ Ada-binding is free software;  you can  redistribute it  and/or      --
-- modify it under terms of the  GNU General Public License as published    --
-- by the Free Soft-ware  Foundation;                                       --
-- either version 2,  or (at your option) any later version.                --
-- 0MQ Ada-binding is distributed in the hope that it will be useful, but   --
-- WITH OUT ANY WARRANTY;                                                   --
-- without even the  implied warranty of MERCHANTABILITY or                 --
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License    --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

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

   not overriding procedure Initialize
     (This        : in out Context;
      App_Threads : Positive)
   is
   begin
      Validate_Library_Version;
      if This.c /= Null_Address then
         raise ZMQ_Error with "Alredy Initialized";
      end if;
      This.c := Low_Level.zmq_init
        (int (App_Threads));
      if This.c = Null_Address then
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
      rc : int;
   begin
      if This.Is_Connected then
         rc := Low_Level.zmq_term (This.c);
         This.c := Null_Address;
         if rc  /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
            GNAT.Source_Info.Enclosing_Entity;
         end if;
      end if;
   end Finalize;

   function Is_Connected (This : Context) return Boolean is
   begin
      return This.c /= Null_Address;
   end Is_Connected;

   function GetImpl (This : Context) return System.Address is
   begin
      return This.c;
   end GetImpl;

end ZMQ.Contexts;

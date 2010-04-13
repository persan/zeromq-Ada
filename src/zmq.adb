------------------------------------------------------------------------------
--                                                                          --
--                             0MQ Ada-binding                              --
--                                                                          --
--                                   Z M Q                                  --
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

with Interfaces.C.Strings;
with ZMQ.Low_Level;
package body ZMQ is
   use Interfaces.C;
   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (no : Integer) return String is
      s : constant String := no'Img;
   begin
      return "[" &  s (s'First + 1 .. s'Last) & "] " &
      Interfaces.C.Strings.Value (Low_Level.zmq_strerror (int (no)));
   end Error_Message;

   function Library_Version return Version_Type is
   begin
      return ret : Version_Type do
         ret := (2, 0, 6); --#TODO fetch from library
      end return;
   end Library_Version;

   function image (item : Version_Type) return String is
      s1 : constant String := item.Major'Img;
      s2 : constant String := item.Minor'Img;
      s3 : constant String := item.Patch'Img;
   begin
      return s1 (s1'First + 1 .. s1'Last) & "." &
      s2 (s2'First + 1 .. s2'Last) & "." &
      s3 (s3'First + 1 .. s3'Last);
   end image;
   procedure Validate_Library_Version is
   begin
      null;
   end Validate_Library_Version;

end ZMQ;

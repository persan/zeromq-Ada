-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                                   Z M Q                                   --
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

with Ada.Task_Attributes;
with GNAT.OS_Lib;

package body ZMQ.Errors is

   package Local_Error_Attr is new Ada.Task_Attributes
     (Attribute     => Integer,
      Initial_Value => 0);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Integer is
   begin
      return Local_Error_Attr.Value;
   end Get_Last_Error;

   ------------------------
   -- Last_Error_Message --
   ------------------------

   function Last_Error_Message return String is
   begin
      return ZMQ.Error_Message (Get_Last_Error);
   end Last_Error_Message;

   --------------------
   -- Set_Last_Error --
   --------------------

   procedure Set_Last_Error (Error : Integer) is
   begin
      Local_Error_Attr.Set_Value (Error);
   end Set_Last_Error;

   ------------------
   -- Set_To_Errno --
   ------------------

   procedure Set_To_Errno is
   begin
      Local_Error_Attr.Set_Value (GNAT.OS_Lib.Errno);
   end Set_To_Errno;

end ZMQ.Errors;

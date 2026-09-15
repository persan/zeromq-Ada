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

package ZMQ.Errors is

   function Get_Last_Error return Integer;
   --  Returns the last set error value.
   --  This is a task local variable.

   function Last_Error_Message return String;
   --  Return a message describing the last error using the ZMQ library's
   --  strerror for the stored errno value.

   procedure Set_Last_Error (Error : Integer);
   --  Stores Error as last error in a task local variable.

   procedure Set_To_Errno;
   --  Stores the current C errno global variable as last error in a task local
   --  variable.

   pragma Inline (Get_Last_Error, Set_Last_Error, Set_To_Errno);

end ZMQ.Errors;

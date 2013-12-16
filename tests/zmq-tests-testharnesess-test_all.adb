-------------------------------------------------------------------------------
--                   Copyright (c) 2011 Per Sandberg                         --
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

with AUnit.Run;
--  with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

with ZMQ.Tests.TestSuits.Test_All;
with GNAT.OS_Lib;
with GNAT.IO;

--------------------------------------
-- Zmq.Tests.Testharnesess.Test_All --
--------------------------------------

procedure ZMQ.Tests.Testharnesess.Test_All is

   procedure Run is new AUnit.Run.Test_Runner
     (ZMQ.Tests.TestSuits.Test_All.Suite);
   --  Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;
   task killer is
      entry ok;
   end killer;

   task body killer is
   begin
      select
         accept ok;
      or
         delay 5.0;
         GNAT.IO.Put_Line ("Times up");
         GNAT.OS_Lib.OS_Exit (-1);
      end select;
   end killer;

begin
   Run (Reporter);
   killer.ok;
end ZMQ.Tests.TestHarnesess.Test_All;

-------------------------------------------------------------------------------
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


with GNATCOLL.JSON;
with Ada.Strings.Unbounded;
package ZMQ.Examples.JSON_Data is
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;
   type Coordinate is record
      X, Y, Z : Float;
   end record;


   type Data_Type is record
      Sensor_Name : Unbounded_String;
      OK          : Boolean := True;
      Location    : Coordinate := (-1.0, -2.0, -3.0);
      Orientation : Coordinate := (-1.2, -2.3, -3.4);
   end record;

   function Create (Val : Coordinate) return JSON_Value;
   function Create (Val : Data_Type) return JSON_Value;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Coordinate);
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Data_Type);

   procedure Cb_Coordinate
     (User_Object : in out Coordinate;
      Name        : UTF8_String;
      Value       : JSON_Value);

   procedure Cb_Data_Type
     (User_Object : in out Data_Type;
      Name        : UTF8_String;
      Value       : JSON_Value);



   procedure Read (Src : JSON_Value; Into : in out Data_Type);
   procedure Read is new Gen_Map_JSON_Object (Data_Type);
   procedure Read is new Gen_Map_JSON_Object (Coordinate);

end ZMQ.Examples.JSON_Data;

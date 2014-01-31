

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
   procedure Read is new
     Gen_Map_JSON_Object (Data_Type);
   procedure Read is new
     Gen_Map_JSON_Object (Coordinate);

end ZMQ.Examples.JSON_Data;

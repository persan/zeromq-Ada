

package body ZMQ.Examples.JSON_Data is

   ------------
   -- Create --
   ------------

   function Create (Val : Coordinate) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Ret.Set_Field ("X", Create (Val.X));
         Ret.Set_Field ("Y", Create (Val.Y));
         Ret.Set_Field ("Z", Create (Val.Z));
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Val : Data_Type) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Ret.Set_Field ("Sensor_Name", Create (Val.Sensor_Name));
         Ret.Set_Field ("OK", Create (Val.OK));
         Ret.Set_Field ("Location", Create (Val.Location));
         Ret.Set_Field ("Orientation", Create (Val.Orientation));
      end return;
   end Create;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Coordinate)
   is
   begin
      Val.Set_Field (Field_Name, Create (Field));
   end Set_Field;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Data_Type)
   is
   begin
      Val.Set_Field (Field_Name, Create (Field));
   end Set_Field;

   -------------------
   -- Cb_Long_Float --
   -------------------



   -------------------
   -- Cb_Coordinate --
   -------------------

   procedure Cb_Coordinate
     (User_Object : in out Coordinate;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      if Name = "X" then
         User_Object.X := Value.Get;
      elsif Name = "Y" then
         User_Object.Y := Value.Get;
      elsif Name = "Z" then
         User_Object.Z := Value.Get;
      end if;
   end Cb_Coordinate;


   ------------------
   -- Cb_Data_Type --
   ------------------

   procedure Cb_Data_Type
     (User_Object : in out Data_Type;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      if Name = "Sensor_Name" then
         User_Object.Sensor_Name := Value.Get;
      elsif Name = "OK" then
         User_Object.OK := Value.Get;
      elsif Name = "Location" then
         Read (Value, Cb_Coordinate'Access, User_Object.Location);
      elsif Name = "Orientation" then
         Read (Value, Cb_Coordinate'Access, User_Object.Orientation);
      end if;
   end Cb_Data_Type;

   procedure  Read (Src : JSON_Value; Into : in out Data_Type) is
   begin
      Read (Src, Cb_Data_Type'Access, Into);
   end Read;

end ZMQ.Examples.JSON_Data;

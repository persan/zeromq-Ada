
with GNAT.Strings;
with Ada.Text_IO;
procedure ZMQ.Examples.JSON_Data.Test is
   use GNAT.Strings;
   V   : Data_Type;
   Src : JSON_Value;
   Tgt : JSON_Value;
   V1  : Data_Type;
   S   : GNAT.Strings.String_Access;
begin
   V := (Sensor_Name => To_Unbounded_String ("bannme"),
         OK => True,
         Location => (1.0, 2.0, 3.0),
         Orientation => (1.1, 2.2, 3.3));

   Src := Create (V);
   S :=  new String'(Src.Write);
   Tgt := Read (S.all, "");
   Read (Tgt, V1);
   V1.OK := False;
   Ada.Text_IO.Put_Line (Create (V1).Write (Compact => False));
   Free (S);
end ZMQ.Examples.JSON_Data.Test;

function ZMQ.Utilities.Stream_Element_Array_Image
  (Item : Ada.Streams.Stream_Element_Array)
   return String
is
   use Ada.Streams;
   Cursor : Natural := 1;
   type Map_String is array (Stream_Element (0) ..
                               Stream_Element (15)) of Character;
   Hex    : constant Map_String := ('0', '1', '2', '3',
                                        '4', '5', '6', '7',
                                        '8', '9', 'A', 'B',
                                   'C', 'D', 'E', 'F');
begin
   return Ret : String (1 .. Item'Length * 2) do
      for I in Item'Range loop
         Ret (Cursor) := Hex (Item (I) / 16);
         Cursor := Cursor + 1;
         Ret (Cursor) := Hex (Item (I) mod 16);
         Cursor := Cursor + 1;
      end loop;
   end return;
end ZMQ.Utilities.Stream_Element_Array_Image;


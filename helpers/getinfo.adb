with ZMQ;
with ada.Text_IO;
procedure getinfo is
begin
   ada.Text_IO.Put_Line (zmq.image (zmq.Binding_Version));
end;

with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Communication is
   procedure Initialize_Communication is
   begin
      Initialize (COM);
      Configure (COM, Baud_Rate => 115200);
   end Initialize_Communication;

   procedure Send_Message (M : String) is
      Buf : aliased Message (Physical_Size => 1024);
   begin
      Set (Buf, To => M & Terminator);
      Put (COM, Buf'Unchecked_Access);
      Await_Transmission_Complete (Buf);
   end Send_Message;

   procedure Send_Debug (M : String) is
   begin
      Send_Message ("DEBUG: " & M);
   end Send_Debug;

   function Determine_Player_Number return Integer is
      CR : constant Character := Character'Val(13);
      LF : constant Character := Character'Val(10);

      subtype Identity_String is String (1..30);

      function Device_Id_to_Identity(Tuple : Device_Id_Tuple) return Identity_String is
         subtype Identity_Number is String (1..10);

         Ids : array (1..3) of Identity_Number;
      begin
         for I in Tuple'Range loop
            Move(Trim(Tuple(I)'Image, Ada.Strings.Left),
                 Ids(I), Justify => Right, Pad => '0');
         end loop;

         return Ids(1) & Ids(2) & Ids(3);
      end Device_Id_to_Identity;

      Own_Id : Identity_String := Device_Id_to_Identity(Unique_Id);
      Foreign_Id : Identity_String;
      Recv_Buf : aliased Message (Physical_Size => 1024);
      Received_Ack : Boolean := False;
      Received_Id : Boolean := False;
      I : Integer := 0;
   begin
      Set_Terminator (Recv_Buf, Terminator);
      Get (COM, Recv_Buf'Unchecked_Access);
      while not (Received_Ack and Received_Id) loop
         if (Received_Id) then
            Send_Message (Own_Id & "ACK");
         else
            Send_Message (Own_Id);
         end if;

         if Recv_Buf.Is_Reception_Complete then
            if Recv_Buf.Content'Length = Own_Id'Length + 3 then
               Received_Ack := True;
            elsif Recv_Buf.Content'Length = Own_Id'Length then
               Foreign_Id := Recv_Buf.Content(Foreign_Id'Range);
               Received_Id := True;
            end if;
            Get (COM, Recv_Buf'Unchecked_Access);
         end if;
         delay 0.5;
      end loop;
      for I in Own_Id'Range loop
         if Own_Id(I) /= Foreign_Id(I) then
            if Own_Id(I) < Foreign_Id(I) then
               return 1;
            else
               return 2;
            end if;
         end if;
      end loop;

      return 0;
   exception
      when E:others =>
         Send_Debug(Exception_Message(E) & CR & LF);
         raise;
   end Determine_Player_Number;
end Communication;

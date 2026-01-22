-- SPDX-License-Identifier: AGPL-3.0-or-later
-- FlatRacoon TUI - API Client implementation

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Sockets;

package body FlatRacoon.API_Client is

   Base_URL : String (1 .. 100) := (others => ' ');
   URL_Length : Natural := 0;

   procedure Initialize (Orchestrator_URL : String := "http://localhost:4000") is
   begin
      URL_Length := Orchestrator_URL'Length;
      Base_URL (1 .. URL_Length) := Orchestrator_URL;
   end Initialize;

   function HTTP_GET (Endpoint : String) return String is
      use GNAT.Sockets;

      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;
      Response : Ada.Strings.Unbounded.Unbounded_String;

      -- Parse URL to extract host and path
      Full_URL : constant String := Base_URL (1 .. URL_Length) & Endpoint;
      Host     : constant String := "localhost";
      Port     : constant Port_Type := 4000;
      Path     : constant String := Endpoint;
   begin
      -- Initialize sockets
      Initialize;

      -- Create socket
      Create_Socket (Socket);

      -- Set address
      Address.Addr := Addresses (Get_Host_By_Name (Host), 1);
      Address.Port := Port;

      -- Connect
      Connect_Socket (Socket, Address);
      Channel := Stream (Socket);

      -- Send HTTP request
      String'Write (Channel, "GET " & Path & " HTTP/1.1" & ASCII.CR & ASCII.LF);
      String'Write (Channel, "Host: " & Host & ASCII.CR & ASCII.LF);
      String'Write (Channel, "Connection: close" & ASCII.CR & ASCII.LF);
      String'Write (Channel, ASCII.CR & ASCII.LF);

      -- Read response
      declare
         Buffer : String (1 .. 4096);
         Last   : Natural;
         In_Body : Boolean := False;
      begin
         loop
            String'Read (Channel, Buffer (1 .. 100), Last);
            exit when Last = 0;

            -- Simple header/body separation
            if not In_Body then
               if Index (Buffer (1 .. Last), ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF) > 0 then
                  In_Body := True;
                  -- Extract body portion
                  declare
                     Body_Start : constant Natural :=
                        Index (Buffer (1 .. Last), ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF) + 4;
                  begin
                     if Body_Start <= Last then
                        Append (Response, Buffer (Body_Start .. Last));
                     end if;
                  end;
               end if;
            else
               Append (Response, Buffer (1 .. Last));
            end if;
         end loop;
      end;

      -- Clean up
      Close_Socket (Socket);
      Finalize;

      return To_String (Response);
   exception
      when E : Socket_Error =>
         Close_Socket (Socket);
         Finalize;
         raise Program_Error with "HTTP GET failed: " & Exception_Message (E);
      when E : others =>
         Close_Socket (Socket);
         Finalize;
         raise Program_Error with "HTTP GET error: " & Exception_Message (E);
   end HTTP_GET;

   procedure HTTP_POST (Endpoint : String; Body : String := "") is
      use GNAT.Sockets;

      Address : Sock_Addr_Type;
      Socket  : Socket_Type;
      Channel : Stream_Access;

      Host : constant String := "localhost";
      Port : constant Port_Type := 4000;
      Path : constant String := Endpoint;
   begin
      -- Initialize sockets
      Initialize;

      -- Create socket
      Create_Socket (Socket);

      -- Set address
      Address.Addr := Addresses (Get_Host_By_Name (Host), 1);
      Address.Port := Port;

      -- Connect
      Connect_Socket (Socket, Address);
      Channel := Stream (Socket);

      -- Send HTTP POST request
      String'Write (Channel, "POST " & Path & " HTTP/1.1" & ASCII.CR & ASCII.LF);
      String'Write (Channel, "Host: " & Host & ASCII.CR & ASCII.LF);
      String'Write (Channel, "Content-Type: application/json" & ASCII.CR & ASCII.LF);
      String'Write (Channel, "Content-Length:" & Body'Length'Image & ASCII.CR & ASCII.LF);
      String'Write (Channel, "Connection: close" & ASCII.CR & ASCII.LF);
      String'Write (Channel, ASCII.CR & ASCII.LF);

      if Body'Length > 0 then
         String'Write (Channel, Body);
      end if;

      -- Clean up
      Close_Socket (Socket);
      Finalize;
   exception
      when E : Socket_Error =>
         Close_Socket (Socket);
         Finalize;
         raise Program_Error with "HTTP POST failed: " & Exception_Message (E);
      when E : others =>
         Close_Socket (Socket);
         Finalize;
         raise Program_Error with "HTTP POST error: " & Exception_Message (E);
   end HTTP_POST;

   function Get_Modules return Module_List is
      JSON_Response : constant String := HTTP_GET ("/api/modules");
   begin
      return Parse_Modules_JSON (JSON_Response);
   exception
      when E : others =>
         Put_Line ("Error getting modules: " & Exception_Message (E));
         declare
            Empty : Module_List;
         begin
            return Empty;
         end;
   end Get_Modules;

   function Get_Module (Name : String) return Module_Info is
      Modules : constant Module_List := Get_Modules;
   begin
      for M of Modules loop
         if Trim (M.Name, Ada.Strings.Right) = Name then
            return M;
         end if;
      end loop;

      raise Module_Not_Found;
   end Get_Module;

   function Get_Deployment_Order return Module_Name_List is
      JSON_Response : constant String := HTTP_GET ("/api/deployment_order");
   begin
      return Parse_Order_JSON (JSON_Response);
   exception
      when E : others =>
         Put_Line ("Error getting deployment order: " & Exception_Message (E));
         declare
            Empty : Module_Name_List (1 .. 0);
         begin
            return Empty;
         end;
   end Get_Deployment_Order;

   function Get_Health return Health_Summary is
      JSON_Response : constant String := HTTP_GET ("/api/health");
      Result : Health_Summary;
   begin
      Result := Parse_Health_JSON (JSON_Response);
      return Result;
   exception
      when E : others =>
         Put_Line ("Error getting health: " & Exception_Message (E));
         Result.All_Healthy := False;
         Result.Healthy_Count := 0;
         Result.Unhealthy_Count := 0;
         Result.Unknown_Count := 0;
         return Result;
   end Get_Health;

   procedure Deploy_All is
   begin
      HTTP_POST ("/api/deploy");
      Put_Line ("✓ Deployment initiated for all modules");
   exception
      when E : others =>
         Put_Line ("✗ Deploy all failed: " & Exception_Message (E));
   end Deploy_All;

   procedure Deploy_Module (Name : String) is
   begin
      HTTP_POST ("/api/deploy/" & Name);
      Put_Line ("✓ Deployment initiated for " & Name);
   exception
      when E : others =>
         Put_Line ("✗ Deploy failed for " & Name & ": " & Exception_Message (E));
   end Deploy_Module;

   procedure Restart_Module (Name : String) is
   begin
      HTTP_POST ("/api/restart/" & Name);
      Put_Line ("✓ Restart initiated for " & Name);
   exception
      when E : others =>
         Put_Line ("✗ Restart failed for " & Name & ": " & Exception_Message (E));
   end Restart_Module;

   procedure Stop_Module (Name : String) is
   begin
      HTTP_POST ("/api/stop/" & Name);
      Put_Line ("✓ Stop initiated for " & Name);
   exception
      when E : others =>
         Put_Line ("✗ Stop failed for " & Name & ": " & Exception_Message (E));
   end Stop_Module;

   function Get_Logs (Name : String; Lines : Positive := 50) return String is
      JSON_Response : constant String :=
         HTTP_GET ("/api/logs/" & Name & "?lines=" & Lines'Image);

      -- Extract logs from JSON response
      Logs_Start : constant Natural := Index (JSON_Response, """logs"":");
   begin
      if Logs_Start = 0 then
         return "No logs available";
      end if;

      -- Simple extraction of logs value
      declare
         Value_Start : Natural := Index (JSON_Response (Logs_Start .. JSON_Response'Last), """");
         Value_End : Natural;
      begin
         if Value_Start = 0 then
            return "No logs available";
         end if;

         Value_Start := Value_Start + 1;
         Value_End := Index (JSON_Response (Value_Start .. JSON_Response'Last), """") - 1;

         if Value_End < Value_Start then
            return "No logs available";
         end if;

         return JSON_Response (Value_Start .. Value_End);
      end;
   exception
      when E : others =>
         return "Error retrieving logs: " & Exception_Message (E);
   end Get_Logs;

   function Parse_Modules_JSON (JSON : String) return Module_List is
      Result : Module_List;

      -- Simple JSON parser for modules array
      function Extract_String_Value (JSON : String; Key : String) return String is
         Key_Pos : constant Natural := Index (JSON, """" & Key & """:");
      begin
         if Key_Pos = 0 then
            return "";
         end if;

         -- Find the value after the key
         declare
            Value_Start : Natural := Index (JSON (Key_Pos .. JSON'Last), """", Key_Pos + Key'Length + 3);
            Value_End   : Natural;
         begin
            if Value_Start = 0 then
               return "";
            end if;

            Value_Start := Value_Start + 1;
            Value_End := Index (JSON (Value_Start .. JSON'Last), """") - 1;

            if Value_End < Value_Start then
               return "";
            end if;

            return JSON (Value_Start .. Value_End);
         end;
      end Extract_String_Value;

      function Extract_Int_Value (JSON : String; Key : String) return Natural is
         Val_Str : constant String := Extract_String_Value (JSON, Key);
      begin
         if Val_Str'Length = 0 then
            return 0;
         end if;
         return Natural'Value (Val_Str);
      exception
         when others =>
            return 0;
      end Extract_Int_Value;

      Modules_Start : Natural;
      Current_Pos   : Natural;
   begin
      -- Find "modules" array
      Modules_Start := Index (JSON, """modules"":[");
      if Modules_Start = 0 then
         return Result;
      end if;

      Current_Pos := Modules_Start + 11;  -- Skip past "modules":[

      -- Parse each module object
      while Current_Pos < JSON'Last loop
         declare
            Obj_Start : constant Natural := Index (JSON (Current_Pos .. JSON'Last), "{");
            Obj_End   : Natural;
            M         : Module_Info;
         begin
            exit when Obj_Start = 0;

            Obj_End := Index (JSON (Obj_Start .. JSON'Last), "}");
            exit when Obj_End = 0;

            declare
               Obj : constant String := JSON (Obj_Start .. Obj_End);
               Name_Str : constant String := Extract_String_Value (Obj, "name");
               Status_Str : constant String := Extract_String_Value (Obj, "status");
               Layer_Str : constant String := Extract_String_Value (Obj, "layer");
               Version_Str : constant String := Extract_String_Value (Obj, "version");
            begin
               -- Fill module info
               M.Name (1 .. Integer'Min (Name_Str'Length, 50)) :=
                  Name_Str (1 .. Integer'Min (Name_Str'Length, 50));
               M.Completion := Extract_Int_Value (Obj, "completion");

               -- Parse status
               if Status_Str = "running" then
                  M.Status := Running;
               elsif Status_Str = "stopped" then
                  M.Status := Stopped;
               elsif Status_Str = "pending" then
                  M.Status := Pending;
               else
                  M.Status := Error;
               end if;

               M.Layer (1 .. Integer'Min (Layer_Str'Length, 20)) :=
                  Layer_Str (1 .. Integer'Min (Layer_Str'Length, 20));
               M.Version (1 .. Integer'Min (Version_Str'Length, 20)) :=
                  Version_Str (1 .. Integer'Min (Version_Str'Length, 20));

               Result.Append (M);
            end;

            Current_Pos := Obj_End + 1;
         end;
      end loop;

      return Result;
   exception
      when others =>
         -- Return empty on parse error
         Result.Clear;
         return Result;
   end Parse_Modules_JSON;

   function Parse_Health_JSON (JSON : String) return Health_Summary is
      Result : Health_Summary;

      function Extract_Bool (JSON : String; Key : String) return Boolean is
         Key_Pos : constant Natural := Index (JSON, """" & Key & """:");
      begin
         if Key_Pos = 0 then
            return False;
         end if;

         return Index (JSON (Key_Pos .. JSON'Last), "true") > 0;
      end Extract_Bool;

      function Extract_Int (JSON : String; Key : String) return Natural is
         Key_Pos : constant Natural := Index (JSON, """" & Key & """:");
         Val_Start : Natural;
         Val_End : Natural;
      begin
         if Key_Pos = 0 then
            return 0;
         end if;

         Val_Start := Key_Pos + Key'Length + 3;
         Val_End := Val_Start;

         while Val_End <= JSON'Last and then JSON (Val_End) in '0' .. '9' loop
            Val_End := Val_End + 1;
         end loop;

         return Natural'Value (JSON (Val_Start .. Val_End - 1));
      exception
         when others =>
            return 0;
      end Extract_Int;
   begin
      Result.All_Healthy := Extract_Bool (JSON, "all_healthy");
      Result.Healthy_Count := Extract_Int (JSON, "healthy");
      Result.Unhealthy_Count := Extract_Int (JSON, "unhealthy");
      Result.Unknown_Count := Extract_Int (JSON, "unknown");

      return Result;
   exception
      when others =>
         Result.All_Healthy := False;
         Result.Healthy_Count := 0;
         Result.Unhealthy_Count := 0;
         Result.Unknown_Count := 0;
         return Result;
   end Parse_Health_JSON;

   function Parse_Order_JSON (JSON : String) return Module_Name_List is
      -- Parse deployment order array
      Order_Start : constant Natural := Index (JSON, """order"":[");
      Current_Pos : Natural;
      Count : Natural := 0;
      Names : array (1 .. 20) of String (1 .. 50);  -- Temporary storage
   begin
      if Order_Start = 0 then
         -- Return empty order
         declare
            Empty : Module_Name_List (1 .. 0);
         begin
            return Empty;
         end;
      end if;

      Current_Pos := Order_Start + 9;  -- Skip past "order":[

      -- Extract module names
      while Current_Pos < JSON'Last and Count < 20 loop
         declare
            Name_Start : constant Natural := Index (JSON (Current_Pos .. JSON'Last), """");
            Name_End : Natural;
         begin
            exit when Name_Start = 0;

            Name_End := Index (JSON (Name_Start + 1 .. JSON'Last), """");
            exit when Name_End = 0;

            Count := Count + 1;
            declare
               Name : constant String := JSON (Name_Start + 1 .. Name_End - 1);
            begin
               Names (Count) (1 .. Integer'Min (Name'Length, 50)) :=
                  Name (1 .. Integer'Min (Name'Length, 50));
            end;

            Current_Pos := Name_End + 1;
         end;
      end loop;

      -- Return array with correct size
      declare
         Result : Module_Name_List (1 .. Count);
      begin
         for I in 1 .. Count loop
            Result (I) := Names (I);
         end loop;
         return Result;
      end;
   exception
      when others =>
         declare
            Empty : Module_Name_List (1 .. 0);
         begin
            return Empty;
         end;
   end Parse_Order_JSON;

end FlatRacoon.API_Client;

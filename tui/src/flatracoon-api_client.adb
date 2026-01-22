-- SPDX-License-Identifier: AGPL-3.0-or-later
-- FlatRacoon TUI - API Client implementation

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
      -- Simplified HTTP client using GNAT.Sockets
      -- In production, use a proper HTTP library like AWS
   begin
      -- TODO: Implement actual HTTP GET
      -- For MVP, return mock data
      return "{}";
   exception
      when others =>
         raise Program_Error with "HTTP GET not yet implemented";
   end HTTP_GET;

   procedure HTTP_POST (Endpoint : String; Body : String := "") is
   begin
      -- TODO: Implement actual HTTP POST
      null;
   exception
      when others =>
         raise Program_Error with "HTTP POST not yet implemented";
   end HTTP_POST;

   function Get_Modules return Module_List is
      Result : Module_List;

      -- Mock data for MVP
      procedure Add_Module (Name : String; Status : Module_Status_Type;
                           Completion : Natural; Layer : String) is
         M : Module_Info;
      begin
         M.Name (1 .. Name'Length) := Name;
         M.Status := Status;
         M.Completion := Completion;
         M.Layer (1 .. Layer'Length) := Layer;
         M.Version (1 .. 5) := "0.1.0";
         Result.Append (M);
      end Add_Module;
   begin
      -- Return mock data until HTTP client is implemented
      Add_Module ("zerotier-k8s-link", Running, 100, "overlay");
      Add_Module ("twingate-helm-deploy", Running, 100, "access");
      Add_Module ("ipfs-overlay", Running, 100, "storage");
      Add_Module ("poly-k8s-mcp", Pending, 35, "orchestration");
      Add_Module ("poly-secret-mcp", Pending, 25, "secrets");

      return Result;
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
      -- Mock deployment order for MVP
      Order : Module_Name_List (1 .. 5);
   begin
      Order (1) (1 .. 19) := "zerotier-k8s-link  ";
      Order (2) (1 .. 21) := "twingate-helm-deploy ";
      Order (3) (1 .. 12) := "ipfs-overlay";
      Order (4) (1 .. 13) := "poly-k8s-mcp ";
      Order (5) (1 .. 15) := "poly-secret-mcp";

      return Order;
   end Get_Deployment_Order;

   function Get_Health return Health_Summary is
      Result : Health_Summary;
   begin
      -- Mock health data for MVP
      Result.All_Healthy := True;
      Result.Healthy_Count := 3;
      Result.Unhealthy_Count := 0;
      Result.Unknown_Count := 2;

      return Result;
   end Get_Health;

   procedure Deploy_All is
   begin
      Put_Line ("Deploy_All: Would POST to /api/deploy");
   end Deploy_All;

   procedure Deploy_Module (Name : String) is
   begin
      Put_Line ("Deploy_Module: Would POST to /api/deploy/" & Name);
   end Deploy_Module;

   procedure Restart_Module (Name : String) is
   begin
      Put_Line ("Restart_Module: Would POST to /api/restart/" & Name);
   end Restart_Module;

   procedure Stop_Module (Name : String) is
   begin
      Put_Line ("Stop_Module: Would POST to /api/stop/" & Name);
   end Stop_Module;

   function Get_Logs (Name : String; Lines : Positive := 50) return String is
   begin
      return "Mock logs for " & Name & " (last " & Lines'Image & " lines)";
   end Get_Logs;

   function Parse_Modules_JSON (JSON : String) return Module_List is
   begin
      -- TODO: Implement JSON parsing
      return Get_Modules;  -- Return mock for now
   end Parse_Modules_JSON;

   function Parse_Health_JSON (JSON : String) return Health_Summary is
   begin
      -- TODO: Implement JSON parsing
      return Get_Health;  -- Return mock for now
   end Parse_Health_JSON;

   function Parse_Order_JSON (JSON : String) return Module_Name_List is
   begin
      -- TODO: Implement JSON parsing
      return Get_Deployment_Order;  -- Return mock for now
   end Parse_Order_JSON;

end FlatRacoon.API_Client;

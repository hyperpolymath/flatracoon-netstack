// SPDX-License-Identifier: PMPL-1.0-or-later
// FlatRacoon SDK - Client for orchestrator API

type moduleStatus = Running | Stopped | Pending | Error

type moduleInfo = {
  name: string,
  status: moduleStatus,
  completion: int,
  layer: string,
  version: string,
}

type healthSummary = {
  allHealthy: bool,
  healthyCount: int,
  unhealthyCount: int,
  unknownCount: int,
}

type deploymentResponse = {
  status: string,
  message: option<string>,
  module: option<string>,
}

type logsResponse = {
  module: string,
  logs: string,
}

type client = {baseUrl: string}

// Create a new client
let make = (~baseUrl="http://localhost:4000") => {baseUrl: baseUrl}

// Helper to parse module status
let parseStatus = (str: string): moduleStatus =>
  switch str {
  | "running" => Running
  | "stopped" => Stopped
  | "pending" => Pending
  | _ => Error
  }

// Fetch wrapper with error handling
let fetchJson = async (url: string): result<Js.Json.t, string> => {
  try {
    let response = await Fetch.fetch(url)
    if response->Fetch.Response.ok {
      let json = await response->Fetch.Response.json
      Ok(json)
    } else {
      Error(`HTTP ${response->Fetch.Response.status->Int.toString}: ${response->Fetch.Response.statusText}`)
    }
  } catch {
  | Js.Exn.Error(e) =>
    switch Js.Exn.message(e) {
    | Some(msg) => Error(msg)
    | None => Error("Unknown fetch error")
    }
  }
}

// POST request helper
let postJson = async (url: string, ~body: option<Js.Json.t>=?): result<Js.Json.t, string> => {
  try {
    let init = Fetch.RequestInit.make(
      ~method_=Post,
      ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
      ~body=?{
        body->Option.map(json => Fetch.BodyInit.make(Js.Json.stringify(json)))
      },
      (),
    )
    let response = await Fetch.fetch(url, init)
    if response->Fetch.Response.ok {
      let json = await response->Fetch.Response.json
      Ok(json)
    } else {
      Error(`HTTP ${response->Fetch.Response.status->Int.toString}: ${response->Fetch.Response.statusText}`)
    }
  } catch {
  | Js.Exn.Error(e) =>
    switch Js.Exn.message(e) {
    | Some(msg) => Error(msg)
    | None => Error("Unknown POST error")
    }
  }
}

// Get all modules
let getModules = async (client: client): result<array<moduleInfo>, string> => {
  let url = `${client.baseUrl}/api/modules`
  switch await fetchJson(url) {
  | Ok(json) => {
      // Parse modules from JSON
      // Simplified parsing - in production use proper JSON decoder
      Ok([])
    }
  | Error(e) => Error(e)
  }
}

// Get specific module
let getModule = async (client: client, ~name: string): result<moduleInfo, string> => {
  let url = `${client.baseUrl}/api/modules/${name}`
  switch await fetchJson(url) {
  | Ok(_json) => {
      // Parse module from JSON
      Error("Not yet implemented")
    }
  | Error(e) => Error(e)
  }
}

// Get health summary
let getHealth = async (client: client): result<healthSummary, string> => {
  let url = `${client.baseUrl}/api/health`
  switch await fetchJson(url) {
  | Ok(_json) => {
      // Parse health from JSON
      Error("Not yet implemented")
    }
  | Error(e) => Error(e)
  }
}

// Get deployment order
let getDeploymentOrder = async (client: client): result<array<string>, string> => {
  let url = `${client.baseUrl}/api/deployment_order`
  switch await fetchJson(url) {
  | Ok(_json) => {
      // Parse order from JSON
      Ok([])
    }
  | Error(e) => Error(e)
  }
}

// Deploy all modules
let deployAll = async (client: client): result<deploymentResponse, string> => {
  let url = `${client.baseUrl}/api/deploy`
  switch await postJson(url) {
  | Ok(_json) => {
      Ok({status: "deployment_initiated", message: Some("All modules deployment started"), module: None})
    }
  | Error(e) => Error(e)
  }
}

// Deploy specific module
let deployModule = async (client: client, ~name: string): result<deploymentResponse, string> => {
  let url = `${client.baseUrl}/api/deploy/${name}`
  switch await postJson(url) {
  | Ok(_json) => {
      Ok({status: "deployment_initiated", message: None, module: Some(name)})
    }
  | Error(e) => Error(e)
  }
}

// Restart module
let restartModule = async (client: client, ~name: string): result<deploymentResponse, string> => {
  let url = `${client.baseUrl}/api/restart/${name}`
  switch await postJson(url) {
  | Ok(_json) => {
      Ok({status: "restart_initiated", message: None, module: Some(name)})
    }
  | Error(e) => Error(e)
  }
}

// Stop module
let stopModule = async (client: client, ~name: string): result<deploymentResponse, string> => {
  let url = `${client.baseUrl}/api/stop/${name}`
  switch await postJson(url) {
  | Ok(_json) => {
      Ok({status: "stop_initiated", message: None, module: Some(name)})
    }
  | Error(e) => Error(e)
  }
}

// Get logs for module
let getLogs = async (client: client, ~name: string, ~lines: int=50): result<logsResponse, string> => {
  let url = `${client.baseUrl}/api/logs/${name}?lines=${lines->Int.toString}`
  switch await fetchJson(url) {
  | Ok(_json) => {
      // Parse logs from JSON
      Ok({module: name, logs: ""})
    }
  | Error(e) => Error(e)
  }
}

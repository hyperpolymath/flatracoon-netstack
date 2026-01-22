# SPDX-License-Identifier: PMPL-1.0-or-later
# Module Registry - Tracks all FlatRacoon stack components

defmodule FlatracoonOrchestrator.ModuleRegistry do
  @moduledoc """
  Central registry for all FlatRacoon stack modules.
  Tracks deployment state, health status, and capabilities.
  """

  use GenServer
  require Logger

  @type module_status :: :not_deployed | :deploying | :healthy | :degraded | :failed
  @type module_layer ::
          :access
          | :overlay
          | :storage
          | :network
          | :naming
          | :backbone
          | :platform
          | :observability
          | :mcp

  @type module_manifest :: %{
          name: String.t(),
          version: String.t(),
          layer: module_layer(),
          repo: String.t(),
          requires: [String.t()],
          provides: [String.t()],
          config_schema: String.t() | nil,
          health_endpoint: String.t() | nil,
          metrics_endpoint: String.t() | nil,
          deployment_mode: :helm | :kubectl | :daemonset | :statefulset,
          namespace: String.t()
        }

  @type module_state :: %{
          manifest: module_manifest(),
          status: module_status(),
          last_health_check: DateTime.t() | nil,
          error_message: String.t() | nil,
          metrics: map()
        }

  ## Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc """
  Register a module with its manifest.
  """
  @spec register_module(module_manifest()) :: :ok | {:error, term()}
  def register_module(manifest) do
    GenServer.call(__MODULE__, {:register, manifest})
  end

  @doc """
  Get all registered modules.
  """
  @spec list_modules() :: [module_state()]
  def list_modules do
    GenServer.call(__MODULE__, :list_modules)
  end

  @doc """
  Get modules by layer.
  """
  @spec list_modules_by_layer(module_layer()) :: [module_state()]
  def list_modules_by_layer(layer) do
    GenServer.call(__MODULE__, {:list_by_layer, layer})
  end

  @doc """
  Get a specific module by name.
  """
  @spec get_module(String.t()) :: {:ok, module_state()} | {:error, :not_found}
  def get_module(name) do
    GenServer.call(__MODULE__, {:get_module, name})
  end

  @doc """
  Update module status.
  """
  @spec update_status(String.t(), module_status(), String.t() | nil) :: :ok
  def update_status(name, status, error_message \\ nil) do
    GenServer.cast(__MODULE__, {:update_status, name, status, error_message})
  end

  @doc """
  Update module health check timestamp.
  """
  @spec record_health_check(String.t(), map()) :: :ok
  def record_health_check(name, metrics \\ %{}) do
    GenServer.cast(__MODULE__, {:record_health_check, name, metrics})
  end

  @doc """
  Get dependency graph (topologically sorted deployment order).
  """
  @spec deployment_order() :: {:ok, [String.t()]} | {:error, :circular_dependency}
  def deployment_order do
    GenServer.call(__MODULE__, :deployment_order)
  end

  ## Server Callbacks

  @impl true
  def init(_) do
    # Initial state: map of module_name => module_state
    state = load_default_manifests()
    Logger.info("ModuleRegistry initialized with #{map_size(state)} modules")
    {:ok, state}
  end

  @impl true
  def handle_call({:register, manifest}, _from, state) do
    name = manifest.name

    module_state = %{
      manifest: manifest,
      status: :not_deployed,
      last_health_check: nil,
      error_message: nil,
      metrics: %{}
    }

    new_state = Map.put(state, name, module_state)
    Logger.info("Registered module: #{name} (layer: #{manifest.layer})")
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:list_modules, _from, state) do
    modules = Map.values(state)
    {:reply, modules, state}
  end

  @impl true
  def handle_call({:list_by_layer, layer}, _from, state) do
    modules =
      state
      |> Map.values()
      |> Enum.filter(&(&1.manifest.layer == layer))

    {:reply, modules, state}
  end

  @impl true
  def handle_call({:get_module, name}, _from, state) do
    case Map.get(state, name) do
      nil -> {:reply, {:error, :not_found}, state}
      module -> {:reply, {:ok, module}, state}
    end
  end

  @impl true
  def handle_call(:deployment_order, _from, state) do
    # Build dependency graph and return topological sort
    case topological_sort(state) do
      {:ok, order} -> {:reply, {:ok, order}, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_cast({:update_status, name, status, error_message}, state) do
    new_state =
      Map.update(state, name, nil, fn module_state ->
        if module_state do
          %{module_state | status: status, error_message: error_message}
        end
      end)

    Logger.info("Module #{name} status: #{status}")
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:record_health_check, name, metrics}, state) do
    new_state =
      Map.update(state, name, nil, fn module_state ->
        if module_state do
          %{
            module_state
            | last_health_check: DateTime.utc_now(),
              metrics: metrics
          }
        end
      end)

    {:noreply, new_state}
  end

  ## Private Functions

  defp load_default_manifests do
    # Load manifests from ../modules/* directories
    %{
      "twingate-helm-deploy" => %{
        manifest: %{
          name: "twingate-helm-deploy",
          version: "0.1.0",
          layer: :access,
          repo: "https://github.com/hyperpolymath/twingate-helm-deploy",
          requires: [],
          provides: ["secure-access", "zero-trust-network"],
          config_schema: "configs/production.ncl",
          health_endpoint: nil,
          metrics_endpoint: nil,
          deployment_mode: :helm,
          namespace: "twingate-system"
        },
        status: :not_deployed,
        last_health_check: nil,
        error_message: nil,
        metrics: %{}
      },
      "zerotier-k8s-link" => %{
        manifest: %{
          name: "zerotier-k8s-link",
          version: "0.1.0",
          layer: :overlay,
          repo: "https://github.com/hyperpolymath/zerotier-k8s-link",
          requires: [],
          provides: ["overlay-network", "encrypted-mesh"],
          config_schema: "configs/network.ncl",
          health_endpoint: nil,
          metrics_endpoint: nil,
          deployment_mode: :daemonset,
          namespace: "zerotier-system"
        },
        status: :not_deployed,
        last_health_check: nil,
        error_message: nil,
        metrics: %{}
      },
      "ipfs-overlay" => %{
        manifest: %{
          name: "ipfs-overlay",
          version: "0.1.0",
          layer: :storage,
          repo: "https://github.com/hyperpolymath/ipfs-overlay",
          requires: ["zerotier-k8s-link"],
          provides: ["distributed-storage", "ipfs-cluster"],
          config_schema: "configs/ipfs.ncl",
          health_endpoint: nil,
          metrics_endpoint: nil,
          deployment_mode: :statefulset,
          namespace: "ipfs-system"
        },
        status: :not_deployed,
        last_health_check: nil,
        error_message: nil,
        metrics: %{}
      }
    }
  end

  defp topological_sort(state) do
    # Build adjacency list: module -> [dependencies]
    graph =
      state
      |> Enum.map(fn {name, module_state} ->
        {name, module_state.manifest.requires}
      end)
      |> Enum.into(%{})

    # Kahn's algorithm for topological sort
    in_degree =
      graph
      |> Enum.reduce(%{}, fn {node, deps}, acc ->
        acc = Map.put_new(acc, node, 0)

        Enum.reduce(deps, acc, fn dep, acc2 ->
          Map.update(acc2, dep, 1, &(&1 + 1))
        end)
      end)

    queue =
      in_degree
      |> Enum.filter(fn {_node, degree} -> degree == 0 end)
      |> Enum.map(fn {node, _degree} -> node end)

    do_topological_sort(graph, in_degree, queue, [])
  end

  defp do_topological_sort(_graph, _in_degree, [], result) do
    {:ok, Enum.reverse(result)}
  end

  defp do_topological_sort(graph, in_degree, [node | queue], result) do
    # Process node
    new_result = [node | result]

    # Get neighbors (modules that depend on this node)
    neighbors =
      graph
      |> Enum.filter(fn {_name, deps} -> node in deps end)
      |> Enum.map(fn {name, _deps} -> name end)

    # Decrease in-degree for neighbors
    {new_in_degree, new_queue} =
      Enum.reduce(neighbors, {in_degree, queue}, fn neighbor, {deg_acc, queue_acc} ->
        new_degree = Map.get(deg_acc, neighbor, 1) - 1
        deg_acc = Map.put(deg_acc, neighbor, new_degree)

        if new_degree == 0 do
          {deg_acc, [neighbor | queue_acc]}
        else
          {deg_acc, queue_acc}
        end
      end)

    do_topological_sort(graph, new_in_degree, new_queue, new_result)
  end
end

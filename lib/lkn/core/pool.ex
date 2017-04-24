#
# lkn.core: an entity-component-system (ecs) implementation for lyxan
# Copyright (C) 2017 Thomas Letan <contact@thomasletan.fr>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

defmodule Lkn.Core.Pool.Supervisor do
  @moduledoc false

  use Supervisor

  @name Lkn.Core.Pool.Supervisor

  def start_link(_arg \\ []) do
    Supervisor.start_link(__MODULE__, [], name: @name)
  end

  def init(_) do
    children = [
      supervisor(Lkn.Core.Pool, [])
    ]

    supervise(children, strategy: :simple_one_for_one)
  end

end

defmodule Lkn.Core.Pool.GenServer do
  @moduledoc false

  require Logger

  alias Lkn.Core.Entity
  alias Lkn.Core.Instance
  alias Lkn.Core.Name
  alias Lkn.Core.Puppeteer

  use GenServer

  defmodule State do
    @moduledoc false

    defstruct [
      :map_key,
      :instances,
    ]

    @type t :: %State{
      map_key: Entity.t,
      instances: [Instance.k],
    }

    @spec new(Entity.t) :: t
    def new(map_key) do
      %State{
        map_key: map_key,
        instances: [],
      }
    end

    @spec supervise?(t, Instance.k) :: boolean
    def supervise?(state, instance_key) do
      Enum.member?(state.instances, instance_key)
    end

    @spec add_instance(t, Instance.k) :: t
    def add_instance(state, instance_key) do
      %State{state|instances: [instance_key|state.instances]}
    end

    @spec remove_instance(t, Instance.k) :: t
    def remove_instance(state, instance_key) do
      %State{state|instances: List.delete(state.instances, instance_key)}
    end

    @spec find_instance(t, Puppeteer.k, Puppeteer.m) :: {t, Instance.k}
    def find_instance(state, puppeteer_key, puppeteer_mod) do
      find_instance_or_spawn(state, state.instances, puppeteer_key, puppeteer_mod)
    end

    @spec find_instance_or_spawn(t, [Instance.k], Puppeteer.k, Puppeteer.m) :: {t, Instance.k}
    defp find_instance_or_spawn(state, [instance_key|instances], puppeteer_key, puppeteer_mod) do
      if Instance.register_puppeteer(instance_key, puppeteer_key, puppeteer_mod) do
        {state, instance_key}
      else
        find_instance_or_spawn(state, instances, puppeteer_key, puppeteer_mod)
      end
    end
    defp find_instance_or_spawn(state, [], puppeteer_key, puppeteer_mod) do
      instance_key = UUID.uuid4()
      {:ok, _} = Instance.spawn_instance(state.map_key, instance_key)
      if !Instance.register_puppeteer(instance_key, puppeteer_key, puppeteer_mod) do
        Logger.error("[lkn.core]: [Pool(#{inspect(state.map_key)})] spawns an instance that refuses to register a new puppeteer")
        raise "New spawned instance refuses to register a new puppeteer"
      end

      {add_instance(state, instance_key), instance_key}
    end
  end

  @doc false
  def start_link(map_key) do
    GenServer.start_link(__MODULE__, map_key, name: Name.pool(map_key))
  end

  def init(map_key) do
    {:ok, State.new(map_key)}
  end

  def handle_call({:register, puppeteer_key, puppeteer_module}, _from, state) do
    {state, instance_key} = State.find_instance(state, puppeteer_key, puppeteer_module)
    {:reply, instance_key, state}
  end

  def handle_cast({:kill_request, instance_key}, state) do
    if State.supervise?(state, instance_key) do
      if Instance.kill(instance_key) do
        {:noreply, State.remove_instance(state, instance_key)}
      else
        {:noreply, state}
      end
    else
      Logger.warn("[lkn.core]: [Pool(#{inspect state.map_key})] receives a request to kill [Instance(#{inspect instance_key})] but it does not supervise it.")

      {:noreply, state}
    end
  end
end

defmodule Lkn.Core.Pool do
  use Supervisor

  alias Lkn.Core.Name

  def start_link(map_key) do
    Supervisor.start_link(__MODULE__, map_key)
  end

  def init(map_key) do
    children = [
      supervisor(Lkn.Core.Instance.Supervisor,  [map_key]),
      worker(Lkn.Core.Pool.GenServer, [map_key]),
    ]

    supervise(children, strategy: :one_for_all)
  end

  @spec kill_request(Entity.t, Instance.k) :: :ok
  def kill_request(map_key, instance_key) do
    GenServer.cast(Name.pool(map_key), {:kill_request, instance_key})
  end

  @spec register_puppeteer(Entity.t, Puppeteer.k, Puppeteer.m) :: Instance.k
  def register_puppeteer(map_key, puppeteer_key, puppeteer_module) do
    instance_key = GenServer.call(Name.pool(map_key), {:register, puppeteer_key, puppeteer_module})

    Registry.register(Lkn.Core.Notifier, Name.notify_group(instance_key), puppeteer_module)

    instance_key
  end

  @spec spawn_pool(Entity.t) :: :ok
  def spawn_pool(map_key) do
    {:ok, _} = Supervisor.start_child(Lkn.Core.Pool.Supervisor, [map_key])
    :ok
  end
end

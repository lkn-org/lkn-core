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
defmodule Lkn.Core.Instance.Supervisor do
  @moduledoc false

  use Supervisor

  alias Lkn.Core.Name

  def start_link(map_key) do
    Supervisor.start_link(__MODULE__, map_key, name: Name.instance_sup(map_key))
  end

  def init(map_key) do
    children = [
      worker(Lkn.Core.Instance, [map_key])
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end

defmodule Lkn.Core.Instance do
  use Lkn.Foundation

  alias Lkn.Core.Entity
  alias Lkn.Core.Name
  alias Lkn.Core.Pool
  alias Lkn.Core.Puppeteer
  alias Lkn.Core.System
  alias Lkn.Foundation.Recipe

  @type t :: any

  defmodule State do
    @moduledoc false

    defstruct [
      :mode,
      :map_key,
      :instance_key,
      :puppeteers,
    ]

    @type mode :: :running|{:zombie, Recipe.t}

    @type t :: %State {
      mode: mode,
      map_key: Entity.t,
      instance_key: Instance.t,
      puppeteers: MapSet.t(Puppeteer.t),
    }

    @spec new(Entity.t, Instance.t) :: t
    def new(map_key, instance_key) do
      %State{
        mode:       :running,
        map_key:    map_key,
        instance_key: instance_key,
        puppeteers: MapSet.new(),
      }
    end

    @spec zombify(t) :: t
    def zombify(state) do
      if MapSet.size(state.puppeteers) == 0 do
        case Entity.read(state.map_key, :delay) do
          Option.some(delay) ->
            {:ok, timer} = Recipe.start_link(state.map_key)
            callback = Option.some(&(Pool.kill_request(&1, state.instance_key)))
            timer
            |> Recipe.set_duration(delay, callback)
            |> Recipe.start
            %State{state|mode: {:zombie, timer}}
          Option.none() ->
            state
        end
      else
        state
      end
    end

    @spec full?(t) :: boolean
    def full?(state) do
      case Entity.read(state.map_key, :limit) do
        Option.some(limit) -> MapSet.size(state.puppeteers) == limit
        Option.none() -> false
      end
    end

    @spec register_puppeteer(t, Puppeteer.t) :: t
    def register_puppeteer(state, puppeteer_key) do
      state =  %State{state|puppeteers: MapSet.put(state.puppeteers, puppeteer_key)}
      case state.mode do
        {:zombie, timer} ->
            Recipe.cancel(timer)
            %State{state|mode: :running}
        _ -> state
      end
    end

    @spec unregister_puppeteer(t, Puppeteer.t) :: t
    def unregister_puppeteer(state, puppeteer_key) do
      %State{state|puppeteers: MapSet.delete(state.puppeteers, puppeteer_key)}
      |> zombify
    end
  end

  use GenServer

  @doc false
  @spec start_link(Entity.t, t) :: GenServer.on_start
  def start_link(map_key, instance_key) do
    GenServer.start_link(__MODULE__, [map: map_key, instance: instance_key], name: Name.instance(instance_key))
  end

  def spawn_instance(map_key, instance_key) do
    Supervisor.start_child(Name.instance_sup(map_key), [instance_key])
  end

  def init(map: map_key, instance: instance_key) do
    sys_list = Entity.systems(map_key)

    _ = Enum.map(sys_list, fn sys ->
      System.start_link(sys, instance_key, map_key)
    end)

    {:ok, State.new(map_key, instance_key)}
  end

  @spec register_entity(t, Entity.t) :: :ok
  def register_entity(instance_key, entity_key) do
    sys_list = Entity.systems(entity_key)

    _ = Enum.map(sys_list, fn sys ->
      try do
        Lkn.Core.System.register_entity(instance_key, sys, entity_key)
      rescue
        _ -> ()
      end
    end)

    :ok
  end

  @spec unregister_entity(t, Entity.t) :: :ok
  def unregister_entity(instance_key, entity_key) do
    sys_list = Entity.systems(entity_key)

    _ = Enum.map(sys_list, fn sys ->
      try do
        Lkn.Core.System.unregister_entity(instance_key, sys, entity_key)
      rescue
        _ -> ()
      end
    end)

    :ok
  end

  @doc false
  @spec register_puppeteer(t, Puppeteer.t) :: boolean
  def register_puppeteer(instance_key, puppeteer_key) do
    GenServer.call(Name.instance(instance_key), {:register_puppeteer, puppeteer_key})
  end

  @doc false
  @spec kill(t) :: boolean
  def kill(instance_key) do
    GenServer.call(Name.instance(instance_key), :killme)
  end

  @spec unregister_puppeteer(t, Puppeteer.t) :: :ok
  def unregister_puppeteer(instance_key, puppeteer_key) do
    GenServer.cast(Name.instance(instance_key), {:unregister_puppeteer, puppeteer_key})
  end

  def handle_call({:register_puppeteer, puppeteer_key}, _from, state) do
    if !State.full?(state) do
      {:reply, true, State.register_puppeteer(state, puppeteer_key)}
    else
      {:reply, false, state}
    end
  end
  def handle_call(:killme, _from, state) do
    case state.mode do
      :running -> {:reply, false, state}
      {:zombie, _} -> {:stop, :normal, true, state}
    end
  end

  def handle_cast({:unregister_puppeteer, puppeteer_key}, state) do
    {:noreply, State.unregister_puppeteer(state, puppeteer_key)}
  end
end

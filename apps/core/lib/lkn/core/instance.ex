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

  @type t :: any

  defmodule State do
    @moduledoc false

    defstruct [
      :locked,
      :mode,
      :map_key,
      :instance_key,
      :puppeteers,
    ]

    @type mode :: :running|{:zombie, Option.t(Beacon.t)}

    @type t :: %State {
      locked: boolean,
      mode: mode,
      map_key: Entity.t,
      instance_key: Instance.t,
      puppeteers: %{Puppeteer.t => Puppeteer.m},
    }

    @spec new(Entity.t, Instance.t) :: t
    def new(map_key, instance_key) do
      %State{
        locked:     false,
        mode:       :running,
        map_key:    map_key,
        instance_key: instance_key,
        puppeteers: Map.new(),
      }
    end

    @spec lock(t) :: t
    def lock(state) do
      %State{state|locked: true}
    end

    @spec kick_all(t) :: :ok
    def kick_all(state) do
      Enum.map(state.puppeteers, fn {key, mod} ->
        mod.force_unregister(key, from: state.instance_key)
      end)

      :ok
    end

    @spec zombify(t) :: t
    def zombify(state) do
      if Map.size(state.puppeteers) == 0 do
        case Entity.read(state.map_key, :delay) do
          Option.some(delay) ->
            if !state.locked do
              {:ok, timer} = Beacon.start_link(state.map_key)

              timer
              |> Beacon.set_duration(delay,
                                     &(Pool.kill_request(&1, state.instance_key)))
              |> Beacon.enable

              %State{state|mode: {:zombie, Option.some(timer)}}
            else
              # we have been locked so nobody will be able to join the instance again,
              # there is no need to wait.
              Pool.kill_request(state.map_key, state.instance_key)

              %State{state|mode: {:zombie, Option.none()}}
            end
          Option.none() ->
            # there is no specified delay, so we can die right now
            Pool.kill_request(state.map_key, state.instance_key)

            %State{state|mode: {:zombie, Option.none()}}
        end
      else
        state
      end
    end

    @spec closed?(t) :: boolean
    def closed?(state) do
      state.locked || case Entity.read(state.map_key, :limit) do
                        Option.some(limit) -> Map.size(state.puppeteers) == limit
                        Option.none() -> false
                      end
    end

    @spec register_puppeteer(t, Puppeteer.t, Puppeteer.m) :: t
    def register_puppeteer(state, puppeteer_key, puppeteer_module) do
      state =  %State{state|puppeteers: Map.put(state.puppeteers, puppeteer_key, puppeteer_module)}
      case state.mode do
        {:zombie, Option.some(timer)} ->
            Beacon.cancel(timer)
            %State{state|mode: :running}
        _ -> state
      end
    end

    @spec unregister_puppeteer(t, Puppeteer.t) :: t
    def unregister_puppeteer(state, puppeteer_key) do
      %State{state|puppeteers: Map.delete(state.puppeteers, puppeteer_key)}
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
    sys_map = Entity.systems(map_key)

    _ = Enum.map(sys_map, fn {sys, comp} ->
      {:ok, _} = System.start_link(sys, instance_key, map_key, comp)
    end)

    {:ok, State.new(map_key, instance_key)}
  end

  @spec register_entity(t, Entity.t) :: :ok
  def register_entity(instance_key, entity_key) do
    sys_map = Entity.systems(entity_key)

    _ = Enum.map(sys_map, fn {sys, _} ->
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
    sys_map = Entity.systems(entity_key)

    _ = Enum.map(sys_map, fn {sys, _} ->
      try do
        Lkn.Core.System.unregister_entity(instance_key, sys, entity_key)
      rescue
        _ -> ()
      end
    end)

    :ok
  end

  @doc false
  @spec register_puppeteer(t, Puppeteer.t, Puppeteer.m) :: boolean
  def register_puppeteer(instance_key, puppeteer_key, puppeteer_module) do
    GenServer.call(Name.instance(instance_key), {:register_puppeteer, puppeteer_key, puppeteer_module})
  end

  @doc """
  After `lock/1` returns, the Instance refuses to register new puppeteers.
  """
  @spec lock(t) :: :ok
  def lock(instance_key) do
    GenServer.call(Name.instance(instance_key), :lock)
    :ok
  end

  @doc """
  After `kick_all/1` returns, the Instance is empty.

  If `lock/1` has been called before, the Instance should quickly
  exit.
  """
  @spec kick_all(t) :: :ok
  def kick_all(instance_key) do
    GenServer.cast(Name.instance(instance_key), :kick_all)
  end

  @doc false
  @spec kill(t) :: boolean
  def kill(instance_key) do
    GenServer.call(Name.instance(instance_key), :killme)
  end

  @spec unregister_puppeteer(t, Puppeteer.t) :: :ok
  def unregister_puppeteer(instance_key, puppeteer_key) do
    GenServer.cast(Name.instance(instance_key), {:unregister_puppeteer, puppeteer_key})

    Registry.unregister(Lkn.Core.Notifier, Name.notify_group(instance_key))
  end

  def handle_call(:lock, _from, state) do
    {:reply, :ok, State.lock(state)}
  end
  def handle_call({:register_puppeteer, puppeteer_key, puppeteer_module}, _from, state) do
    if !State.closed?(state) do
      {:reply, true, State.register_puppeteer(state, puppeteer_key, puppeteer_module)}
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

  def handle_cast(:kick_all, state) do
    State.kick_all(state)
    {:noreply, state}
  end
  def handle_cast({:unregister_puppeteer, puppeteer_key}, state) do
    {:noreply, State.unregister_puppeteer(state, puppeteer_key)}
  end
end

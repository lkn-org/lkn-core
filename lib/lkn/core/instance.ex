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
      worker(Lkn.Core.Instance, [map_key], restart: :transient)
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end

defmodule Lkn.Core.Instance do
  @moduledoc """
  A Process to arbitrate the Systems of a given Map.

  There are two things to remember about Instances:

  1. There can be several Instances of a single Map
  2. They are dynamically created by the `Lkn.Core.Pool` of a Map when required

  In other words, it is not the developer job to spawn an Instance.  You can see
  an instance as the entry point of a game scene. It acts as a proxy between
  `Lkn.Core.Puppeteer`s and `Lkn.Core.System`s.

  An Instance will stay alive as long as it hosts at least one
  `Lkn.Core.Puppeteer`. Once it has became empty, it warns its `Lkn.Core.Pool`
  so that the Pool can kill it.
  """
  use Lkn.Prelude

  alias Lkn.Core.Entity
  alias Lkn.Core.Puppet
  alias Lkn.Core, as: L
  alias Lkn.Core.Name
  alias Lkn.Core.Pool
  alias Lkn.Core.Puppeteer
  alias Lkn.Core.System

  @typedoc """
  A key to identify and reach an Instance.
  """
  @type k :: any

  defmodule State do
    @moduledoc false

    defstruct [
      :locked,
      :mode,
      :map_key,
      :instance_key,
      :puppeteers,
      :puppets,
    ]

    @type mode :: :running|{:zombie, Option.t(Beacon.t)}

    @type t :: %State {
      locked: boolean,
      mode: mode,
      map_key: L.Map.k,
      instance_key: Instance.k,
      puppeteers: %{Puppeteer.k => Puppeteer.m},
      puppets: MapSet.t,
    }

    @spec new(L.Map.k, Instance.k) :: t
    def new(map_key, instance_key) do
      %State{
        locked:       false,
        mode:         :running,
        map_key:      map_key,
        instance_key: instance_key,
        puppeteers:   Map.new(),
        puppets:      MapSet.new(),
      }
    end

    @spec add_puppet(t, L.Puppet.k) :: t
    def add_puppet(state, puppet) do
      %State{state|puppets: MapSet.put(state.puppets, puppet)}
    end

    @spec delete_puppet(t, L.Puppet.k) :: t
    def delete_puppet(state, puppet) do
      %State{state|puppets: MapSet.delete(state.puppets, puppet)}
    end

    @spec lock(t) :: t
    def lock(state) do
      %State{state|locked: true}
    end

    @spec kick_all(t) :: :ok
    def kick_all(state) do
      Enum.map(state.puppeteers, fn {key, _mod} ->
        Lkn.Core.Puppeteer.leave_instance(key, state.instance_key)
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

              %State{state|mode: {:zombie, Option.nothing()}}
            end
          Option.nothing() ->
            # there is no specified delay, so we can die right now
            Pool.kill_request(state.map_key, state.instance_key)

            %State{state|mode: {:zombie, Option.nothing()}}
        end
      else
        state
      end
    end

    @spec closed?(t) :: boolean
    def closed?(state) do
      state.locked || case Entity.read(state.map_key, :limit) do
                        Option.some(limit) -> Map.size(state.puppeteers) == limit
                        Option.nothing() -> false
                      end
    end

    @spec register_puppeteer(t, Puppeteer.k, Puppeteer.m) :: t
    def register_puppeteer(state, puppeteer_key, puppeteer_module) do
      state =  %State{state|puppeteers: Map.put(state.puppeteers, puppeteer_key, puppeteer_module)}
      case state.mode do
        {:zombie, Option.some(timer)} ->
            Beacon.cancel(timer)
            %State{state|mode: :running}
        _ -> state
      end
    end

    @spec unregister_puppeteer(t, Puppeteer.k) :: t
    def unregister_puppeteer(state, puppeteer_key) do
      %State{state|puppeteers: Map.delete(state.puppeteers, puppeteer_key)}
      |> zombify
    end
  end

  use GenServer

  @doc false
  @spec start_link(L.Map.k, k) :: GenServer.on_start
  def start_link(map_key, instance_key) do
    GenServer.start_link(__MODULE__, [map: map_key, instance: instance_key], name: Name.instance(instance_key))
  end

  @doc false
  @spec spawn_instance(L.Map.k, k) :: Supervisor.on_start
  def spawn_instance(map_key, instance_key) do
    Supervisor.start_child(Name.instance_sup(map_key), [instance_key])
  end

  def init(map: map_key, instance: instance_key) do
    sys_map = Entity.systems(map_key)

    _ = Enum.map(sys_map, fn sys ->
      {:ok, _} = System.start_link(sys, instance_key, map_key)
    end)

    {:ok, State.new(map_key, instance_key)}
  end

  @doc """
  Insert a new Puppet into an Instance.

  This function can be used by a `Lkn.Core.Puppeteer` to insert a new Puppet
  into an Instance. There is no notion of Puppet owner, from an Instance point
  of view. Therefore, any Puppeteer can send a command “on behalf of a given
  Puppet. This Puppeteer will have the obligation to unregister it.

  Under the hood, this function dispatches the register event to each system the
  Puppet has a Component for.
  """
  @spec register_puppet(k, Puppet.k, %{Lkn.Core.System.m => Keyworld.t}) :: boolean
  def register_puppet(instance_key, puppet_key, opts \\ %{}) do
    GenServer.call(Lkn.Core.Name.instance(instance_key), {:register_puppet, puppet_key, opts})
  end

  @doc """
  Remove a Puppet from an Instance.

  This function can be used by a `Lkn.Core.Puppeteer` to an Instance. An
  Instance will never do it by itself, so it needs to be done by the Puppeteer
  owner, e.g. before it registers istelf. Note that, if the Puppeteer forgets to
  unregister one of its Puppets, this Puppets will stay in this Instance as long
  as at least one Puppeteer stays registered (and it will probably do nothing at
  all).
  """
  @spec unregister_puppet(k, Puppet.k) :: boolean
  def unregister_puppet(instance_key, puppet_key) do
    GenServer.call(Lkn.Core.Name.instance(instance_key), {:unregister_puppet, puppet_key})
  end

  @doc false
  @spec register_puppeteer(k, Puppeteer.k, Puppeteer.m) :: boolean
  def register_puppeteer(instance_key, puppeteer_key, puppeteer_module) do
    GenServer.call(Name.instance(instance_key), {:register_puppeteer, puppeteer_key, puppeteer_module})
  end

  @doc false
  @spec lock(k) :: :ok
  def lock(instance_key) do
    GenServer.call(Name.instance(instance_key), :lock)
    :ok
  end

  @doc false
  @spec kick_all(k) :: :ok
  def kick_all(instance_key) do
    GenServer.cast(Name.instance(instance_key), :kick_all)
  end

  @doc false
  @spec kill(k) :: boolean
  def kill(instance_key) do
    GenServer.call(Name.instance(instance_key), :killme)
  end

  @doc """
  Notify an Instance that a given Puppeteer is leaving.

  This function needs to be used by a Puppeteer, __after__ it has removed all of
  its Puppets. Right now, a Puppeteer cannot choose its Instance to join, and
  the `Lkn.Core.Pool.register_puppeteer` is the function to use to join the
  Instance of a given Map.
  """
  @spec unregister_puppeteer(k, Puppeteer.k) :: :ok
  def unregister_puppeteer(instance_key, puppeteer_key) do
    GenServer.cast(Name.instance(instance_key), {:unregister_puppeteer, puppeteer_key})

    Registry.unregister(Lkn.Core.Notifier, Name.notify_group(instance_key))
  end

  def handle_call(:lock, _from, state) do
    {:reply, :ok, State.lock(state)}
  end
  def handle_call({:register_puppeteer, puppeteer_key, puppeteer_module}, _from, state) do
    if !State.closed?(state) do
      # we remember this new puppeteer
      s2 = State.register_puppeteer(state, puppeteer_key, puppeteer_module)

      # we compute a digest of the map and each puppets
      map = Lkn.Core.Entity.digest(state.map_key)
      puppets = Enum.reduce(state.puppets, Map.new(), &Map.put(&2, &1, Lkn.Core.Entity.digest(&1)))

      # we send these digest to our new friend the puppeteer
      Lkn.Core.Puppeteer.instance_digest(
        puppeteer_key,
        state.instance_key,
        state.map_key,
        map,
        puppets
      )

      {:reply, true, s2}
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
  def handle_call({:register_puppet, puppet_key, sys_opts}, _from, state) do
    # add the puppet to our list
    if MapSet.member?(state.puppets, puppet_key) do
      {:reply, false, state}
    else
      state = State.add_puppet(state, puppet_key)

      # get a digest of the puppet and send it to the puppeteer
      notify_puppeteers(state.instance_key, &Lkn.Core.Puppeteer.puppet_enter(
            &1,
            state.instance_key,
            puppet_key,
            Lkn.Core.Entity.digest(puppet_key)
          )
      )

      # try registering the puppet to each system
      sys_map = Entity.systems(puppet_key)

      _ = Enum.map(sys_map, fn sys ->
        try do
          Lkn.Core.System.register_puppet(
            state.instance_key,
            sys,
            puppet_key,
            Map.get(sys_opts, sys, [])
          )
        rescue
          _ -> nil
        end
      end)

      {:reply, true, state}
    end
  end
  def handle_call({:unregister_puppet, puppet_key}, _from, state) do
    state = if MapSet.member?(state.puppets, puppet_key) do
      # unregister the pupet from each systems
      sys_map = Entity.systems(puppet_key)

      _ = Enum.map(sys_map, fn sys ->
        try do
          Lkn.Core.System.unregister_puppet(state.instance_key, sys, puppet_key)
        rescue
          _ -> nil
        end
      end)

      # notify the puppeteers
      notify_puppeteers(state.instance_key, &Lkn.Core.Puppeteer.puppet_leave(
            &1,
            state.instance_key,
            puppet_key
          )
      )

      State.delete_puppet(state, puppet_key)
    else
      state
    end

    {:reply, true, state}
  end
  def handle_cast(:kick_all, state) do
    State.kick_all(state)
    {:noreply, state}
  end
  def handle_cast({:unregister_puppeteer, puppeteer_key}, state) do
    {:noreply, State.unregister_puppeteer(state, puppeteer_key)}
  end

  def notify_puppeteers(instance_key, notif) do
    Registry.dispatch(Lkn.Core.Notifier, Name.notify_group(instance_key), fn entries ->
      for {_, key} <- entries do
        notif.(key)
      end
    end)
  end
end

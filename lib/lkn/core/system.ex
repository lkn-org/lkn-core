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
defmodule Lkn.Core.System do
  @moduledoc """
  A behaviour module for implementing and querying a System.

  **Note:** You are not supposed to spawn a System yourself. The
    `Lkn.Core.Instance` takes care of that for you. This is why you
    will not find any `start*` function here.

  The core of this module is the `defsystem/2` macro. It is the
  recommended way to implement of System, that is a Process that
  handles one aspect of the gameplay. Before we going any further,
  lets have a look at a following minimal example.

      defsystem Sys do
        # 1. Specify which 'Component's are required by this 'System'.
        @map Sys.MapSpec
        @puppet Sys.PuppetSpec

        # 2. Implementation of the 'System' callbacks.
        def init_state(_instance_key, _map_key) do
          # return some state
        end

        def puppet_enter(state, _instance_key, _map_key, _puppets, _puppet_key)
          # we do nothing, but we could
          state
        end

        def puppet_leave(state, _instance_key, _map_key, _puppets, _puppet_key)
          # we do nothing, but we could
          state
        end

        # 3. Define some specific functions
        cast func1(a :: integer, b :: boolean) do
          # here you can use 'a' and 'b', but also 'state',
          # 'puppets', 'map_key' and 'instance_key'

          # you have to return the new state value
          state
        end
      end

  ## Map and Puppet Components

  A System handles one facet of the gameplay. Its purpose is to
  manipulate the set of “compatible” of Puppets. A System is tied to
  two Component Specification Modules (that is two modules defined
  with the `Lkn.Core.Component.defcomponent/2` macro). A Component is
  a Proxy to manipulate an Entity. It abstracts away its underlying
  structure and expose a common interface. See `Lkn.Core.Component` to
  learn how to specify then implement a Component.

  To define which Component is required for a Map and which one is
  required for a Puppet, we use the `@map` and `@puppet` annotations.

  ## The System Behaviour

  The System callbacks can be divided into two categories. The
  `init_map/2` function is called only one time to initialize the
  inner state of the System. This state implementation is at the
  discretion of the developer. The other callbacks are a set of hooks
  which are called when a particular event occures.

  ## The `cast` Keyword

  In addition to the System callbacks which are shared by all the
  Systems, each System exposes a public interface the Puppeteer can
  use to actually play. The function interface are defined using the
  `cast` keyword.

  You can use the `cast` keyword as follows: `cast
  <func_name>([arg_name :: arg_type]*) do <block> end`. Inside the
  block, you can use the arguments you have defined. In addition, you
  also can use the following variables:

  - `state` is the current state of the System, as initialized by
    `init_state/2` and modified by the System callbacks and functions
  - `instance_key` is the key to identify the Instance which spawns
    the system, it can be used to reach its other systems
  - `map_key` is the key to identify the map on which the puppets have
    been spawned
  - `puppets` is a `MapSet` of all the Puppets currently in the
    Instance and “compatible” with the System

  In addition to this variables, you also can use the `notify/1`
  function. It takes a lambda of types `(Lkn.Core.Puppeteer.k ->
  any)`. It can be used to reach all the Puppeteers that have
  populated the Instance with their Puppets.

  The block has to return the new system state.

  The `defsystem` will generate a client function to use this
  function. In addition to the specified arguments, it takes an
  additional one: an Instance key.
  """

  use Lkn.Prelude

  alias Lkn.Core.Map
  alias Lkn.Core.Name
  alias Lkn.Core.Puppet
  alias Lkn.Core.System
  alias Lkn.Core.Instance

  @typedoc "A System defined using the `defsystem/2` macro."
  @type m :: module

  @typedoc "A set of “compatible” puppets."
  @type puppets :: MapSet.t(Puppet.t)

  defmodule State do
    @moduledoc false

    defstruct [
      :map_key,
      :instance_key,
      :puppets,
    ]

    @type t :: %State{
      map_key: Map.k,
      instance_key: Instance.k,
      puppets: System.puppets,
    }

    @spec new(Instance.k, Map.k) :: t
    def new(instance_key, map_key) do
      %State{
        puppets: MapSet.new(),
        instance_key: instance_key,
        map_key: map_key,
      }
    end

    @spec put(t, Puppet.k) :: t
    def put(state, puppet_key) do
      puppets = MapSet.put(state.puppets, puppet_key)

      %State{state|
             puppets: puppets
      }
    end

    @spec delete(t, Puppet.k) :: t
    def delete(state, puppet_key) do
      %State{state|
             puppets: MapSet.delete(state.puppets, puppet_key)
      }
    end

    @spec population(t) :: integer
    def population(state) do
      MapSet.size(state.puppets)
    end

    @spec notify(t, any) :: :ok
    def notify(state, notif) do
      Lkn.Core.Instance.notify_puppeteers(state.instance_key, notif)
    end
  end

  @typedoc """
  The inner state of the Server process.
  """
  @type state :: any

  @doc """
  Initialize the state of the system.

  With the `instance_key`, you can trigger other systems of the same
  instance. With the `map_key`, you can request through the Map
  Component some information about the map.
  """
  @callback init_state(instance_key :: Instance.k, map_key :: Map.k) :: state

  @doc """
  A hook function which is called when a “compatible” puppet enters the Instance.
  """
  @callback puppet_enter(
    state :: state,
    instance_key :: Instance.k,
    map_key :: Map.k,
    puppets :: System.puppets,
    puppet_key :: Puppet.k) :: state

  @doc """
  A hook function which is called when a “compatible” puppet leaves the Instance.
  """
  @callback puppet_leave(
    state :: state,
    instance_key :: Instance.k,
    map_key :: Map.k,
    puppets :: System.puppets,
    puppet_key :: Puppet.k) :: state

  @doc """
  A macro to ease the definition of a new System which provides the
  [cast](#module-the-keyword) keyword.
  """
  defmacro defsystem(name, do: block) do
    alias Lkn.Core.Specs

    state_type = quote do Lkn.Core.System.state end
    key_type = quote do Lkn.Core.Instance.k end
    key_to_name = quote do
      &(Lkn.Core.Name.system(&1, unquote(name)))
    end

    quote do
      defmodule unquote(name) do
        use Lkn.Prelude
        use GenServer

        @behaviour Lkn.Core.System

        unquote(Specs.gen_server_from_specs(
              block,
              key_type,
              key_to_name,
              state_type,
              key_name: Specs.var_name("instance_key"),
              impl_suffix: "_impl",
              allow_impl: true,
              allow_specs: false,
              additional_args: [
                quote do unquote(Specs.var_name("map_key")) :: Lkn.Core.Map.k end,
                quote do unquote(Specs.var_name("puppets")) :: Lkn.Core.System.puppets end,
              ],
            ))

        @doc false
        @spec component(:puppet|:map) :: module
        def component(:puppet) do
          @puppet
        end
        def component(:map) do
          @map
        end

        def init(st) do
          {:ok, st}
        end

        defp priv_handle_cast({:notify, notification}, priv: priv, pub: pub) do
          State.notify(priv, notification)
          [priv: priv, pub: pub]
        end

        defp priv_handle_call({:register_puppet, entity_key}, _from, priv: priv, pub: pub) do
          {res, priv, pub} = if Lkn.Core.Entity.has_component?(entity_key, __MODULE__) do
            {true, State.put(priv, entity_key), puppet_enter(pub, priv.instance_key, priv.map_key, priv.puppets, entity_key)}
          else
            {false, priv, pub}
          end

          {:reply, res, [priv: priv, pub: pub]}
        end
        defp priv_handle_call({:unregister_puppet, entity_key}, _from, priv: priv, pub: pub) do
          {res, priv, pub} =
          if Lkn.Core.Entity.has_component?(entity_key, __MODULE__) do
            priv = State.delete(priv, entity_key)
            {true, priv, puppet_leave(pub, priv.instance_key, priv.map_key, priv.puppets, entity_key)}
          else
            {false, priv, pub}
          end

          {:reply, res, [priv: priv, pub: pub]}
        end
        defp priv_handle_call(:population_size, _from, priv: priv, pub: pub) do
          {:reply, State.population(priv), [priv: priv, pub: pub]}
        end

        def handle_cast({:priv, cmd}, state) do
          {:noreply, priv_handle_cast(cmd, state)}
        end
        def handle_cast({:spec, {name, args}}, priv: priv, pub: pub) do
          name = String.to_atom(String.replace_suffix(Atom.to_string(name), "", "_impl"))
          s = :erlang.apply(__MODULE__, name, [priv.instance_key|args] ++ [priv.map_key, priv.puppets, pub])

          {:noreply, [priv: priv, pub: s]}
        end

        def handle_call({:spec, {name, args}}, _call, priv: priv, pub: pub) do
          name = String.to_atom(String.replace_suffix(Atom.to_string(name), "", "_impl"))
          {res, s} = :erlang.apply(__MODULE__, name, [priv.instance_key|args] ++ [priv.map_key, priv.puppets, pub])

          {:reply, res, [priv: priv, pub: s]}
        end
        def handle_call({:priv, cmd}, from, priv: priv, pub: pub) do
          priv_handle_call(cmd, from, priv: priv, pub: pub)
        end

        @spec notify(any) :: :ok
        defp notify(notification) do
          GenServer.cast(self(), {:priv, {:notify, notification}})
        end
      end
    end
  end

  @doc false
  @spec start_link(m, Instance.k, Map.k) :: GenServer.on_start
  def start_link(module, instance_key, map_key) do
    GenServer.start_link(module,
      [
        priv: State.new(instance_key, map_key),
        pub: module.init_state(instance_key, map_key),
      ],
      name: Name.system(instance_key, module))
  end

  @doc false
  @spec register_puppet(Instance.k, m, Puppet.k) :: boolean
  def register_puppet(instance_key, system, puppet_key) do
    GenServer.call(Name.system(instance_key, system), {:priv, {:register_puppet, puppet_key}})
  end

  @doc false
  @spec unregister_puppet(Instance.k, m, Puppet.k) :: boolean
  def unregister_puppet(instance_key, system, puppet_key) do
    GenServer.call(Name.system(instance_key, system), {:priv, {:unregister_puppet, puppet_key}})
  end

  @doc """
  Returns the number of “compatible” Puppets of a given System
  registered to a given Instance.
  """
  @spec population_size(Instance.k, m) :: integer
  def population_size(instance_key, system) do
    GenServer.call(Name.system(instance_key, system), {:priv, :population_size})
  end
end

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
  A behaviour module for implementing a System.
  """

  use Lkn.Prelude

  alias Lkn.Core.Map
  alias Lkn.Core.Name
  alias Lkn.Core.Puppet
  alias Lkn.Core.System
  alias Lkn.Core.Instance

  @type m :: module
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
      Registry.dispatch(Lkn.Core.Notifier, Name.notify_group(state.instance_key), fn entries ->
        for {_, key} <- entries do
          notif.(key)
        end
      end)
    end
  end

  @type state :: any
  @callback init_state(Map.k) :: state
  @callback puppet_enter(state, System.puppets, Puppet.k) :: state
  @callback puppet_leave(state, System.puppets, Puppet.k) :: state

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
        defp priv_handle_cast({:register_puppet, entity_key}, priv: priv, pub: pub) do
          {priv, pub} = if Lkn.Core.Entity.has_component?(entity_key, __MODULE__) do
            {State.put(priv, entity_key), puppet_enter(pub, priv.puppets, entity_key)}
          else
            {priv, pub}
          end

          [priv: priv, pub: pub]
        end
        defp priv_handle_cast({:unregister_puppet, entity_key}, priv: priv, pub: pub) do
          {priv, pub} =
          if Lkn.Core.Entity.has_component?(entity_key, __MODULE__) do
            priv = State.delete(priv, entity_key)
            {priv, puppet_leave(pub, priv.puppets, entity_key)}
          else
            {priv, pub}
          end

          [priv: priv, pub: pub]
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

  @spec start_link(System.m, Instance.k, Map.k) :: GenServer.on_start
  def start_link(module, instance_key, map_key) do
    GenServer.start_link(module,
      [
        priv: State.new(instance_key, map_key),
        pub: module.init_state(map_key),
      ],
      name: Name.system(instance_key, module))
  end

  @doc false
  @spec cast(Instance.k, System.m, term) :: :ok
  def cast(instance_key, system, cmd) do
    GenServer.cast(Name.system(instance_key, system), {:pub, cmd})
  end

  @doc false
  @spec call(Instance.k, System.m, term) :: any
  def call(instance_key, system, cmd) do
    GenServer.call(Name.system(instance_key, system), {:pub, cmd})
  end

  @spec register_puppet(Instance.k, System.m, Puppet.k) :: :ok
  def register_puppet(instance_key, system, puppet_key) do
    GenServer.cast(Name.system(instance_key, system), {:priv, {:register_puppet, puppet_key}})
  end

  @spec unregister_puppet(Instance.k, System.m, Puppet.k) :: :ok
  def unregister_puppet(instance_key, system, puppet_key) do
    GenServer.cast(Name.system(instance_key, system), {:priv, {:unregister_puppet, puppet_key}})
  end

  @spec population_size(Instance.k, module) :: integer
  def population_size(instance_key, system) do
    GenServer.call(Name.system(instance_key, system), {:priv, :population_size})
  end
end

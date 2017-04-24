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

  alias Lkn.Core.Entity
  alias Lkn.Core.Name
  alias Lkn.Core.System

  @type m :: module
  @type entities :: %{Entity.t => module}

  defmodule State do
    @moduledoc false

    defstruct [
      :map_key,
      :map_comp,
      :instance_key,
      :entities,
    ]

    @type t :: %State{
      map_key: Entity.t,
      map_comp: module,
      instance_key: Instance.k,
      entities: System.entities,
    }

    @spec new(Instance.k, term, module) :: t
    def new(instance_key, map_key, map_comp) do
      %State{
        entities: Map.new(),
        instance_key: instance_key,
        map_comp: map_comp,
        map_key: map_key,
      }
    end

    @spec put(t, Entity.t, System.m) :: t
    def put(state, entity_key, module) do
      entities = Map.put(state.entities, entity_key, module)

      %State{state|
             entities: entities
      }
    end

    @spec delete(t, Entity.t) :: t
    def delete(state, entity_key) do
      %State{state|
             entities: Map.delete(state.entities, entity_key)
      }
    end

    @spec population(t) :: integer
    def population(state) do
      Map.size(state.entities)
    end

    @spec notify(t, any) :: :ok
    def notify(state, notif) do
      Registry.dispatch(Lkn.Core.Notifier, Name.notify_group(state.instance_key), fn entries ->
        for {pid, _} <- entries do
          send pid, notif
        end
      end)
    end
  end

  defmacro __using__([state: state_t, puppet_component: ec, map_component: em]) do
    quote location: :keep do
      require Logger

      use Lkn.Prelude

      use GenServer
      @type state :: unquote(state_t)

      @callback init_state(Entity.t, module) :: state

      def component(:puppet) do
        unquote(ec)
      end
      def component(:map) do
        unquote(em)
      end

      @spec entity_enter(state, System.entities, Entity.t) :: state
      @doc """
      A callback called when a new entity is added to the system.

      **Note:** it can be overriden.
      """
      def entity_enter(state, entities, entity_key) do
        state
      end

      @spec entity_leave(state, System.entities, Entity.t) :: state
      @doc """
      A callback called when an entity is removed from the system.

      **Note:** it can be overriden.
      """
      def entity_leave(state, entities, key) do
        state
      end

      @spec system_cast(any, Lkn.Core.System.entities, state) :: state
      def system_cast(cmd, entities, state) do
        state
      end

      @spec system_call(any, term, Lkn.Core.System.entities, state) :: term
      def system_call(cmd, _from, entities, state) do
        {:ok, state}
      end

      def comp(entities, key) do
        Map.fetch!(entities, key)
      end

      defp priv_handle_cast(notif = {:notify, notification}, priv: priv, pub: pub) do
        State.notify(priv, notif)
        [priv: priv, pub: pub]
      end

      defp priv_handle_cast({:register_entity, entity_key}, priv: priv, pub: pub) do
        {priv, pub} =
          case Lkn.Core.Entity.get_component(entity_key, __MODULE__) do
            Option.some(module) ->
              {State.put(priv, entity_key, module), entity_enter(pub, priv.entities, entity_key)}
            _ ->
              {priv, pub}
          end

        [priv: priv, pub: pub]
      end

      defp priv_handle_cast({:unregister_entity, entity_key}, priv: priv, pub: pub) do
        {priv, pub} =
          if Lkn.Core.Entity.has_component?(entity_key, __MODULE__) do
            priv = State.delete(priv, entity_key)
            {priv, entity_leave(pub, priv.entities, entity_key)}
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

      def handle_cast({:pub, cmd}, priv: priv, pub: pub) do
        pub = system_cast(cmd, priv.entities, pub)
        {:noreply, [priv: priv, pub: pub]}
      end

      def handle_call({:priv, cmd}, from, priv: priv, pub: pub) do
        priv_handle_call(cmd, from, priv: priv, pub: pub)
      end

      def handle_call({:pub, cmd}, from, priv: priv, pub: pub) do
        {res, pub} = system_call(cmd, from, priv.entities, pub)
        {:reply, res, [priv: priv, pub: pub]}
      end

      def cast(instance_key, cmd) do
        Lkn.Core.System.cast(instance_key, __MODULE__, cmd)
      end

      def call(instance_key, system, cmd) do
        Lkn.Core.System.call(instance_key, __MODULE__, cmd)
      end

      defoverridable [
        system_cast: 3,
        system_call: 4,
        entity_enter: 3,
        entity_leave: 3,
      ]
    end
  end

  def start_link(module, instance_key, map_key, map_comp) do
    GenServer.start_link(module,
      [
        priv: State.new(instance_key, map_key, map_comp),
        pub: module.init_state(map_key, map_comp),
      ],
      name: Name.system(instance_key, module))
  end

  def cast(instance_key, system, cmd) do
    GenServer.cast(Name.system(instance_key, system), {:pub, cmd})
  end

  def call(instance_key, system, cmd) do
    GenServer.call(Name.system(instance_key, system), {:pub, cmd})
  end

  def register_entity(instance_key, system, entity_key) do
    GenServer.cast(Name.system(instance_key, system), {:priv, {:register_entity, entity_key}})
  end

  def unregister_entity(instance_key, system, entity_key) do
    GenServer.cast(Name.system(instance_key, system), {:priv, {:unregister_entity, entity_key}})
  end

  def notify(notification) do
    GenServer.cast(self(), {:priv, {:notify, notification}})
  end

  @spec population_size(Instance.k, module) :: integer
  def population_size(instance_key, system) do
    GenServer.call(Name.system(instance_key, system), {:priv, :population_size})
  end
end

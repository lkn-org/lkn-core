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
defmodule Lkn.Core.Component do
  alias Lkn.Core.Specs

  @type state :: any

  @callback init_state(Lkn.Core.Entity.k) :: {:ok, state} | :error

  defmacro defcomponent(name, do: block) do
    state_type = quote do Lkn.Core.Component.state end
    key_type = quote do Lkn.Core.Entity.k end
    key_to_name = quote do
      &(Lkn.Core.Name.component(&1, unquote(name)))
    end

    quote do
      defmodule unquote(name) do
        unquote(Specs.gen_server_from_specs(
              block,
              key_type,
              key_to_name,
              state_type,
              key_name: Specs.var_name("entity_key"),
            ))

        @doc false
        def system do
          @system
        end

        defmacro __using__(_) do
          quote do
            defmodule State do
              defstruct [
                :entity_key,
                :state,
              ]

              @type t :: %State{
                entity_key: Lkn.Core.Entity.k,
                state: Lkn.Core.Component.state,
              }
            end

            @behaviour unquote(__MODULE__)
            @behaviour Lkn.Core.Component

            use GenServer

            alias Lkn.Core.Entity
            alias Lkn.Core.Name
            alias Lkn.Core.Properties

            def specs do
              unquote(__MODULE__)
            end

            @spec start_link(Entity.k) :: GenServer.on_start
            def start_link(entity_key) do
              GenServer.start_link(__MODULE__, entity_key, name: Name.component(entity_key, unquote(__MODULE__)))
            end

            def init(entity_key) do
              {:ok, s} = init_state(entity_key)
              {:ok, %State{entity_key: entity_key, state: s}}
            end

            @spec read(Entity.k, Properties.prop) :: Option.t(Properties.value)
            defp read(entity_key, p) do
              Properties.read(entity_key, p)
            end

            @spec write(Entity.k, Properties.prop, Properties.value) :: :ok
            def write(entity_key, p, v) do
              Properties.write(entity_key, p, v)
            end

            def handle_cast({:spec, {name, args}}, state) do
              s = :erlang.apply(__MODULE__, name, [state.entity_key|args] ++ [state.state])

              {:noreply, %State{state|state: s}}
            end

            def handle_call({:spec, {name, args}}, _call, state) do
              {res, s} = :erlang.apply(__MODULE__, name, [state.entity_key|args] ++ [state.state])

              {:reply, res, %State{state|state: s}}
            end
          end
        end
      end
    end
  end
end
